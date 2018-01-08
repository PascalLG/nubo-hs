-----------------------------------------------------------------------------
-- Nubo Client Application
-- Copyright (c) 2017, Pascal Levy
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-----------------------------------------------------------------------------

module WebService (
      ProgressBar(..)
    , callWebService
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Network.HTTP.Client as H
import Data.ByteString.Builder
import Control.Exception (bracket_, try)
import Control.Monad.State
import Network.HTTP.Client
import Network.HTTP.Types.Header (hContentLength)
import Text.Printf
import Data.IORef
import Environment
import PrettyPrint
import Error
import MsgPack
import Database

-----------------------------------------------------------------------------
-- Web services.

-- | Call the nubo API endpoint with a command, some MsgPack'ed parameters,
-- whether a progress bar should be displayed during uploading or downloading,
-- and return either an error or a MsgPack'ed response.
--
callWebService :: String -> MsgValue -> ProgressBar -> EnvIO (Either Error MsgValue)
callWebService cmd params bar = do
    auth <- getConfig CfgAuthToken
    let token = case auth of
                    Just a  -> [("auth", MsgString a)]
                    Nothing -> []

    Just url <- getConfig CfgRemoteURL
    env <- get
    let Just manager = tlsManager env
    let cm = consoleMode env

    result <- liftIO $ bracket_ (when (bar /= ProgressNone) (putStr $ showCursor cm False))
                                (when (bar /= ProgressNone) (putStr $ "\r" ++ eraseEOL cm ++ showCursor cm True))
                                (callAPI manager
                                         (url <//> "sync.php")
                                         (MsgObject ([("cmd", msgString cmd), 
                                                      ("params", params), 
                                                      ("api", MsgInteger apiLevel)]
                                                       ++ token))
                                         (if bar == ProgressSend && cm /= ModeBasic then progress cm else progress')
                                         (if bar == ProgressReceive && cm /= ModeBasic then progress cm else progress'))

    case result >>= decodeMsgPack of
        Left err  -> return $ Left err
        Right msg -> saveAuthToken msg >> handleErrors msg

    where
        saveAuthToken :: MsgValue -> EnvIO ()
        saveAuthToken msg = case msg !? "auth" of
            Just (MsgString a) -> saveConfig CfgAuthToken a
            _                  -> return ()

        handleErrors :: MsgValue -> EnvIO (Either Error MsgValue)
        handleErrors msg = case (msg !? "error", msg !? "message") of
            (Just (MsgInteger 1), _)                     -> return $ Left ErrCloudNotInitialized
            (Just (MsgInteger 5), _)                     -> return $ Left ErrCloudAuthenticationFailed
            (Just (MsgInteger 9), _)                     -> return $ Left ErrApiLevelOldClient
            (Just (MsgInteger 10), _)                    -> return $ Left ErrApiLevelOldServer
            (Just (MsgInteger _), Just (MsgString text)) -> return $ Left $ ErrCloudGeneric (T.unpack text)
            (Just (MsgInteger _), Nothing)               -> return $ Left $ ErrCloudGeneric "no detailed description"
            _                                            -> return $ Right msg

-- | Parse the URL, build the HTTP request, perform the actual
-- call displaying progress bars if necessary, then return either
-- an error or the server response.
--
callAPI :: H.Manager -> String -> MsgValue -> Observer -> Observer -> IO (Either Error L.ByteString)
callAPI manager url params obs_send obs_recv = do
    result <- try $ do
        initialreq <- parseRequest url
        let pack = encodeMsgPack params
        let body = RequestBodyStream (L.length pack) (produce pack)
        let req = initialreq { method = C.pack "POST", requestBody = body }

        withResponse req manager $ \response -> do
            let size = case lookup hContentLength (responseHeaders response) >>= C.readInteger of
                           Just (y, _) -> fromInteger y
                           _           -> 0
            (builder, _) <- runStateT (consume 0 (responseBody response)) (size, minBound)
            return response { responseBody = toLazyByteString builder }

    return $ handleExceptions result >>= handleHttpErrors

    where
        handleExceptions :: Either HttpException (Response L.ByteString) -> Either Error (Response L.ByteString)
        handleExceptions (Left (InvalidUrlException uri _))                    = Left $ ErrServerInvalidURL uri
        handleExceptions (Left (HttpExceptionRequest _ (ConnectionFailure _))) = Left $ ErrServerConnectionFailure
        handleExceptions (Left (HttpExceptionRequest _ content))               = Left $ ErrServerOtherException (show content)
        handleExceptions (Right response)                                      = Right response

        handleHttpErrors :: Response L.ByteString -> Either Error L.ByteString
        handleHttpErrors response = case status `div` 100 of
            2 -> Right $ responseBody response
            4 -> Left $ ErrServer400 status
            5 -> Left $ ErrServer500 status
            _ -> Left $ ErrServerOtherException ("unexpected HTTP status " ++ show status)
            where status = fromEnum (responseStatus response)

        produce :: L.ByteString -> (IO B.ByteString -> IO r) -> IO r
        produce lbs sink = do
            ref <- newIORef (lbs, (fromIntegral (L.length lbs), minBound))
            let fetch :: IO B.ByteString
                fetch = do
                    (bs, (size, pos1)) <- readIORef ref
                    (_, (_, pos2)) <- runStateT (obs_send (size - fromIntegral (L.length bs))) (size, pos1)
                    let (c, cs) = L.splitAt 65536 bs
                    writeIORef ref (cs, (size, pos2))
                    return $ L.toStrict c
            sink fetch

        consume :: Int -> BodyReader -> StateT (Int, Int) IO Builder
        consume rec brread = do
            _ <- obs_recv rec
            x <- lift $ brread
            if B.null x then return mempty
                        else do
                            y <- consume (rec + B.length x) brread
                            return $ mappend (byteString x) y

-- | Concatenate a path to a URL.
--
(<//>) :: String -> String -> String
(<//>) root filepath = (norm root) ++ filepath
    where norm u = if (last u) /= '/' then (u ++ "/") else u

-- | API Level. The server returns an error if its own level does not
-- match the client. This value must be incremented each time an evolution
-- breaks compatibility with already deployed servers.
--
apiLevel :: Int
apiLevel = 1

-----------------------------------------------------------------------------
-- Progress bar.

-- | Style for progress bars.
--
data ProgressBar = ProgressNone         -- no progress bar
                 | ProgressSend         -- progress bar when uploading data
                 | ProgressReceive      -- progress bar when downloading data
                 deriving (Eq)

-- | Type signature of a function observing
-- data transfer.
--
type Observer = Int -> StateT (Int, Int) IO ()

-- | Display a progress bar.
--
progress :: ConsoleMode -> Observer
progress cm current = do
    (size, oldvalue) <- get
    if size /= 0 then do
        -- Total number of bytes to transfer is known. Display a
        -- regular progress bar.
        let value = (current * 100 + size `div` 2) `div` size
        when (value /= oldvalue) $ do
            put (size, value)
            let pos = value `div` 2
            let text = show value
            liftIO $ putStr ("\r" ++
                            (replicate (4 - length text) ' ') ++ text ++ " % |" ++
                            (foreColor cm AnsiYellow (replicate pos '#')) ++ (replicate (50 - pos) '-') ++
                            "|  ")
    else do
        -- Total number of bytes to transfer is unknown. Display
        -- some bouncing animation.
        let value = current `div` 65536
        when (value /= oldvalue) $ do
            put (size, value)
            let pos = if x > 46 then 92 - x else x where x = value `mod` 92
            liftIO $ putStr ("\r" ++
                            (printf "%7.1f Mb |" (((fromIntegral current) / 1048576.0) :: Double)) ++
                            (replicate pos '-') ++ (foreColor cm AnsiYellow "####") ++ (replicate (46 - pos) '-') ++
                            "|  ")

-- | Do not display a progress bar. Function with the same
-- type signature as above so it can be used as direct replacement,
-- but doing nothing.
--
progress' :: Observer
progress' _ = return ()

-----------------------------------------------------------------------------
