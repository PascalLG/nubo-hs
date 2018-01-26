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

module Error.Internal (
      Error(..)
    , ExitStatus(..)
) where

-----------------------------------------------------------------------------

-- | Type to represent error conditions.
--
data Error =
      ErrUnsupportedOption String
    | ErrExtraArgument String
    | ErrSyncBothOursTheirs
    | ErrUnknownArchiveFormat String
    | ErrCorruptedArchive String
    | ErrDatabaseNotFound
    | ErrServerInvalidURL String
    | ErrServerConnectionFailure
    | ErrServerOtherException String
    | ErrServer400 Int
    | ErrServer500 Int
    | ErrMsgPackDecoding String
    | ErrCloudNotInitialized
    | ErrCloudAuthenticationFailed
    | ErrCloudGeneric String
    | ErrIOException String String
    | ErrInvalidMasterKey
    | ErrInvalidCharInFilename
    | ErrCaseConflictInFilename
    | ErrReservedFilename
    | ErrFileIsTooBig
    | ErrAlreadySyncFolder String
    | ErrInvalidPattern String
    | ErrInvalidPatternRef String
    | ErrMissingMasterKey
    | ErrApiLevelOldClient
    | ErrApiLevelOldServer
    deriving (Eq)
    
instance Show Error where
    show = renderError

-- | Type to represent exit status codes.
--
data ExitStatus =
      StatusOK                  -- Should be first so fromEnum StatusOK == 0
    | StatusInvalidCommand
    | StatusDatabaseNotFound
    | StatusDatabaseAlreadyExists
    | StatusAuthenticationFailed
    | StatusConnectionFailed
    | StatusInvalidPattern
    | StatusInvalidServerResponse
    | StatusSynchronizationFailed
    deriving (Show, Enum, Eq)

-- | Render an error as a string.
--
renderError :: Error -> String
renderError (ErrUnsupportedOption opt)      = "unsupported option: " ++ opt
renderError (ErrExtraArgument arg)          = "extra argument: " ++ arg
renderError (ErrSyncBothOursTheirs)         = "both --ours and --theirs specified"
renderError (ErrUnknownArchiveFormat path)  = "unknown archive format for file '" ++ path ++ "'"
renderError (ErrCorruptedArchive path)      = "corrupted archive for file '" ++ path ++ "'"
renderError (ErrDatabaseNotFound)           = "database not found, folder is probably not synchronised"
renderError (ErrServerInvalidURL url)       = "invalid URL: '" ++ url ++ "'"
renderError (ErrServerConnectionFailure)    = "connection failed"
renderError (ErrServerOtherException desc)  = "unexpected error (" ++ desc ++ ")"
renderError (ErrServer400 status)           = "page not found (HTTP " ++ show status ++ ")"
renderError (ErrServer500 status)           = "internal server error (HTTP " ++ show status ++ ")"
renderError (ErrMsgPackDecoding msg)        = "server response decoding failed (" ++ msg ++ ")"
renderError (ErrCloudNotInitialized)        = "cloud not initialised"
renderError (ErrCloudAuthenticationFailed)  = "authentication failed"
renderError (ErrCloudGeneric msg)           = "unexpected server error (" ++ msg ++ ")"
renderError (ErrIOException path msg)       = "I/O error while accessing file '" ++ path ++ "': " ++ msg
renderError (ErrInvalidMasterKey)           = "the server returned an invalid master key"
renderError (ErrInvalidCharInFilename)      = "invalid characters in filename"
renderError (ErrCaseConflictInFilename)     = "case conflict with another filename"
renderError (ErrReservedFilename)           = "reserved keyword"
renderError (ErrFileIsTooBig)               = "file is too big"
renderError (ErrAlreadySyncFolder path)     = "this folder is under '" ++ path ++ "' which is already synced"
renderError (ErrInvalidPattern pattern)     = "invalid pattern: '" ++ pattern ++ "'"
renderError (ErrInvalidPatternRef ref)      = "invalid pattern reference: '" ++ ref ++ "'"
renderError (ErrMissingMasterKey)           = "missing master key"
renderError (ErrApiLevelOldClient)          = "incompatible version, please update your client application"
renderError (ErrApiLevelOldServer)          = "incompatible version, please update your server application"

-----------------------------------------------------------------------------
