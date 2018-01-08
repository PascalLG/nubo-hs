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

module CmdIgnore (
      cmdIgnore
    , helpIgnore
) where

import Control.Monad.Trans (liftIO)
import Control.Monad.State (get)
import Data.Char (isDigit)
import Data.Tuple (swap)
import PrettyPrint
import Environment
import Error
import Database
import Matching

-----------------------------------------------------------------------------

-- | Parse arguments for the 'ignore' command.
--
cmdIgnore :: [String] -> EnvIO ExitStatus
cmdIgnore args = do
    result <- parseArgsM args [OptionDelete]
    case result of
        Left err -> putErr (ErrUnsupportedOption err) >> return StatusInvalidCommand
        Right (opts, xs)
            | elem OptionDelete opts -> openDBAndRun $ deletePatterns xs
            | null xs                -> openDBAndRun $ printPatterns
            | otherwise              -> openDBAndRun $ addPatterns xs

-- | Print the list of file patterns to ignore. The list is sorted
-- by pattern and each entry is assigned a rank that can be used
-- later to delete that entry.
--
printPatterns :: EnvIO ExitStatus
printPatterns = do
    patterns <- getPatternList
    cm <- consoleMode <$> get
    liftIO $ mapM_ (printpat cm) (zip [1..] patterns)
    return StatusOK
    where
        printpat :: ConsoleMode -> (Int, (Int, Pattern)) -> IO ()
        printpat cm (rank, (_, pattern)) = do
            let text = (show rank) ++ ": "
            putStrLn ((replicate (8 - length text) ' ') ++ (foreColor cm AnsiYellow text) ++ (show pattern))

-- | Add patterns to the list of files to ignore. If a pattern already
-- exists, it is ignored.
--
addPatterns :: [String] -> EnvIO ExitStatus
addPatterns patterns = exec addpat patterns
    where
        addpat :: String -> EnvIO Bool
        addpat ps = do
            case parsePattern ps of
                Left err -> putErr err >> return False
                Right p  -> do
                    (_, list) <- unzip <$> getPatternList
                    ope <- if p `elem` list then return "ignoring: "
                                            else addPattern p >> return "adding: "
                    liftIO $ putStrLn (ope ++ (show p))
                    return True

-- | Delete patterns. Patterns can be either specified by their rank,
-- in which case that rank must be prefixed by a # sign, or either by
-- their full name.
--
deletePatterns :: [String] -> EnvIO ExitStatus
deletePatterns params = getPatternList >>= \list -> exec (delpat list) params
    where 
        delpat :: [(Int, Pattern)] -> String -> EnvIO Bool
        delpat list param = case parse param of
                                    Nothing -> do
                                        putErr (ErrInvalidPatternRef param)
                                        return False
                                    Just uid -> do
                                        deletePattern uid
                                        let (Just pattern) = lookup uid list
                                        liftIO $ putStrLn ("deleting: " ++ show pattern)
                                        return True
            where
                parse :: String -> Maybe Int
                parse ps
                    | head ps == '#' && all isDigit (tail ps) = let rank = read (tail ps) - 1
                                                                in if rank >= 0 && rank < length list then Just $ fst (list !! rank)
                                                                                                      else Nothing
                    | otherwise = case parsePattern ps of
                                        Left _ -> Nothing
                                        Right p -> lookup p (map swap list)

-- | Helper function that executes several action and returns an
-- error status if one or more of these actions fail.
--
exec :: (Monad m) => (a -> m Bool) -> [a] -> m ExitStatus
exec actions list = do
    oks <- mapM actions list
    return $ if (and oks) then StatusOK else StatusInvalidPattern

-----------------------------------------------------------------------------

-- | Print usage for the ignore command.
--
helpIgnore :: EnvIO ()
helpIgnore = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo ignore}}"
    putLine $ "    {y:nubo ignore}} <{y:patterns...}}>"
    putLine $ "    {y:nubo ignore}} {{y:-d}} | {y:--delete}}} <{y:ranks...}}>"
    putLine $ "    {y:nubo ignore}} {{y:-d}} | {y:--delete}}} <{y:patterns...}}>"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Manage the list of files to ignore. As the name suggests, ignored files are"
    putLine $ "    not taken into account during synchronisation. Local changes to files that"
    putLine $ "    match one or more patterns of this list are never sent to the cloud and"
    putLine $ "    conversely, remote changes are not reflected locally. You typically use"
    putLine $ "    this feature if you synchronise your cloud on a computer that is shared"
    putLine $ "    among several users and you do not want some private files to appear on that"
    putLine $ "    computer."
    putLine $ ""
    putLine $ "    A file pattern can contain wildcards. {y:*}} matches any number of any character,"
    putLine $ "    including none. {y:?}} matches any single character. Those wildcards can never"
    putLine $ "    match a path separator though. For example, {m:*.txt}} matches any file whose"
    putLine $ "    extension is {m:.txt}}, {m:foo?.bar}} matches {m:foo1.bar}} and {m:foo2.bar}} but not {m:foo10.bar}}."
    putLine $ "    If a pattern ends by a path separator, it can only match a folder, not a"
    putLine $ "    file."
    putLine $ ""
    putLine $ "    If a pattern does not specify a path, then it can match any file in any"
    putLine $ "    folder. On the other hand, if a pattern does specify a path, that path is"
    putLine $ "    always relative to the root of the cloud. For example, assuming the current"
    putLine $ "    folder is the root of a cloud:"
    putLine $ ""
    putLine $ "        {m:foo.bar}}        all files named {m:foo.bar}} whatever the folder they are in"
    putLine $ "        {m:foo/}}           all folders named {m:foo}} whatever the folder they are in"
    putLine $ "        {m:foo/bar}}        the file or folder {m:./foo/bar}}"
    putLine $ "        {m:foo/bar/*.txt}}  all files with a {m:.txt}} extension in the {m:./foo/bar/}} folder"
    putLine $ ""
    putLine $ "    Without parameter, {y:nubo ignore}} simply prints the list of actual patterns,"
    putLine $ "    one per line. Each pattern is prefixed by a rank number you can use to refer"
    putLine $ "    to that pattern in subsequent commands."
    putLine $ ""
    putLine $ "    To add one or more patterns, specify these patterns on the command line."
    putLine $ "    Patterns should be enclosed between double quotes to avoid shell expansion"
    putLine $ "    on Unix-like systems. For example:"
    putLine $ ""
    putLine $ "        {y:nubo ignore \"*.txt\" \"foo/bar?.png\"}}"
    putLine $ ""
    putLine $ "    To delete one or more patterns, use the {y:--delete}} option. You can specify"
    putLine $ "    which entries to delete either by their rank as obtained by a previous {y:nubo}}"
    putLine $ "    {y:ignore}} command, prefixed by a # sign; or either by their full name. For"
    putLine $ "    example:"
    putLine $ ""
    putLine $ "        {y:nubo ignore --delete #2}}"
    putLine $ "        {y:nubo ignore --delete \"*.txt\"}}"
    putLine $ ""
    putLine $ "    Ranks are assigned sequentially. If you plan to run several {y:nubo ignore}}"
    putLine $ "    {y:--delete}} commands in a raw, keep in mind that each command may shift some"
    putLine $ "    pattern ranks. It may be safer in such a case to refer to a pattern by its"
    putLine $ "    full name."
    putLine $ ""
    putLine $ "    Note: even though they do not appear explicitly in this list, the following"
    putLine $ "    files and folders are always ignored:"
    putLine $ ""
    putLine $ "        {m:.git}}"
    putLine $ "        {m:.cabal-sandbox}}"
    putLine $ "        {m:.DS_Store}}"
    putLine $ "        {m:.stack-work}}"
    putLine $ "        {m:cabal.sandbox.config}}"
    putLine $ "        {m:desktop.ini}}"
    putLine $ "        {m:Icon\\r}}"
    putLine $ "        {m:thumbs.db}}"
    putLine $ ""
    putLine $ "    For performance and compatibility reasons, {y:nubo}} also ignores files larger"
    putLine $ "    than 300 Mb and files with names containing characters that are not"
    putLine $ "    supported on all platforms."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-d}}, {y:--delete}}  Delete the specified file patterns."
    putLine $ ""

-----------------------------------------------------------------------------
