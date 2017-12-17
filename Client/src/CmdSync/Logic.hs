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

module CmdSync.Logic (
      SyncPriority(..)
    , FileStatus(..)
    , ConflictType(..)
    , Action(..)
    , FileAction
    , computeChanges
    , computeActions
    , doesCreateDir
    , doesDeleteDir
) where

import qualified Data.ByteString as B
import FileHelper

-----------------------------------------------------------------------------

-- | Synchronisation priority in case of conflict.
--
data SyncPriority =
      SyncDefault
    | SyncOurs
    | SyncTheirs
    deriving (Show)

-- | Status of a file.
--
data FileStatus = 
      FileNotModified
    | FileModified
    | FileCreated
    | FileDeleted
    deriving (Show, Eq)

-- | Type of conflicts.
--
data ConflictType = 
      ModifiedOnBothSides
    | CreatedOnBothSides
    | LocallyModifiedRemotelyDeleted
    | LocallyDeletedRemotelyModified
    deriving (Eq)

instance Show ConflictType where
    show ModifiedOnBothSides            = "modified on both sides"
    show CreatedOnBothSides             = "created on both sides"
    show LocallyModifiedRemotelyDeleted = "locally modified, remotely deleted"
    show LocallyDeletedRemotelyModified = "locally deleted, remotely modified"
    
-- | Synchronisation action.
--
data Action =
      ActionSendFile B.ByteString
    | ActionSendDir
    | ActionGetFile B.ByteString
    | ActionGetDir
    | ActionDeleteLocalFile
    | ActionDeleteLocalDir
    | ActionDeleteRemote
    | ActionUpdateCache B.ByteString
    | ActionDeleteFromCache
    | ActionConflict ConflictType
    | ActionError -- Should never happen, but who knows.
    deriving (Show, Eq)

-- | Convenient tuple associating an action to a file.
--
type FileAction = (UFilePath, Action)

-- | Predicate function indicating whether a FileAction involves
-- creating a local directory.
--
doesCreateDir :: FileAction -> Bool
doesCreateDir (_, ActionGetDir) = True
doesCreateDir _                 = False

-- | Predicate function indicating whether a FileAction involves
-- deleting a local directory.
--
doesDeleteDir :: FileAction -> Bool
doesDeleteDir (_, ActionDeleteLocalDir) = True
doesDeleteDir _                         = False

-- | Take two lists of files with their hashes, and determine for each file if it
-- was created, deleted, modified or not touched. The first parameter must be
-- the "old" list and the second parameter must be the "new" list. BOTH LISTS
-- MUST BE SORTED.
--
computeChanges :: [(UFilePath, B.ByteString)] -> [(UFilePath, B.ByteString)] -> [(UFilePath, B.ByteString, FileStatus)]
computeChanges [] [] = []
computeChanges ((name, hash):xs) [] = (name, hash, FileDeleted) : computeChanges xs []
computeChanges [] ((name, hash):ys) = (name, hash, FileCreated) : computeChanges [] ys
computeChanges x@((xname, xhash):xs) y@((yname, yhash):ys)
    | xname < yname = (xname, xhash, FileDeleted) : computeChanges xs y
    | xname > yname = (yname, yhash, FileCreated) : computeChanges x ys
    | otherwise     = (xname, yhash, if xhash /= yhash then FileModified else FileNotModified) : computeChanges xs ys

-- | Take two lists of file changes, and determine which actions must be undertaken
-- to synchronise them. The first parameter must be the local file list and the
-- second parameter must be the remote file list. BOTH LISTS MUST BE SORTED.
--
computeActions :: SyncPriority -> [(UFilePath, B.ByteString, FileStatus)] -> [(UFilePath, B.ByteString, FileStatus)] -> [FileAction]
computeActions _ [] [] = []
computeActions priority ((xname, xhash, xstatus):xs) [] = xstatus /= FileDeleted ?: (xname, actionSend xhash) $ computeActions priority xs []
computeActions priority [] ((yname, yhash, ystatus):ys) = ystatus /= FileDeleted ?: (yname, actionGet yhash) $ computeActions priority [] ys
computeActions priority x@((xname, xhash, xstatus):xs) y@((yname, yhash, ystatus):ys)
    | xname < yname = xstatus /= FileDeleted ?: (xname, actionSend xhash) $ computeActions priority xs y
    | xname > yname = ystatus /= FileDeleted ?: (yname, actionGet yhash) $ computeActions priority x ys
    | otherwise     = case action priority xstatus ystatus of
                          Just a  -> (xname, a) : computeActions priority xs ys
                          Nothing -> computeActions priority xs ys
    where
        action :: SyncPriority -> FileStatus -> FileStatus -> Maybe Action
        action _            FileNotModified  FileNotModified = Nothing
        action _            FileModified     FileNotModified = Just $ actionSend xhash
        action _            FileDeleted      FileNotModified = Just $ ActionDeleteRemote
        action _            FileNotModified  FileModified    = Just $ actionGet yhash
        action SyncDefault  FileModified     FileModified    = Just $ if xhash /= yhash then ActionConflict ModifiedOnBothSides else ActionUpdateCache xhash
        action SyncOurs     FileModified     FileModified    = Just $ if xhash /= yhash then actionSend xhash else ActionUpdateCache xhash
        action SyncTheirs   FileModified     FileModified    = Just $ if xhash /= yhash then actionGet yhash else ActionUpdateCache xhash
        action SyncDefault  FileDeleted      FileModified    = Just $ ActionConflict LocallyDeletedRemotelyModified
        action SyncOurs     FileDeleted      FileModified    = Just $ ActionDeleteRemote
        action SyncTheirs   FileDeleted      FileModified    = Just $ actionGet yhash
        action _            FileNotModified  FileDeleted     = Just $ actionDeleteLocal xhash
        action SyncDefault  FileModified     FileDeleted     = Just $ ActionConflict LocallyModifiedRemotelyDeleted
        action SyncOurs     FileModified     FileDeleted     = Just $ actionSend xhash
        action SyncTheirs   FileModified     FileDeleted     = Just $ actionDeleteLocal xhash
        action _            FileDeleted      FileDeleted     = Just $ ActionDeleteFromCache
        action SyncDefault  FileCreated      FileCreated     = Just $ if xhash /= yhash then ActionConflict CreatedOnBothSides else ActionUpdateCache xhash
        action SyncOurs     FileCreated      FileCreated     = Just $ if xhash /= yhash then actionSend xhash else ActionUpdateCache xhash
        action SyncTheirs   FileCreated      FileCreated     = Just $ if xhash /= yhash then actionGet yhash else ActionUpdateCache xhash
        action _            _                _               = Just $ ActionError  -- should never happen but who knows

-- | Build the right Send action, depending on whether the item is
-- a file or a directory.
--
actionSend :: B.ByteString -> Action
actionSend hash 
    | B.null hash = ActionSendDir
    | otherwise   = ActionSendFile hash

-- | Build the right Get action, depending on whether the item is
-- a file or a directory.
--
actionGet :: B.ByteString -> Action
actionGet hash
    | B.null hash = ActionGetDir
    | otherwise   = ActionGetFile hash

-- | Build the right DeleteLocal action, depending on whether the item is
-- a file or a directory.
--
actionDeleteLocal :: B.ByteString -> Action
actionDeleteLocal hash
    | B.null hash = ActionDeleteLocalDir
    | otherwise   = ActionDeleteLocalFile

-- | Ternary operator that conditionally prepend an element to
-- a list.
--
infixl 1 ?:
(?:) :: Bool -> a -> [a] -> [a]
(?:) False _ xs = xs
(?:) True x xs = x : xs

-----------------------------------------------------------------------------
