-----------------------------------------------------------------------------
-- Nubo Client Application - Unit Testing
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

module CmdSyncSpec (
    spec
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Test.Hspec
import CmdSync.Logic

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "Actions" $ do
        let file = T.pack "path/to/some/file"
        let hash = B.pack [1..16]
        it "create directory" $ do
            doesCreateDir (file, ActionSendFile hash)               `shouldBe` False
            doesCreateDir (file, ActionSendDir)                     `shouldBe` False
            doesCreateDir (file, ActionGetFile hash)                `shouldBe` False
            doesCreateDir (file, ActionGetDir)                      `shouldBe` True
            doesCreateDir (file, ActionDeleteLocalFile)             `shouldBe` False
            doesCreateDir (file, ActionDeleteLocalDir)              `shouldBe` False
            doesCreateDir (file, ActionDeleteRemote)                `shouldBe` False
            doesCreateDir (file, ActionUpdateCache hash)            `shouldBe` False
            doesCreateDir (file, ActionDeleteFromCache)             `shouldBe` False
            doesCreateDir (file, ActionConflict CreatedOnBothSides) `shouldBe` False
            doesCreateDir (file, ActionError)                       `shouldBe` False
        it "delete directory" $ do
            doesDeleteDir (file, ActionSendFile hash)               `shouldBe` False
            doesDeleteDir (file, ActionSendDir)                     `shouldBe` False
            doesDeleteDir (file, ActionGetFile hash)                `shouldBe` False
            doesDeleteDir (file, ActionGetDir)                      `shouldBe` False
            doesDeleteDir (file, ActionDeleteLocalFile)             `shouldBe` False
            doesDeleteDir (file, ActionDeleteLocalDir)              `shouldBe` True
            doesDeleteDir (file, ActionDeleteRemote)                `shouldBe` False
            doesDeleteDir (file, ActionUpdateCache hash)            `shouldBe` False
            doesDeleteDir (file, ActionDeleteFromCache)             `shouldBe` False
            doesDeleteDir (file, ActionConflict CreatedOnBothSides) `shouldBe` False
            doesDeleteDir (file, ActionError)                       `shouldBe` False

    describe "General logic" $ do
        it "compute changes" $ do
            changes []                           []                           `shouldBe` []
            changes [("f1", "h1")]               []                           `shouldBe` [("f1", "h1", FileDeleted)]
            changes [("f1", "h1"), ("f2", "h2")] []                           `shouldBe` [("f1", "h1", FileDeleted), ("f2", "h2", FileDeleted)]
            changes []                           [("f1", "h1")]               `shouldBe` [("f1", "h1", FileCreated)]
            changes []                           [("f1", "h1"), ("f2", "h2")] `shouldBe` [("f1", "h1", FileCreated), ("f2", "h2", FileCreated)]
            changes [("f1", "h1")]               [("f2", "h2")]               `shouldBe` [("f1", "h1", FileDeleted), ("f2", "h2", FileCreated)]
            changes [("f2", "h2")]               [("f1", "h1")]               `shouldBe` [("f1", "h1", FileCreated), ("f2", "h2", FileDeleted)]
            changes [("f1", "h1")]               [("f1", "h1")]               `shouldBe` [("f1", "h1", FileNotModified)]
            changes [("f1", "h1")]               [("f1", "h2")]               `shouldBe` [("f1", "h2", FileModified)]
            changes [("f1", "h1"), ("f3", "h3")] [("f2", "h2"), ("f3", "h3")] `shouldBe` [("f1", "h1", FileDeleted), ("f2", "h2", FileCreated), ("f3", "h3", FileNotModified)]
            changes [("f2", "h2"), ("f3", "h3")] [("f1", "h1"), ("f3", "h3")] `shouldBe` [("f1", "h1", FileCreated), ("f2", "h2", FileDeleted), ("f3", "h3", FileNotModified)]
            changes [("f1", "h1"), ("f3", "h3")] [("f1", "h1"), ("f3", "h3")] `shouldBe` [("f1", "h1", FileNotModified), ("f3", "h3", FileNotModified)]
            changes [("f1", "h1"), ("f3", "h3")] [("f1", "h2"), ("f3", "h3")] `shouldBe` [("f1", "h2", FileModified), ("f3", "h3", FileNotModified)]

    describe "Sync logic for files" $ do
        let f1 = T.pack "f1"
        let h1 = encodeUtf8 (T.pack "h1")
        let h2 = encodeUtf8 (T.pack "h2")
        let h3 = encodeUtf8 (T.pack "h3")
        it "locally created" $ do
            actions SyncDefault []             [("f1", "h1")] []             `shouldBe` [(f1, ActionSendFile h1)]
            actions SyncOurs    []             [("f1", "h1")] []             `shouldBe` [(f1, ActionSendFile h1)]
            actions SyncTheirs  []             [("f1", "h1")] []             `shouldBe` [(f1, ActionSendFile h1)]
        it "remotely created" $ do
            actions SyncDefault []             []             [("f1", "h1")] `shouldBe` [(f1, ActionGetFile h1)]
            actions SyncOurs    []             []             [("f1", "h1")] `shouldBe` [(f1, ActionGetFile h1)]
            actions SyncTheirs  []             []             [("f1", "h1")] `shouldBe` [(f1, ActionGetFile h1)]
        it "locally not modified / remotely not modified" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h1")] [("f1", "h1")] `shouldBe` []
            actions SyncOurs    [("f1", "h1")] [("f1", "h1")] [("f1", "h1")] `shouldBe` []
            actions SyncTheirs  [("f1", "h1")] [("f1", "h1")] [("f1", "h1")] `shouldBe` []
        it "locally not modified / remotely modified" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionGetFile h2)]
            actions SyncOurs    [("f1", "h1")] [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionGetFile h2)]
            actions SyncTheirs  [("f1", "h1")] [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionGetFile h2)]
        it "locally not modified / remotely deleted" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h1")] []             `shouldBe` [(f1, ActionDeleteLocalFile)]
            actions SyncOurs    [("f1", "h1")] [("f1", "h1")] []             `shouldBe` [(f1, ActionDeleteLocalFile)]
            actions SyncTheirs  [("f1", "h1")] [("f1", "h1")] []             `shouldBe` [(f1, ActionDeleteLocalFile)]
        it "locally modified / remotely not modified" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h2")] [("f1", "h1")] `shouldBe` [(f1, ActionSendFile h2)]
            actions SyncOurs    [("f1", "h1")] [("f1", "h2")] [("f1", "h1")] `shouldBe` [(f1, ActionSendFile h2)]
            actions SyncTheirs  [("f1", "h1")] [("f1", "h2")] [("f1", "h1")] `shouldBe` [(f1, ActionSendFile h2)]
        it "locally modified / remotely modified" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h2")] [("f1", "h3")] `shouldBe` [(f1, ActionConflict ModifiedOnBothSides)]
            actions SyncOurs    [("f1", "h1")] [("f1", "h2")] [("f1", "h3")] `shouldBe` [(f1, ActionSendFile h2)]
            actions SyncTheirs  [("f1", "h1")] [("f1", "h2")] [("f1", "h3")] `shouldBe` [(f1, ActionGetFile h3)]
        it "locally modified / remotely deleted" $ do
            actions SyncDefault [("f1", "h1")] [("f1", "h2")] []             `shouldBe` [(f1, ActionConflict LocallyModifiedRemotelyDeleted)]
            actions SyncOurs    [("f1", "h1")] [("f1", "h2")] []             `shouldBe` [(f1, ActionSendFile h2)]
            actions SyncTheirs  [("f1", "h1")] [("f1", "h2")] []             `shouldBe` [(f1, ActionDeleteLocalFile)]
        it "locally created / remotely created" $ do
            actions SyncDefault []             [("f1", "h1")] [("f1", "h1")] `shouldBe` [(f1, ActionUpdateCache h1)]
            actions SyncOurs    []             [("f1", "h1")] [("f1", "h1")] `shouldBe` [(f1, ActionUpdateCache h1)]
            actions SyncTheirs  []             [("f1", "h1")] [("f1", "h1")] `shouldBe` [(f1, ActionUpdateCache h1)]
            actions SyncDefault []             [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionConflict CreatedOnBothSides)]
            actions SyncOurs    []             [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionSendFile h1)]
            actions SyncTheirs  []             [("f1", "h1")] [("f1", "h2")] `shouldBe` [(f1, ActionGetFile h2)]
        it "locally deleted / remotely not modified" $ do
            actions SyncDefault [("f1", "h1")] []             [("f1", "h1")] `shouldBe` [(f1, ActionDeleteRemote)]
            actions SyncOurs    [("f1", "h1")] []             [("f1", "h1")] `shouldBe` [(f1, ActionDeleteRemote)]
            actions SyncTheirs  [("f1", "h1")] []             [("f1", "h1")] `shouldBe` [(f1, ActionDeleteRemote)]
        it "locally deleted / remotely modified" $ do
            actions SyncDefault [("f1", "h1")] []             [("f1", "h2")] `shouldBe` [(f1, ActionConflict LocallyDeletedRemotelyModified)]
            actions SyncOurs    [("f1", "h1")] []             [("f1", "h2")] `shouldBe` [(f1, ActionDeleteRemote)]
            actions SyncTheirs  [("f1", "h1")] []             [("f1", "h2")] `shouldBe` [(f1, ActionGetFile h2)]
        it "locally deleted / remotely deleted" $ do
            actions SyncDefault [("f1", "h1")] []             []             `shouldBe` [(f1, ActionDeleteFromCache)]
            actions SyncOurs    [("f1", "h1")] []             []             `shouldBe` [(f1, ActionDeleteFromCache)]
            actions SyncTheirs  [("f1", "h1")] []             []             `shouldBe` [(f1, ActionDeleteFromCache)]

    describe "Sync logic for directories" $ do
        let f1 = T.pack "f1"
        it "locally created" $ do
            actions SyncDefault []             [("f1", "")]   []           `shouldBe` [(f1, ActionSendDir)]
            actions SyncOurs    []             [("f1", "")]   []           `shouldBe` [(f1, ActionSendDir)]
            actions SyncTheirs  []             [("f1", "")]   []           `shouldBe` [(f1, ActionSendDir)]
        it "remotely created" $ do
            actions SyncDefault []             []             [("f1", "")] `shouldBe` [(f1, ActionGetDir)]
            actions SyncOurs    []             []             [("f1", "")] `shouldBe` [(f1, ActionGetDir)]
            actions SyncTheirs  []             []             [("f1", "")] `shouldBe` [(f1, ActionGetDir)]
        it "locally not modified / remotely not modified" $ do
            actions SyncDefault [("f1", "")]   [("f1", "")]   [("f1", "")] `shouldBe` []
            actions SyncOurs    [("f1", "")]   [("f1", "")]   [("f1", "")] `shouldBe` []
            actions SyncTheirs  [("f1", "")]   [("f1", "")]   [("f1", "")] `shouldBe` []
        it "locally not modified / remotely deleted" $ do
            actions SyncDefault [("f1", "")]   [("f1", "")]   []           `shouldBe` [(f1, ActionDeleteLocalDir)]
            actions SyncOurs    [("f1", "")]   [("f1", "")]   []           `shouldBe` [(f1, ActionDeleteLocalDir)]
            actions SyncTheirs  [("f1", "")]   [("f1", "")]   []           `shouldBe` [(f1, ActionDeleteLocalDir)]
        it "locally created / remotely created" $ do
            actions SyncDefault []             [("f1", "")]   [("f1", "")] `shouldBe` [(f1, ActionUpdateCache B.empty)]
            actions SyncOurs    []             [("f1", "")]   [("f1", "")] `shouldBe` [(f1, ActionUpdateCache B.empty)]
            actions SyncTheirs  []             [("f1", "")]   [("f1", "")] `shouldBe` [(f1, ActionUpdateCache B.empty)]
        it "locally deleted / remotely not modified" $ do
            actions SyncDefault [("f1", "")]   []             [("f1", "")] `shouldBe` [(f1, ActionDeleteRemote)]
            actions SyncOurs    [("f1", "")]   []             [("f1", "")] `shouldBe` [(f1, ActionDeleteRemote)]
            actions SyncTheirs  [("f1", "")]   []             [("f1", "")] `shouldBe` [(f1, ActionDeleteRemote)]
        it "locally deleted / remotely deleted" $ do
            actions SyncDefault [("f1", "")]   []             []           `shouldBe` [(f1, ActionDeleteFromCache)]
            actions SyncOurs    [("f1", "")]   []             []           `shouldBe` [(f1, ActionDeleteFromCache)]
            actions SyncTheirs  [("f1", "")]   []             []           `shouldBe` [(f1, ActionDeleteFromCache)]

-----------------------------------------------------------------------------

changes :: [(String, String)] -> [(String, String)] -> [(String, String, FileStatus)]
changes xs ys = funpack $ computeChanges (fpack xs) (fpack ys)

actions :: SyncPriority -> [(String, String)] -> [(String, String)] -> [(String, String)] -> [FileAction]
actions priority cache local remote = computeActions priority (computeChanges c l) (computeChanges c r)
    where c = fpack cache
          l = fpack local
          r = fpack remote

fpack :: [(String, String)] -> [(T.Text, B.ByteString)]
fpack = map (\(path, hash) -> (T.pack path, encodeUtf8 (T.pack hash)))

funpack :: [(T.Text, B.ByteString, FileStatus)] -> [(String, String, FileStatus)]
funpack = map (\(path, hash, status) -> (T.unpack path, T.unpack (decodeUtf8 hash), status))

-----------------------------------------------------------------------------
