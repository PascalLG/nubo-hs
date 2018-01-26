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

module EnvironmentSpec (
    spec
) where

import Test.Hspec
import Error.Internal
import Environment

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "Parse args" $ do
        let options = [OptionDry, OptionForce, OptionTest]
        it "no options" $ do
            parseArgs [] options `shouldBe` ([], [], [])
            parseArgs ["arg 1"] options `shouldBe` ([], [], ["arg 1"])
            parseArgs ["arg 1", "arg 2"] options `shouldBe` ([], [], ["arg 1", "arg 2"])
        
        it "long options" $ do
            parseArgs ["--dry"] options `shouldBe` ([], [OptionDry], [])
            parseArgs ["--dry", "arg 1"] options `shouldBe` ([], [OptionDry], ["arg 1"])
            parseArgs ["--dry", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry], ["arg 1", "arg 2"])
            parseArgs ["--dry", "--force"] options `shouldBe` ([], [OptionDry, OptionForce], [])
            parseArgs ["--dry", "--force", "arg 1"] options `shouldBe` ([], [OptionDry, OptionForce], ["arg 1"])
            parseArgs ["--dry", "--force", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry, OptionForce], ["arg 1", "arg 2"])
            parseArgs ["--test", "--dry", "--force"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], [])
            parseArgs ["--test", "--dry", "--force", "arg 1"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], ["arg 1"])
            parseArgs ["--test", "--dry", "--force", "arg 1", "arg 2"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], ["arg 1", "arg 2"])

        it "short options" $ do
            parseArgs ["-d"] options `shouldBe` ([], [OptionDry], [])
            parseArgs ["-d", "arg 1"] options `shouldBe` ([], [OptionDry], ["arg 1"])
            parseArgs ["-d", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry], ["arg 1", "arg 2"])
            parseArgs ["-d", "-f"] options `shouldBe` ([], [OptionDry, OptionForce], [])
            parseArgs ["-d", "-f", "arg 1"] options `shouldBe` ([], [OptionDry, OptionForce], ["arg 1"])
            parseArgs ["-d", "-f", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry, OptionForce], ["arg 1", "arg 2"])
            parseArgs ["-u", "-d", "-f"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], [])
            parseArgs ["-u", "-d", "-f", "arg 1"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], ["arg 1"])
            parseArgs ["-u", "-d", "-f", "arg 1", "arg 2"] options `shouldBe` ([], [OptionTest, OptionDry, OptionForce], ["arg 1", "arg 2"])
            parseArgs ["-du"] options `shouldBe` ([], [OptionDry, OptionTest], [])
            parseArgs ["-du", "arg 1"] options `shouldBe` ([], [OptionDry, OptionTest], ["arg 1"])
            parseArgs ["-du", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry, OptionTest], ["arg 1", "arg 2"])
            parseArgs ["-du", "-f"] options `shouldBe` ([], [OptionDry, OptionTest, OptionForce], [])
            parseArgs ["-du", "-f", "arg 1"] options `shouldBe` ([], [OptionDry, OptionTest, OptionForce], ["arg 1"])
            parseArgs ["-du", "-f", "arg 1", "arg 2"] options `shouldBe` ([], [OptionDry, OptionTest, OptionForce], ["arg 1", "arg 2"])
            parseArgs ["-fdu"] options `shouldBe` ([], [OptionForce, OptionDry, OptionTest], [])
            parseArgs ["-fdu", "arg 1"] options `shouldBe` ([], [OptionForce, OptionDry, OptionTest], ["arg 1"])
            parseArgs ["-fdu", "arg 1", "arg 2"] options `shouldBe` ([], [OptionForce, OptionDry, OptionTest], ["arg 1", "arg 2"])

        it "duplicates" $ do
            parseArgs ["--dry", "--dry"] options `shouldBe` ([], [OptionDry], [])
            parseArgs ["--dry", "--test", "--dry"] options `shouldBe` ([], [OptionDry, OptionTest], [])
            parseArgs ["-dufduf"] options `shouldBe` ([], [OptionDry, OptionTest, OptionForce], [])

        it "errors" $ do
            parseArgs ["--bar"] options `shouldBe` ([ErrUnsupportedOption "--bar"], [], [])
            parseArgs ["--test", "--bar"] options `shouldBe` ([ErrUnsupportedOption "--bar"], [OptionTest], [])
            parseArgs ["--bar", "--test"] options `shouldBe` ([ErrUnsupportedOption "--bar"], [OptionTest], [])
            parseArgs ["--bar", "--test", "--xyz"] options `shouldBe` ([ErrUnsupportedOption "--bar", ErrUnsupportedOption "--xyz"], [OptionTest], [])
            parseArgs ["-b"] options `shouldBe` ([ErrUnsupportedOption "-b"], [], [])
            parseArgs ["-bu"] options `shouldBe` ([ErrUnsupportedOption "-b"], [OptionTest], [])
            parseArgs ["-ub"] options `shouldBe` ([ErrUnsupportedOption "-b"], [OptionTest], [])
            parseArgs ["-bux"] options `shouldBe` ([ErrUnsupportedOption "-b", ErrUnsupportedOption "-x"], [OptionTest], [])
            parseArgs ["-bb", "arg 1"] options `shouldBe` ([ErrUnsupportedOption "-b"], [], ["arg 1"])
            parseArgs ["-bub", "arg 1"] options `shouldBe` ([ErrUnsupportedOption "-b"], [OptionTest], ["arg 1"])

-----------------------------------------------------------------------------
