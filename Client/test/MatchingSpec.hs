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

module MatchingSpec (
    spec
) where

import qualified Data.Text as T
import Data.Binary (encode, decode)
import Test.Hspec
import Error
import Matching.Internal

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "helpers" $ do
        it "split" $ do
            splitUPath (T.pack "a") False       `shouldBe` [(T.pack "a", False)]
            splitUPath (T.pack "a") True        `shouldBe` [(T.pack "a", True)]
            splitUPath (T.pack "a/b") False     `shouldBe` [(T.pack "a", True), (T.pack "b", False)]
            splitUPath (T.pack "a/b") True      `shouldBe` [(T.pack "a", True), (T.pack "b", True)]
            splitUPath (T.pack "a/b/c") False   `shouldBe` [(T.pack "a", True), (T.pack "b", True), (T.pack "c", False)]
            splitUPath (T.pack "a/b/c") True    `shouldBe` [(T.pack "a", True), (T.pack "b", True), (T.pack "c", True)]

        it "match glob" $ do
            matchGlob [] (T.empty)               `shouldBe` True
            matchGlob [] (T.pack "xyz")          `shouldBe` False

            matchGlob [MatchAny] (T.empty)       `shouldBe` False
            matchGlob [MatchAny] (T.pack "x")    `shouldBe` True
            matchGlob [MatchAny] (T.pack "xy")   `shouldBe` False

            matchGlob [MatchDir] (T.empty)       `shouldBe` True
            matchGlob [MatchDir] (T.pack "x")    `shouldBe` True
            matchGlob [MatchDir] (T.pack "xy")   `shouldBe` True

            let pattern1 = [MatchLiteral (T.pack "abc")]
            matchGlob pattern1 (T.empty)         `shouldBe` False
            matchGlob pattern1 (T.pack "ab")     `shouldBe` False
            matchGlob pattern1 (T.pack "abc")    `shouldBe` True
            matchGlob pattern1 (T.pack "abcd")   `shouldBe` False
            matchGlob pattern1 (T.pack "xyz")    `shouldBe` False

            let pattern2 = [MatchLiteral (T.pack "ab"), MatchAny, MatchLiteral (T.pack "cd")]
            matchGlob pattern2 (T.empty)            `shouldBe` False
            matchGlob pattern2 (T.pack "abcd")      `shouldBe` False
            matchGlob pattern2 (T.pack "abxcd")     `shouldBe` True
            matchGlob pattern2 (T.pack "abxycd")    `shouldBe` False
            matchGlob pattern2 (T.pack "abx")       `shouldBe` False

            let pattern3 = [MatchLiteral (T.pack "ab"), MatchDir, MatchLiteral (T.pack "cd")]
            matchGlob pattern3 (T.empty)            `shouldBe` False
            matchGlob pattern3 (T.pack "abcd")      `shouldBe` True
            matchGlob pattern3 (T.pack "abxcd")     `shouldBe` True
            matchGlob pattern3 (T.pack "abxycd")    `shouldBe` True
            matchGlob pattern3 (T.pack "abxy")      `shouldBe` False

            let pattern4 = [MatchLiteral (T.pack "ab"), MatchAny, MatchDir, MatchLiteral (T.pack "cd")]
            matchGlob pattern4 (T.empty)            `shouldBe` False
            matchGlob pattern4 (T.pack "abcd")      `shouldBe` False
            matchGlob pattern4 (T.pack "abxcd")     `shouldBe` True
            matchGlob pattern4 (T.pack "abxycd")    `shouldBe` True
            matchGlob pattern4 (T.pack "abx")       `shouldBe` False

    describe "parse" $ do
        it "glob" $ do
            parsePattern "a"        `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "a")] False))
            parsePattern "ab"       `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab")] False))
            parsePattern "ab?"      `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab"), MatchAny] False))
            parsePattern "ab?c"     `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab"), MatchAny, MatchLiteral (T.pack "c")] False))
            parsePattern "ab*"      `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab"), MatchDir] False))
            parsePattern "ab*c"     `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab"), MatchDir, MatchLiteral (T.pack "c")] False))
            parsePattern "ab*cd"    `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "ab"), MatchDir, MatchLiteral (T.pack "cd")] False))
            parsePattern "?ab"      `shouldBe` Right (RelativePattern (Glob [MatchAny, MatchLiteral (T.pack "ab")] False))
            parsePattern "*ab"      `shouldBe` Right (RelativePattern (Glob [MatchDir, MatchLiteral (T.pack "ab")] False))

        it "relative patterns" $ do
            parsePattern "abc"      `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "abc")] False))
            parsePattern "abc/"     `shouldBe` Right (RelativePattern (Glob [MatchLiteral (T.pack "abc")] True))

        it "absolute patterns" $ do
            parsePattern "/abc"     `shouldBe` Right (AbsolutePattern [Glob [MatchLiteral (T.pack "abc")] False])
            parsePattern "./abc"    `shouldBe` Right (AbsolutePattern [Glob [MatchLiteral (T.pack "abc")] False])
            parsePattern "./abc/"   `shouldBe` Right (AbsolutePattern [Glob [MatchLiteral (T.pack "abc")] True])
            parsePattern "abc/def"  `shouldBe` Right (AbsolutePattern [Glob [MatchLiteral (T.pack "abc")] True, Glob [MatchLiteral (T.pack "def")] False])
            parsePattern "abc/def/" `shouldBe` Right (AbsolutePattern [Glob [MatchLiteral (T.pack "abc")] True, Glob [MatchLiteral (T.pack "def")] True])

        it "invalid patterns" $ do
            parsePattern ""         `shouldBe` Left (ErrInvalidPattern "")
            parsePattern "/"        `shouldBe` Left (ErrInvalidPattern "/")
            parsePattern "//"       `shouldBe` Left (ErrInvalidPattern "//")
            parsePattern "./"       `shouldBe` Left (ErrInvalidPattern "./")
            parsePattern ".//"      `shouldBe` Left (ErrInvalidPattern ".//")

    describe "put/get" $ do
        it "serialise pattern" $ do
            "a"             `shouldSatisfy` encdec
            "ab"            `shouldSatisfy` encdec
            "ab?"           `shouldSatisfy` encdec
            "ab*"           `shouldSatisfy` encdec
            "a?b*c"         `shouldSatisfy` encdec
            "abc/"          `shouldSatisfy` encdec
            "./abc"         `shouldSatisfy` encdec
            "./abc/"        `shouldSatisfy` encdec
            "./abc/def"     `shouldSatisfy` encdec
            "./ab*/def/"    `shouldSatisfy` encdec

    describe "pattern matching" $ do
        it "relative" $ do
            let Right pattern1 = parsePattern "ab*"
            isPatternMatching pattern1 (T.pack "a/x/y", False)      `shouldBe` False
            isPatternMatching pattern1 (T.pack "ab/b/c", False)     `shouldBe` True
            isPatternMatching pattern1 (T.pack "abc/b/c", False)    `shouldBe` True
            isPatternMatching pattern1 (T.pack "abcd/b/c", False)   `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/a/y", False)      `shouldBe` False
            isPatternMatching pattern1 (T.pack "x/ab/y", False)     `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/abc/y", False)    `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/abcd/y", False)   `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/y/a", False)      `shouldBe` False
            isPatternMatching pattern1 (T.pack "x/y/ab", False)     `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/y/abc", False)    `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/y/abcd", False)   `shouldBe` True

            let Right pattern2 = parsePattern "ab*/"
            isPatternMatching pattern2 (T.pack "a/x/y", False)      `shouldBe` False
            isPatternMatching pattern2 (T.pack "ab/b/c", False)     `shouldBe` True
            isPatternMatching pattern2 (T.pack "abc/b/c", False)    `shouldBe` True
            isPatternMatching pattern2 (T.pack "abcd/b/c", False)   `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/a/y", False)      `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/ab/y", False)     `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/abc/y", False)    `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/abcd/y", False)   `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/y/a", False)      `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/y/ab", False)     `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/y/abc", False)    `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/y/abcd", False)   `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/y/a", True)       `shouldBe` False
            isPatternMatching pattern2 (T.pack "x/y/ab", True)      `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/y/abc", True)     `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/y/abcd", True)    `shouldBe` True

        it "absolute" $ do
            let Right pattern1 = parsePattern "a/?/c"
            isPatternMatching pattern1 (T.pack "a/b/c", False)      `shouldBe` True
            isPatternMatching pattern1 (T.pack "aa/b/c", False)     `shouldBe` False
            isPatternMatching pattern1 (T.pack "a/bb/c", False)     `shouldBe` False
            isPatternMatching pattern1 (T.pack "a/b/cc", False)     `shouldBe` False
            isPatternMatching pattern1 (T.pack "a/b/c/d", False)    `shouldBe` True
            isPatternMatching pattern1 (T.pack "x/a/b/c", False)    `shouldBe` False
            let Right pattern2 = parsePattern "a/?/c/"
            isPatternMatching pattern2 (T.pack "a/b/c", False)      `shouldBe` False
            isPatternMatching pattern2 (T.pack "a/b/c", True)       `shouldBe` True
            isPatternMatching pattern2 (T.pack "a/b/c/d", False)    `shouldBe` True
            isPatternMatching pattern2 (T.pack "x/a/b/c", True)     `shouldBe` False

encdec :: String -> Bool
encdec text = (decode . encode) pat == pat
    where Right pat = parsePattern text

-----------------------------------------------------------------------------
