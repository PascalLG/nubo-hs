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

module Matching.Internal where

import qualified Data.Text as T
import Data.Binary (Binary(..), putWord8, getWord8)
import System.FilePath (splitPath, joinPath, pathSeparator, isPathSeparator)
import Data.Monoid ((<>))
import Control.Monad (liftM, liftM2)
import Data.Text.Normalize
import Misc
import Error

-----------------------------------------------------------------------------

-- | Patterns come in two flavors: relative patterns used to exclude
-- any file that matches whatever the directory it resides in, and
-- absolute patterns used to exclude a specific file.

data Pattern = RelativePattern Glob
             | AbsolutePattern [Glob]
             deriving (Eq)

instance Show Pattern where
    show pattern = case pattern of
        RelativePattern g  -> show g
        AbsolutePattern gs -> joinPath ("." : (map show gs))

instance Binary Pattern where
    put (RelativePattern g) = putWord8 1 <> put g
    put (AbsolutePattern gs) = putWord8 2 <> put gs
    get = getWord8 >>= \t -> case t of
            1 -> liftM RelativePattern get
            2 -> liftM AbsolutePattern get
            _ -> fail "unknown pattern type"

instance Ord Pattern where
    compare (RelativePattern x) (RelativePattern y) = compare x y
    compare (AbsolutePattern x) (AbsolutePattern y) = compare x y
    compare (RelativePattern _) (AbsolutePattern _) = LT
    compare (AbsolutePattern _) (RelativePattern _) = GT

-- | Glob pattern. It is a list of terms, and a boolean flag indicating
-- whether the pattern can match any file name or only a folder name.
--
data Glob = Glob [MatchTerm] Bool
          deriving (Eq)

instance Show Glob where
    show (Glob m dir) = T.unpack (showg m) ++ (if dir then [pathSeparator] else "")
        where
            showg :: [MatchTerm] -> T.Text
            showg [] = T.empty
            showg (MatchLiteral t:ts) = T.append t $ showg ts
            showg (MatchAny:ts) = T.cons '?' $ showg ts
            showg (MatchDir:ts) = T.cons '*' $ showg ts

instance Binary Glob where
    put (Glob ts dir) = put ts <> put dir
    get = liftM2 Glob get get

instance Ord Glob where
    compare (Glob t1 f1) (Glob t2 f2) = let r = compare t1 t2
                                        in  if r == EQ then compare f1 f2 else r

-- | A match term. Beside literal text segments, only ? and * wildcards
-- are supported. A full glob pattern is simply a list of match terms.
--
data MatchTerm = MatchLiteral T.Text
               | MatchAny
               | MatchDir
               deriving (Eq)

instance Binary MatchTerm where
    put (MatchLiteral t) = putWord8 1 <> put t
    put MatchAny = putWord8 2
    put MatchDir = putWord8 3
    get = getWord8 >>= \t -> case t of
            1 -> liftM MatchLiteral get
            2 -> return MatchAny
            3 -> return MatchDir
            _ -> fail "unknown pattern type"

instance Ord MatchTerm where
    compare (MatchLiteral x) (MatchLiteral y) = compare x y
    compare (MatchLiteral _) MatchAny = LT
    compare (MatchLiteral _) MatchDir = LT
    compare MatchAny MatchDir = LT
    compare x y | x == y = EQ
    compare _ _ = GT

-----------------------------------------------------------------------------

-- | Parse a string and return either a pattern object or
-- an error if the pattern is invalid.
--
parsePattern :: String -> Either Error Pattern
parsePattern p = case map parse (splitPath p) of
                    []          -> Left $ ErrInvalidPattern p
                    [Glob [] _] -> Left $ ErrInvalidPattern p
                    [t]         -> case isRoot t of
                                    False -> Right $ RelativePattern t
                                    _     -> Left $ ErrInvalidPattern p
                    ts          -> case dropWhile isRoot ts of
                                    []    -> Left $ ErrInvalidPattern p
                                    gs    -> Right $ AbsolutePattern gs
    where
        parse :: FilePath -> Glob
        parse path = Glob (parseGlob (dropWhileEnd isPathSeparator path)) (any isPathSeparator path)

        isRoot :: Glob -> Bool
        isRoot (Glob [MatchLiteral a] True) | a == T.singleton '.' = True
        isRoot (Glob [] True) = True
        isRoot _ = False

-- | Parse a glob pattern and return a list of match terms. Only *
-- and ? wildcards are supported. Text encoding is NFC-normalized
-- to allow easy comparing with universal filenames returned by
-- the FileHelper module.
--
parseGlob :: String -> [MatchTerm]
parseGlob = map norm . simplify . map parse
    where
        parse :: Char -> MatchTerm
        parse '*' = MatchDir
        parse '?' = MatchAny
        parse c = MatchLiteral $ T.singleton c

        simplify :: [MatchTerm] -> [MatchTerm]
        simplify (MatchLiteral a:MatchLiteral b:ts) = simplify (MatchLiteral (T.append a b):ts)
        simplify (MatchDir:m@(MatchDir:_)) = simplify m
        simplify (t:ts) = t : simplify ts
        simplify [] = []

        norm :: MatchTerm -> MatchTerm
        norm (MatchLiteral t) = MatchLiteral (normalize NFC t)
        norm m = m

-----------------------------------------------------------------------------

-- | Match a pattern against a path. Note: the boolean parameter
-- associated with the path indicates whether that path refers to
-- a file or a folder.
--
isPatternMatching :: Pattern -> (T.Text, Bool) -> Bool
isPatternMatching pattern (upath, isdir) = let paths = splitUPath upath isdir
                                           in case pattern of
                                                  RelativePattern g -> any (match g) paths
                                                  AbsolutePattern gs -> all (uncurry match) (zip gs paths)
    where
        match :: Glob -> (T.Text, Bool) -> Bool
        match (Glob ts dironly) (path, isdir') = (isdir' || not dironly) && matchGlob ts path

-- | Helper function that splits a path, associating a boolean True
-- with components refering to a folder and False with components
-- refering to files.
--
splitUPath :: T.Text -> Bool -> [(T.Text, Bool)]
splitUPath p dir = map (\x -> (x, True)) (init lst) ++ [(last lst, dir)]
    where lst = T.split (== '/') p

-- | Helper function that matches a glob pattern against a
-- string.
--
matchGlob :: [MatchTerm] -> T.Text -> Bool
matchGlob [] text = T.null text
matchGlob (MatchAny:ts) text = not (T.null text) && matchGlob ts (T.tail text)
matchGlob (MatchDir:ts) text = or $ map (matchGlob ts) (T.tails text)
matchGlob (MatchLiteral t:ts) text = case T.stripPrefix t text of
                                        Just suffix -> matchGlob ts suffix
                                        Nothing     -> False

-- | Helper function that check if a given file matches at least one
-- of the patterns in a list.
--
isPathMatchingOneOf :: [Pattern] -> (T.Text, Bool) -> Bool
isPathMatchingOneOf patterns file = any (\pattern -> isPatternMatching pattern file) patterns

-----------------------------------------------------------------------------
