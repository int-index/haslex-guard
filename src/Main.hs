#!/usr/bin/env stack
{- stack script
  --resolver lts-6.30
  --package "semigroups optional-args text system-filepath turtle" -}

{-# LANGUAGE
  OverloadedStrings, MultiWayIf, LambdaCase,
  DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Main where

import Data.Char as C
import Data.Foldable as F
import Data.IORef
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Optional
import Data.Semigroup
import Data.Text as T
import Filesystem.Path.CurrentOS as FS
import Turtle hiding ((<>))

plural :: Int -> String -> String -> String
plural n a b
  | n == 1    = a
  | otherwise = b

data Options = Options
  { opSrcPaths :: [Turtle.FilePath]
  } deriving ()

data Neighbours a = Neighbours
  { nbPrev :: Maybe a
  , nbCurr :: a
  , nbNext :: Maybe a
  } deriving (Functor, Foldable, Traversable, Show)

mkNeighbours :: [a] -> [Neighbours a]
mkNeighbours [] = []
mkNeighbours [a] = [Neighbours Nothing a Nothing]
mkNeighbours [a, b] = [Neighbours (Just a) b Nothing]
mkNeighbours (a : b : c : xs) =
  Neighbours (Just a) b (Just c) : mkNeighbours (b : c : xs)

data Numbered num a = Numbered num a
  deriving (Functor)

getNumber :: Numbered num a -> num
getNumber (Numbered num _) = num

getNumbered :: Numbered Text Text -> Text
getNumbered (Numbered num a) = num <> " " <> a

renderNumber :: Show num => Numbered num a -> Numbered Text a
renderNumber (Numbered num a) = Numbered (T.pack (show num)) a

justifyNumber :: Int -> Numbered Text a -> Numbered Text a
justifyNumber n (Numbered num a) = Numbered (T.justifyRight n ' ' num) a

justifyNumbers
  :: Traversable t => t (Numbered Text a) -> (Int, t (Numbered Text a))
justifyNumbers numbers = (maxLen, justifyNumber maxLen <$> numbers)
  where
    maxLen = L.maximum (T.length . getNumber <$> numbers)

numberNeighbours
  :: Enum num => num -> Neighbours a -> Neighbours (Numbered num a)
numberNeighbours numCurr (Neighbours mPrev curr mNext) =
  Neighbours
    (Numbered numPrev <$> mPrev)
    (Numbered numCurr curr)
    (Numbered numNext <$> mNext)
  where
    numPrev = pred numCurr
    numNext = succ numCurr

data Range n = Range n n | RangeOffset n n
  deriving (Show)

rangeMax :: Range Int -> Int
rangeMax (Range n m) = max n m
rangeMax (RangeOffset n m) = n + m

data StyleError = StyleError
  { seLineNum   :: Int
  , seLines     :: Neighbours Text
  , seBadRanges :: NonEmpty (Range Int)
  , seMessage   :: Text
  } deriving (Show)

withinRange :: Int -> Range Int -> Bool
withinRange k (Range n m) = k >= n && k < m
withinRange k (RangeOffset n m) = withinRange k (Range n (n + m))

withinRanges :: Foldable f => Int -> f (Range Int) -> Bool
withinRanges k = L.any (withinRange k)

highlightRanges :: NonEmpty (Range Int) -> Text
highlightRanges rs = T.pack (L.take maxRange symbols)
  where
    symbols  = L.map (\k -> if withinRanges k rs then '^' else ' ') [0..]
    maxRange = L.maximum (fmap rangeMax rs)

printStyleError :: Turtle.FilePath -> StyleError -> Text
printStyleError srcFilePath se =
  T.unlines $
    mconcat
      [ [format fp srcFilePath]
      , maybeToList (nbPrev flatLines)
      , [nbCurr flatLines]
      , extraLines
      , maybeToList (nbNext flatLines) ]
  where
    (lineNumberOffset, numberedLines) =
      justifyNumbers
        (renderNumber <$> numberNeighbours (seLineNum se) (seLines se))
    flatLines = getNumbered <$> numberedLines
    extraLines =
      let
        leftOffset   = T.replicate (lineNumberOffset + 1) " "
        hl           = highlightRanges (seBadRanges se)
        hlGroups     = T.group hl
        message      = seMessage se
        noInline     = [hl, message]
        inlineLeft   = do
          (hlG, hlGs) <- L.uncons hlGroups
          guard $ " " `T.isPrefixOf` hlG
            && T.length hlG > T.length message
          Just
            [T.justifyRight (T.length hlG - 1) ' ' message
              <> " " <> mconcat hlGs]
        inlineRight  = do
          let extraLine = hl <> " " <> message
          guard (T.length extraLine <= 80)
          Just [extraLine]
      in L.map (leftOffset <>) $
        fromMaybe noInline (inlineLeft <|> inlineRight)

checkStyle :: [Text] -> [StyleError]
checkStyle ts = do
  neighbours <- L.zipWith Numbered [0..] (mkNeighbours ts)
  check <- [checkIndentStep, checkColumnMargin, checkTrailingSpace]
  check neighbours

checkIndentStep :: Numbered Int (Neighbours Text) -> [StyleError]
checkIndentStep (Numbered num nb@(Neighbours (Just a) b _)) =
  let
    countIndent = T.length . T.takeWhile (==' ')
    aIndent = countIndent a
    bIndent = countIndent b
    indentDiff = countIndent b - countIndent a
    goodIndentDiff = indentDiff <= 0 || indentDiff == 2
    message = "Thou shalt not indent with " <> T.pack (show indentDiff)
      <> " " <> T.pack (plural indentDiff "space" "spaces") <> "! ಠ_ಠ"
  in do
    badRange <- if
      | goodIndentDiff -> []
      | otherwise      -> [pure (Range aIndent bIndent)]
    [StyleError num nb badRange message]
checkIndentStep _ = []

checkColumnMargin :: Numbered Int (Neighbours Text) -> [StyleError]
checkColumnMargin (Numbered num nb) =
  let
    columnMargin = T.length (nbCurr nb)
    goodColumnMargin = columnMargin <= 80
    message = "Thou shalt not exceed the 80 characters limit!"
  in do
    badRange <- if
      | goodColumnMargin -> []
      | otherwise        -> [pure (Range 81 columnMargin)]
    [StyleError num nb badRange message]

checkTrailingSpace :: Numbered Int (Neighbours Text) -> [StyleError]
checkTrailingSpace (Numbered num nb) =
  let
    countTrailing = T.length . T.takeWhile C.isSpace . T.reverse
    trailingLen = countTrailing (nbCurr nb)
    lineLen = T.length (nbCurr nb)
    trailingStart = lineLen - trailingLen
    goodTrailingLen = trailingLen == 0
    message = "Thou shalt not end a line with a space!"
  in do
    badRange <- if
      | goodTrailingLen -> []
      | otherwise       -> [pure (Range trailingStart lineLen)]
    [StyleError num nb badRange message]

findHsFiles :: Turtle.FilePath -> Shell Turtle.FilePath
findHsFiles dir = do
  path <- lsif (pure . isNotIgnored) dir
  True <- return (FS.hasExtension path "hs")
  return path
  where
    isNotIgnored :: Turtle.FilePath -> Bool
    isNotIgnored p = dirname p /= ".stack-work" && dirname p /= "dist"

main :: IO ()
main = do
  detectedErrorsRef <- newIORef (0 :: Int)
  sh $ do
    opts <- options "Haslex Guard" $
      Options <$> many (argPath "SOURCE" Default)
    srcPath <- select $ case opSrcPaths opts of
      []       -> ["."]
      srcPaths -> srcPaths
    srcFilePath <- testfile srcPath >>= \case
      True  -> srcPath <$ guard (FS.hasExtension srcPath "hs")
      False -> testdir srcPath >>= \case
        True  -> findHsFiles srcPath
        False -> die $ format ("Path not found: " % fp) srcPath
    src <- liftIO $ readTextFile srcFilePath
    let styleErrors = checkStyle (T.lines src)
    traverse_ (echo . printStyleError srcFilePath) styleErrors
    unless (L.null styleErrors) $ liftIO $ modifyIORef detectedErrorsRef (+1)
  readIORef detectedErrorsRef >>= \case
    0 -> exit ExitSuccess
    n -> do
      echo $ "Detected " <> T.pack (show n) <> " " <>
        fromString (plural n "error" "errors") <> ".\n"
      exit (ExitFailure 2)
