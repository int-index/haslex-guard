module Main where

import Data.Foldable (toList, traverse_)
import Options.Generic
import qualified Control.Foldl as F
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Turtle hiding ((<>))

data Options = Options
  { src_path :: Text
  } deriving (Generic)

instance ParseRecord Options

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
    maxLen = maximum (T.length . getNumber <$> numbers)

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
withinRanges k = any (withinRange k)

highlightRanges :: NonEmpty (Range Int) -> Text
highlightRanges rs = T.pack (take maxRange symbols)
  where
    symbols  = map (\k -> if withinRanges k rs then '^' else ' ') [0..]
    maxRange = maximum (fmap rangeMax rs)

printStyleError :: StyleError -> Text
printStyleError se =
  T.unlines $
    mconcat
      [ toList (nbPrev flatLines)
      , [nbCurr flatLines, extraLine]
      , toList (nbNext flatLines) ]
  where
    (lineNumberOffset, numberedLines) =
      justifyNumbers
        (renderNumber <$> numberNeighbours (seLineNum se) (seLines se))
    flatLines = getNumbered <$> numberedLines
    extraLine = T.replicate (lineNumberOffset + 1) " " <>
      highlightRanges (seBadRanges se) <> " " <> seMessage se

checkIndentStyle :: [Text] -> [StyleError]
checkIndentStyle = checkIndentStyle1 <=< zipWith Numbered [0..] . mkNeighbours
  where
    checkIndentStyle1 :: Numbered Int (Neighbours Text) -> [StyleError]
    checkIndentStyle1 (Numbered num nb@(Neighbours (Just a) b _)) =
      let
        countIndent = T.length . T.takeWhile (==' ')
        aIndent = countIndent a
        bIndent = countIndent b
        indentDiff = countIndent b - countIndent a
        goodIndentDiff = indentDiff <= 0 || indentDiff == 2
        message = "Thou shall not indent with " <> T.pack (show indentDiff)
          <> " spaces! ಠ_ಠ"
      in do
        badRange <- if
          | goodIndentDiff -> []
          | otherwise      -> [pure (Range aIndent bIndent)]
        [StyleError num nb badRange message]
    checkIndentStyle1 _ = []

main :: IO ()
main = do
  opts <- getRecord "Haslex Guard"
  let srcPath = fromText (src_path opts)
  src <- fold (input srcPath) F.list
  let styleErrors = checkIndentStyle src
  traverse_ (echo . printStyleError) styleErrors
