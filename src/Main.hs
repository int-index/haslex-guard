module Main where

import Data.Foldable (toList, traverse_)
import Options.Generic
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Char as C
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Turtle hiding ((<>))
import Language.English.Plural

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
      , [nbCurr flatLines]
      , extraLines
      , toList (nbNext flatLines) ]
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
      in map (leftOffset <>) $
        fromMaybe noInline (inlineLeft <|> inlineRight)

checkStyle :: [Text] -> [StyleError]
checkStyle ts = do
  neighbours <- zipWith Numbered [0..] (mkNeighbours ts)
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
      <> " " <> T.pack (tryPlural indentDiff "space") <> "! ಠ_ಠ"
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

main :: IO ()
main = do
  opts <- getRecord "Haslex Guard"
  src <- T.readFile (T.unpack (src_path opts))
  let styleErrors = checkStyle (T.lines src)
  traverse_ (echo . printStyleError) styleErrors
