{-# LANGUAGE OverloadedStrings #-}

module Lib ( Params(..)
           , Paragraph(..)
           , ParaClass(..)
           , justext
           , defaultParams
           , getHeadingTag
           , isBoilerPlate
           ) where

import Data.List (find, foldl', dropWhileEnd)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (length, null, findIndex, lines, strip, split, count)
import qualified Data.Text.ICU as R (Match, find, findAll, span, group, regex, prefix, suffix)
import Text.HTML.Parser (Token(..), parseTokens)

data ParaClass
  = Good
  | NearGood
  | Bad
  | Short
  deriving (Eq)

instance Show ParaClass where
  show c = case c of
             Good -> "good"
             NearGood -> "nearGood"
             Bad -> "bad"
             Short -> "short"

data Paragraph = Paragraph { text :: Text
                           , linkDensity :: Float
                           , charCountInLinks :: Int
                           , classType :: ParaClass
                           , path :: [Text]
                           , textLength :: Int
                           }
                 deriving (Show)

data Params = Params { maxLinkDensity :: Float
                     , lengthLow :: Int
                     , lengthHigh :: Int
                     , stopWordsLow :: Float
                     , stopWordsHigh :: Float
                     , maxHeadingDistance :: Int }

defaultParams :: Params
defaultParams =
  Params { maxLinkDensity = 0.2
         , lengthLow = 70
         , lengthHigh = 200
         , stopWordsLow = 0.30
         , stopWordsHigh = 0.32
         , maxHeadingDistance = 200 }

paragraphTags :: [Text]
paragraphTags = [
  "body", "blockquote", "caption", "center", "col", "colgroup", "dd",
  "div", "dl", "dt", "fieldset", "form", "legend", "optgroup", "option",
  "p", "pre", "table", "td", "textarea", "tfoot", "th", "thead", "tr",
  "ul", "li", "h1", "h2", "h3", "h4", "h5", "h6"
  ]

stripTags :: [Text]
stripTags = ["head", "style", "script", "form"]

noCloseTags :: [Text]
noCloseTags = ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "menuitem", "meta", "param", "source", "track", "wbr"]

data Tag = Tag [Text] Token deriving (Show)

tagName :: Tag -> Text
tagName (Tag _ tok) =
  case tok of
    TagOpen name _ -> name
    TagSelfClose name _ -> name
    TagClose name -> name
    _ -> ""

getText :: Tag -> Text
getText (Tag _ (ContentText text)) = text
getText _ = ""

getPath :: Tag -> [Text]
getPath (Tag path _) = path

pathContains :: [Text] -> Tag -> Bool
pathContains tagNames tag@(Tag path tok) =
  any (`elem` tagNames) (path ++ [tagName tag])

contains :: Char -> Text -> Bool
contains char = isJust . T.findIndex (char ==)

normalizeWhitespace :: Text -> Text
normalizeWhitespace txt =
  let matches = R.findAll (R.regex [] "\\s+") txt
  in case matches of
       [] -> txt
       m : [] -> fromJust (R.prefix 0 m) <> sep m <> fromJust (R.suffix 0 m)
       m : rest -> fromJust (R.prefix 0 m) <> sep m <> join rest
  where
    join :: [R.Match] -> Text
    join [] = ""
    join [m] = R.span m <> sep m <> fromJust (R.suffix 0 m)
    join (m : rest) = R.span m <> sep m <> join rest
        
    sep :: R.Match -> Text
    sep match =
      let m = fromJust (R.group 0 match)
      in if contains '\n' m then "\n" else " "


groupTags :: [Tag] -> [([Text], [Tag])]
groupTags [] = []
groupTags tags =
  let (group, rest) = nextGroup False [] tags
  in group : groupTags rest
  where
    nextGroup :: Bool -> [Tag] -> [Tag] -> (([Text], [Tag]), [Tag])
    nextGroup _ acc [] = (([], acc), [])
    nextGroup isPrevBreak acc (tag : rest)
      | isParaSeparator tag = ((getPath tag, acc), rest)
      | isBreak tag && isPrevBreak = ((getPath tag, init acc), rest)
      | isBreak tag = nextGroup True (acc ++ [tag]) rest
      | Tag _ (ContentText txt) <- tag =
          if T.null (T.strip txt)
             then nextGroup isPrevBreak (acc ++ [tag]) rest
             else nextGroup False (acc ++ [tag]) rest
      | otherwise = nextGroup False (acc ++ [tag]) rest
      
    isParaSeparator :: Tag -> Bool
    isParaSeparator = (`elem` paragraphTags) . tagName

    isBreak :: Tag -> Bool
    isBreak = ("br" == ) . tagName


buildParas :: [Tag] -> [Paragraph]
buildParas tags = map mkPara (groupTags tags)
  where
    mkPara :: ([Text], [Tag]) -> Paragraph
    mkPara (path, tags) =
      let text = normalizeWhitespace (T.strip (foldl' appendText "" tags))
          tagsInsideLink = filter (pathContains ["a"]) tags
          linkCharCount = foldl' addCharCount 0 tagsInsideLink
          linkDensity = density linkCharCount (T.length text)
      in Paragraph text linkDensity linkCharCount Bad path (T.length text)
          
    appendText :: Text -> Tag -> Text
    appendText text = (text <>) . getText

    addCharCount :: Int -> Tag -> Int
    addCharCount count = (count + ) . T.length . getText

    density :: Int -> Int -> Float
    density _ 0 = 0
    density a b = fromIntegral a / fromIntegral b


classifyPara :: Params -> [Text] -> Paragraph -> Paragraph
classifyPara params stopWords para
  | linkDensity' > maxLinkDensity' = badPara
  | containsCopyrightGlyph text' = badPara
  | length' < lengthLow' =
      if charCountInLinks' > 0
        then badPara
        else shortPara
  | stopWordDensity' >= stopWordsHigh' =
      if length' > lengthHigh'
        then goodPara
        else nearGoodPara
  | stopWordDensity' >= stopWordsLow' = nearGoodPara
  | otherwise = badPara
  where
    linkDensity' = linkDensity para
    text' = text para
    length' = T.length (text para)
    charCountInLinks' = charCountInLinks para
    stopWordDensity' = stopWordDensity stopWords para

    containsCopyrightGlyph :: Text -> Bool
    containsCopyrightGlyph text =
      contains '\xa9' text || T.count "&copy;" text > 0
    
    maxLinkDensity' = maxLinkDensity params
    lengthLow' = lengthLow params
    lengthHigh' = lengthHigh params
    stopWordsLow' = stopWordsLow params
    stopWordsHigh' = stopWordsHigh params
    
    goodPara = para { classType = Good }
    nearGoodPara = para { classType = NearGood }    
    badPara = para { classType = Bad }
    shortPara = para { classType = Short }

    stopWordDensity :: [Text] -> Paragraph -> Float
    stopWordDensity stopWords para' =
      let words = concat $ T.split (== ' ') <$> T.lines (text para')
          matches = filter (`elem` stopWords) words
      in fromIntegral (length matches) / fromIntegral (length words)


prevBaseNeighborType :: Int -> [Paragraph] -> ParaClass
prevBaseNeighborType i =
  maybe Bad classType .
     find ((`elem` [Good, Bad]) . classType) . prevNeighbors i
  
nextBaseNeighborType :: Int -> [Paragraph] -> ParaClass
nextBaseNeighborType i =
  maybe Bad classType .
     find ((`elem` [Good, Bad]) . classType) . nextNeighbors i

prevNeighborType :: Int -> [Paragraph] -> ParaClass
prevNeighborType i =
  maybe Bad classType . index 0 . prevNeighbors i

nextNeighborType :: Int -> [Paragraph] -> ParaClass
nextNeighborType i =
  maybe Bad classType . index 0 . nextNeighbors i

prevNeighbors :: Int -> [Paragraph] -> [Paragraph]
prevNeighbors i = reverse . take (i - 1)

nextNeighbors :: Int -> [Paragraph] -> [Paragraph]
nextNeighbors i = drop (i + 1)

index :: Int -> [a] -> Maybe a
index i xs | i < length xs = Just (xs !! i)
index _ _ = Nothing

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0 .. length xs] xs

isBoilerPlate :: Paragraph -> Bool
isBoilerPlate = (== Good) . classType

isHeading :: Paragraph -> Bool
isHeading para =
  any (isJust . R.find (R.regex [] "h\\d")) (path para)

getHeadingTag :: [Text] -> Maybe Text
getHeadingTag = find (isJust . R.find (R.regex [] "h\\d"))

findGoodParaWithin :: Int -> [Paragraph] -> Maybe Paragraph
findGoodParaWithin _ [] = Nothing
findGoodParaWithin dist _ | dist <= 0 = Nothing
findGoodParaWithin dist (p : ps) =
  if classType p == Good
    then Just p
    else findGoodParaWithin (dist - T.length (text p)) ps

reviseShortHeadings :: Params -> [Paragraph] -> [Paragraph]
reviseShortHeadings params paras =
  map reviseShortHeadings' (enumerate paras)
  where
    reviseShortHeadings' :: (Int, Paragraph) -> Paragraph
    reviseShortHeadings' (i, para) =
      if classType para == Short && isHeading para
         then revise i para
         else para

    revise :: Int -> Paragraph -> Paragraph
    revise i p =
      let nextParas = nextNeighbors i paras
          goodPara = findGoodParaWithin (maxHeadingDistance params) nextParas
      in if isJust goodPara
            then p { classType = NearGood }
            else p

reviseHeadings :: Params -> [Paragraph] -> [Paragraph] -> [Paragraph]
reviseHeadings params paras revisedParas =
  map reviseHeadings' (enumerate revisedParas)
  where
    reviseHeadings' :: (Int, Paragraph) -> Paragraph
    reviseHeadings' (i, revisedPara) =
      let beforeRevision = classType (paras !! i)
          afterRevision = classType revisedPara
      in if isHeading revisedPara && beforeRevision /= Bad && afterRevision == Bad
         then revise i revisedPara
         else revisedPara

    revise :: Int -> Paragraph -> Paragraph
    revise i p =
      let nextParas = nextNeighbors i paras
          goodPara = findGoodParaWithin (maxHeadingDistance params) nextParas
      in if isJust goodPara
            then p { classType = Good }
            else p


reviseShort :: [Paragraph] -> [Paragraph]
reviseShort [] = []
reviseShort paras = map reviseShort' (enumerate paras)
  where
    reviseShort' :: (Int, Paragraph) -> Paragraph
    reviseShort' (i, para) =
      if classType para == Short
        then revise i para
        else para
    
    revise :: Int -> Paragraph -> Paragraph
    revise i p =
      let prevBase = prevBaseNeighborType i paras
          nextBase = nextBaseNeighborType i paras
          prev = prevNeighborType i paras
          next = nextNeighborType i paras
      in case (prevBase, nextBase) of
        (Good, Good) -> p { classType = Good }
        (Bad, Bad) -> p { classType = Bad }
        (Bad, Good) | prev == NearGood -> p { classType = Good }
        (Good, Bad) | next == NearGood -> p { classType = Good }
        _ -> p { classType = Bad }

reviseNearGood :: [Paragraph] -> [Paragraph]
reviseNearGood paras = map reviseNearGood' (enumerate paras)
  where
    reviseNearGood' :: (Int, Paragraph) -> Paragraph
    reviseNearGood' (i, para) =
      if classType para == NearGood
         then revise i para
         else para

    revise :: Int -> Paragraph -> Paragraph
    revise i p =
      let prevBase = prevBaseNeighborType i paras
          nextBase = nextBaseNeighborType i paras
      in case (prevBase, nextBase) of
           (Bad, Bad) -> p { classType = Bad }
           _ -> p { classType = Good }


reviseClassification :: Params -> [Paragraph] -> [Paragraph]
reviseClassification params paras =
  reviseHeadings params paras
    $ reviseNearGood
    $ reviseShort
    $ reviseShortHeadings params paras

preProcess :: [Tag] -> [Tag]
preProcess = filter (not . exclude)
  where
    exclude :: Tag -> Bool
    exclude tag@(Tag _ tok) =
      pathContains stripTags tag || isComment tok || isDoctype tok
      
    isComment :: Token -> Bool
    isComment (Comment _) = True
    isComment _ = False

    isDoctype :: Token -> Bool
    isDoctype (Doctype _) = True
    isDoctype _ = False

createTags :: [Token] -> [Tag]
createTags = snd . foldl' build ([], [])
  where
    build :: ([Text], [Tag]) -> Token -> ([Text], [Tag])
    build (path, tags) tok =
      let tags' = tags ++ [Tag path tok] in
      case tok of
        TagOpen t _ | not (isNoClose t) -> (path ++ [t], tags')
        TagClose t -> (popTill t path, tags')
        _ -> (path, tags')
    
    isNoClose :: Text -> Bool
    isNoClose = (`elem` noCloseTags)

    popTill :: Text -> [Text] -> [Text]
    popTill tagName path =
      case dropWhileEnd (tagName /=) path of
        [] -> []
        path' -> init path'

parseHtml :: Text -> [Tag]
parseHtml = createTags . parseTokens

justext :: Text -> Params -> [Text] -> [Paragraph]
justext html params stopWords =
  let tags = preProcess (parseHtml html)
      paras = discardEmpty (buildParas tags)
      classifiedParas = classifyPara params stopWords <$> paras
      revisedParas = reviseClassification params classifiedParas
  in revisedParas
  where
    discardEmpty :: [Paragraph] -> [Paragraph]
    discardEmpty = filter (isNothing . R.find (R.regex [] "^[\n ]*$") . text)
  
