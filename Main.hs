--module Main (markdownParser, convertADTHTML, getTime, convertADTHTMLVersion2) where

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..),ParseResult(..), ParseError (..))

import           Parser           (is, string, isNot, spaces, inlineSpace, 
                                  satisfy, char, int, wrappedBySameSymbol,
                                  wrappedByBracket,eof, positiveNum, 
                                  validHeading, altHeading ,
                                  manyTill, void,wrapWithSpaces,
                                  tableSeperator,
                                  failed,eof,strip, sepBy, consumeUntil,
                                  wrapWith, digit,
                                  lookAhead, oneof, trim)

import Control.Applicative ( Alternative(many, (<|>), some) ,optional)
import Control.Monad (guard)
import Data.List (intercalate)

data TextModifier =   Italic String
                    | Bold String
                    | Strike String
                    | Link String String
                    | Inline String
                    | Footnote Int
      deriving(Show, Eq)

data ADT = Empty
          |TextMod TextModifier
          |Image String String String --alt--url--caption
          |FootnoteRef Int String 
          |FreeText [ADT]
          |Normal String
          |Heading Int [ADT] -- level -- content
          |Blockquote [ADT]
          |Code String String -- languge -- content
          |Item [ADT]
          |Orderedlist [ADT]
          |Table [[ADT]] [[[ADT]]] --head--remaining row(s)
          |Markdown [ADT]     
  deriving (Show, Eq)


--Italic
textModItalic :: Parser TextModifier
textModItalic = Italic <$> wrappedBySameSymbol "_"
  
-- Bold
textModBold :: Parser TextModifier
textModBold = Bold <$> wrappedBySameSymbol "**"

-- Strike
textModStrike :: Parser TextModifier
textModStrike = Strike <$> wrappedBySameSymbol "~~"


-- Link
textModLink :: Parser TextModifier
textModLink = Link <$> wrappedByBracket "[]" <*> wrappedByBracket "()"


-- Inline code
textModInline :: Parser TextModifier
textModInline = Inline <$> (wrappedBySameSymbol "`")

textModFootnoteHelper :: Parser Int
textModFootnoteHelper = do
  _   <- inlineSpace *> string "[^"
  num <- positiveNum
  _   <- is ']'
  return num
  
-- Footnote 
textModFootnote :: Parser TextModifier
textModFootnote = Footnote <$> textModFootnoteHelper

-- Text modifier parser
textModParser :: Parser TextModifier
textModParser = textModItalic 
            <|> textModBold 
            <|> textModStrike
            <|> textModLink 
            <|> textModInline 
            <|> textModFootnote

-- Image
imageParser :: Parser ADT
imageParser = do
  _       <- inlineSpace *> is '!'
  alt     <- wrappedByBracket "[]"
  url     <- inlineSpace *>  is '(' *> some(isNot ' ') <* spaces
  caption <- wrappedBySameSymbol "\"" <* inlineSpace <* is ')'
  _ <- lookAhead(string "\n" <|> (eof *> pure ""))
  return (Image alt url caption)

-- Footnote Reference
footnoteRefParser :: Parser ADT
footnoteRefParser = do
    num <- textModFootnoteHelper -- Reuse
    _   <- inlineSpace *> is ':' *> inlineSpace
    text<- many(isNot '\n')
    return (FootnoteRef num text)


-- Free text helper function
-- Using recursion
freetextHelper :: Parser [ADT]
freetextHelper = do
  freeText  <- some(Normal <$> consumeUntil textModParser "" 
                <|> TextMod <$> textModParser             )

  rest      <-  (eof *> pure []) -- end of input
            <|> (lookAhead (inlineSpace *> is '\n') *> pure [])-- encounter newline 
            <|> freetextHelper -- else continue consuming

  return (freeText ++ rest)

-- Free text parser
freeTextParser :: Parser ADT
freeTextParser = FreeText <$> freetextHelper

-- Heading parser
headingParser :: Parser ADT
headingParser = 
  Heading <$> (validHeading) <*> (is ' ' *> inlineSpace *> freetextHelper)


-- Alternative heading parser
altHeadingParser :: Parser ADT
altHeadingParser = do
  text  <- freetextHelper 
  level <- is '\n' *> (altHeading '=' 1 <|> altHeading '-' 2)
  return $ Heading level text

-- Block quote helper
-- remove the spaces before and after the '>'
blockquoteHelper :: Parser String
blockquoteHelper = inlineSpace *> is '>' *> inlineSpace

-- Block quote parser
blockquoteParser :: Parser ADT
blockquoteParser = 
  Blockquote <$> 
  (blockquoteHelper *> 
  sepBy(inlineSpace *> freeTextParser)(is '\n' *> blockquoteHelper))

-- Inline code helper
-- Remove the white spaces before the backticks
codeHelper:: Parser String
codeHelper = inlineSpace *> string "```" 

-- Inline code parser
codeParser :: Parser ADT
codeParser = 
  Code <$> (codeHelper *> many(isNot '\n')) 
  <*> (is '\n' *> manyTill (char)(validClosingBackTicks))
  where 
    validClosingBackTicks = 
      is '\n' *> codeHelper*> inlineSpace *> (void(is '\n') <|> eof)

-- Order list indentation checking function
orderListIndetation :: Int -> Parser ()
orderListIndetation level = do
    s <- inlineSpace
    guard(length s ==4*level)

-- Order list each line parser
-- Check the number and the spaces before the number
orderListEachLineParser :: Int-> Int -> Parser [ADT]
orderListEachLineParser start level  = do 
    orderListIndetation level
    num <- positiveNum -- the number of item
    -- start = the number of item it should be 
    guard(start == 1 && num == start || start /= 1) -- if it is the first item, num should be one
                                                    -- else as long as it is greater than 0
    _   <- string ". " *> inlineSpace
    freetextHelper

-- Ordered list helper functions
-- Concat the sub orderlist and next item 
orderedListHelper :: Int -> Int -> Parser [ADT]
orderedListHelper start level = do
  current <- (orderListEachLineParser start level)  <* inlineSpace 
  sub     <- is '\n' *> orderedListHelper 1 (level+1)       <|> pure []
  next    <- is '\n' *> orderedListHelper (start + 1) level <|> pure []
  return $ Item (current <> [Orderedlist sub | not (null sub)]) : next 

-- Order list parser
-- Tail- recursion
orderListParser :: Parser ADT
orderListParser = Orderedlist <$> (orderedListHelper 1 0)


-- Table each row parser
-- Use sepBy
tableEachRowParser :: Parser [[ADT]]
tableEachRowParser = 
  inlineSpace *> is '|' *> inlineSpace *> 
  sepBy(trying)(inlineSpace *> is '|' <* inlineSpace) <* inlineSpace <* is '|' 

  where 
    trying = do
      freeText  <- some(  Normal <$> (consumeUntil textModParser "|") 
                      <|> TextMod <$> textModParser)
      rest      <-(lookAhead (inlineSpace *> oneof "\n|") *> pure []) 
                <|> trying
      return (freeText ++ rest)

-- Table helper function
-- Get the rows of the table,except the first row
tableHelper :: Int -> Parser [[[ADT]]]
tableHelper len = do -- len = number of column
  eachRow <- tableEachRowParser
  guard(len == length eachRow) -- fail if number of column does not match
  next <-(inlineSpace *> is '\n' *>  tableHelper len) <|> pure []
  return (eachRow : next)

-- Table parser
tableParser :: Parser ADT
tableParser = do
  head      <- tableEachRowParser <* inlineSpace <* is '\n' -- head of table
  seperator <- tableSeperator 0 -- get the number of column
  guard(length head == seperator) -- check number of column matches or not
  otherRows <- tableHelper (length head) -- get the other rows
  return (Table head otherRows)

-- Markdown helper function
-- Combine all the parsers
markdownHelper :: Parser ADT
markdownHelper =  imageParser
              <|> footnoteRefParser
              <|> headingParser
              <|> altHeadingParser
              <|> blockquoteParser
              <|> codeParser
              <|> orderListParser
              <|> tableParser
              <|> freeTextParser
              <|> pure Empty

-- Markdown parser
-- remove the leading and trailing whitespace
-- Then convert it to ADT
markdownParser ::Parser ADT
markdownParser = Markdown <$> (trim *> sepBy(markdownHelper)(is '\n'))

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- Convert the text modifier ADT to html 
-- Add some tags before and after the string
-- wrapWith (tags) (content)
textModConvert :: ADT -> String
textModConvert (TextMod (Italic a))       = wrapWith "em" a

textModConvert (TextMod (Bold a))         = wrapWith "strong" a

textModConvert (TextMod (Strike a))       = wrapWith "del" a

textModConvert (TextMod (Link text url))  = 
                                "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

textModConvert (TextMod (Inline a))       = wrapWith "code" a

textModConvert (TextMod (Footnote n))     = wrapWith "sup" content

  where 
    content = "<a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n 
                    ++ "\">" ++ show n ++ "</a>"

textModConvert (Normal a) = a
textModConvert _          = ""

-- Convert image adt to html
imageConvert :: ADT -> String
imageConvert (Image alt url caption) =  
                          "<img src=\"" ++ url ++ "\" alt=\""++ alt ++ "\"" ++
                          " title=\"" ++ caption ++"\">"
imageConvert _ = ""                                 

-- Convert the footnote reference adt to html
footnoteRefConvert :: ADT -> String
footnoteRefConvert (FootnoteRef num text) = 
                          "<p id=\"fn"++ show num ++"\">" ++ text ++ "</p>"
footnoteRefConvert _ = ""                                          

-- Helper function to convert the free text adt to html
-- FreeText adt is built by text modifier and Normal ADTs
-- So we just apply text modifier converters to all items of freetext adts
-- After that, we use intercalate to combine the items to become a string
freeTextConvertHelper :: [ADT] -> String
freeTextConvertHelper lst = intercalate "" (textModConvert <$> lst)

-- Convert the freeText ADT to html
-- Using the helper function, then add p tag after and before the string
freeTextConvert :: ADT -> String
freeTextConvert (FreeText lst) = wrapWith "p" (freeTextConvertHelper lst)
freeTextConvert _ = ""

-- Convert the heading ADT to html
-- Since heading contains free text,we resue the freetext helper functions
-- Then add h tags before and after the string
headingConvert :: ADT -> String
headingConvert (Heading num lst) =  
                    wrapWith ("h" ++ show num )(freeTextConvertHelper lst)
headingConvert _ = ""

-- Convert blockquote ADT to html
-- Since blockquote ADT contains free text, so we reuse free text converter
-- After that,we add some spaces and tags before and after the string 
blockquoteConvert :: ADT -> String
blockquoteConvert (Blockquote lstInLst)  = 
 wrapWith "blockquote" (whiteSpaces2 ++ content ++ whiteSpaces1)
  where
    whiteSpaces1 = "\n    "
    whiteSpaces2 = "\n        "
    content = intercalate whiteSpaces2 (freeTextConvert <$> lstInLst)
blockquoteConvert _ = ""

-- Convert the code ADT to html
-- Pattern matching: 
-- empty language , no need to write the language name
-- non-empty language, need different tags     
codeConvert :: ADT -> String
codeConvert (Code "" text) = wrapWith "pre" (wrapWith "code" text)
codeConvert (Code language text) = 
  wrapWith "pre" ("<code"++check++text++"</code>")
  where 
    check = " class=\"language-"++language++"\">"
codeConvert _ = ""

-- Helper function for converting orderlist adt to html
-- Convert each item of order list to html
orderListhelper :: Int -> ADT -> String
orderListhelper num (Item t) =
  l ++ wrapWith "li" (intercalate "\n" ((orderListanotherHelper (num+1)) <$> t))
  where l = replicate( (num+1)*4) (' ')
orderListhelper _ _ = ""

-- Helper function for converting orderlist item to html
-- Pattern matching:
-- Orderlist : Call nother functions to convert the sub orderlist
-- others:  Use textModConverter
orderListanotherHelper :: Int -> ADT -> String
orderListanotherHelper  num (Orderedlist ol) =   
  (orderListConvertHelper  (num) (Orderedlist ol) ) ++
  "\n" ++ replicate(num*4)(' ')
orderListanotherHelper _  a = textModConvert a 

-- Helper function for converting an entire order list to html
orderListConvertHelper :: Int -> ADT -> String
orderListConvertHelper num (Orderedlist ol) = 
   l num ++ 
   wrapWith "ol" ((wrapWithSpaces "\n" lst) ++ le )
  where 
    lst = intercalate "\n" ((orderListhelper (num+1) )<$> ol )
    l :: Int -> String
    l 0 = ""
    l num = replicate(4*(num+1))(' ')
    le = replicate((num+1)*4)(' ')

orderListConvertHelper _ _ = ""

-- Order list parser
-- Call the order list helper function with 0, means level 0
orderListConvert :: ADT -> String
orderListConvert = orderListConvertHelper 0

-- Convert the Table ADT's head to html
headConvert :: Int -> String-> [[ADT]] -> String
headConvert num tag nstLst =
  l ++ wrapWith "tr" ((wrapWithSpaces "\n" lst) ++ l)

  where 
    l =  replicate (num*4)(' ')
    lst = intercalate "\n" ((tbContentConvert (num+1) tag) <$> nstLst)

-- Convert the row of Table ADT to html
tbContentConvert :: Int -> String-> [ADT] -> String
tbContentConvert num tag lst =
  l ++ wrapWith tag (strip(freeTextConvertHelper(lst)))
  where l = replicate (num*4)(' ')

-- Convert the entire Table ADT to html
tableConvertHelper :: Int -> ADT -> String
tableConvertHelper num (Table head rest) =
   wrapWith "table" (wrapWithSpaces "\n"  (header ++ "\n" ++ otherRows) ++ l)

  where 
    l = replicate (num*4)(' ')
    header = headConvert (num+1) "th" head
    otherRows = (intercalate "\n" ((headConvert (num+1) "td") <$> rest))

tableConvertHelper _ _ = ""

-- Convert Table ADT to html
tableConvert :: ADT -> String
tableConvert = tableConvertHelper 1

-- Combine all converters
finalConverter :: ADT -> String
finalConverter (Image alt url caption) = imageConvert (Image alt url caption)
finalConverter (FootnoteRef num text) = footnoteRefConvert (FootnoteRef num text)
finalConverter (FreeText lst) = freeTextConvert (FreeText lst)
finalConverter (Heading num lst) = headingConvert (Heading num lst)
finalConverter (Blockquote lstInLst) = blockquoteConvert (Blockquote lstInLst)
finalConverter (Code language text) = codeConvert (Code language text)
finalConverter (Orderedlist ol) = orderListConvert (Orderedlist ol)
finalConverter (Table head rest) = tableConvert (Table head rest)
finalConverter Empty = wrapWith "p" ""
finalConverter _ = ""

-- Convert the ADT to completed html (without title)
convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (Markdown lst) =
  upper ++ "    "++
  intercalate "\n    " (finalConverter <$> lst) ++ bottom
  where
    upper =   "<!DOCTYPE html>\n"
        ++"<html lang=\"en\">\n\n"
        ++"<head>\n"
        ++"    <meta charset=\"UTF-8\">\n"
        ++"    <title>Test</title>\n"
        ++  "</head>\n\n"
            ++"<body>\n"

    bottom = "\n</body>\n\n</html>\n"
convertADTHTML _ =""

-- Convert the ADT to completed html (with title)
convertADTHTMLVersion2 :: String -> ADT -> String
convertADTHTMLVersion2 title (Markdown lst) =
   upper ++ title ++ "</title>\n"++
        middle ++ "    "++
        intercalate "\n    " (finalConverter <$> lst) ++ bottom
  where
    upper =   "<!DOCTYPE html>\n"
        ++"<html lang=\"en\">\n\n"
        ++"<head>\n"
        ++"    <meta charset=\"UTF-8\">\n"
        ++"    <title>"
    middle = "</head>\n\n"
            ++"<body>\n"

    bottom = "\n</body>\n\n</html>\n"
convertADTHTMLVersion2 _ _ = ""
