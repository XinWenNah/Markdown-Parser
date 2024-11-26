{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Implementation of a parser-combinator.
module Parser(is, string, isNot, spaces, inlineSpace, 
                                  satisfy, char, int, wrappedBySameSymbol,
                                  wrappedByBracket,eof, positiveNum,
                                  validHeading, altHeading ,
                                  manyTill, void, 
                                  tableSeperator,
                                  failed,strip, sepBy, consumeUntil,getTitle,
                                  wrapWith, digit,
                                  lookAhead,oneof,trim, wrapWithSpaces) where
import           Control.Applicative 
import Control.Monad (guard)
import           Data.Char           (isAlpha, isDigit, isLower, isSpace,
                                      isUpper)
import           Instances           (ParseError (..), ParseResult (..),
                                      Parser (..), readInt)                     

-- $setup
-- >>> import Instances (isErrorResult, parse)


-- | -------------------------------------------------
-- | --------------- Core parsers --------------------
-- | -------------------------------------------------

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed = Parser . const . Error

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = Parser . const . Error . UnexpectedChar

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse char "")
-- True
char :: Parser Char
char = Parser f
  where
    f ""       = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit

---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = Parser f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing        -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = Parser f
  where
    f "" = Result "" ()
    f x  = Error $ ExpectedEof x

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
  where
    -- This is okay because guards are small
    f' c
      | f c = pure c
      | otherwise = unexpectedCharParser c

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Write a function that parses one of the characters in the given string.
--
-- /Hint/: What does `elem` do? What are its parameters?
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Hint/: What does `notElem` do? What are its parameters?
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Hint/: Use the 'isDigit' function
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Hint/: Use the 'isLower' function
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Hint/: Use the 'isUpper' function
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Hint/: Use the 'isAlpha' function
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Write a parser that will parse zero or more spaces (including newlines)
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces1 " abc"
-- Result >abc< " "
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
spaces1 :: Parser String
spaces1 = some space


-- | Write a parser that will parse zero or more spaces (not including newlines)
--
-- The possible whitespace characters: \t, \r, \f, \v, and a space character.
--
-- >>> parse inlineSpace " abc"
-- Result >abc< " "
--
-- >>> parse inlineSpace "abc"
-- Result >abc< ""
inlineSpace :: Parser String
inlineSpace = many (oneof "\t\r\f\v ")

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use 'is' and 'traverse'.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Hint/: You can use the Monad instance or Applicatives
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok = (<* spaces)

-- tok p = do
--   r <- p
--   spaces
--   pure r

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string


-- |  Takes two parsers as arguments, return a parser that will keep consuming
-- using first parser and concat the results, but fail if second parser is 
-- able to consume or reach the end of string
-- FROM github copilot
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = end *> pure [] 
         <|> ((:) <$> p <*> go)

-- | Takes two parsers as arguments, return a parser that will use first parser
-- then second parser and so on, but fail if
--  * one of the input parsers fail
--  * reach the end of string
-- FROM APPLIED 9
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy a b = (:) <$> a <*> many(b *> a) <|> pure[]                                 

-- | Takes one string as argument, return a parser that will keep consuming
-- character that surrounding by the string, but it will fail if
--  * encounter newline character
--  * reach the end of string
wrappedBySameSymbol :: String -> Parser String
wrappedBySameSymbol x = string x *> manyTill (noneof "\n")(string x)

-- | Takes brackets as argument, return a parser that will keep consuming
-- character inside the bracktes, but it will fail if
--  * encounter newline character
--  * reach the end of string
--  * empty input string
wrappedByBracket :: String -> Parser String
wrappedByBracket [] = failed(UnexpectedString "END")
wrappedByBracket [_] = failed(UnexpectedString "END")
wrappedByBracket (o:c:_) = 
              inlineSpace *> is o *> manyTill (noneof "\n")(string [c])

-- | Return a parser that will return the number of the input string, but it
-- will fail if
--  * any whitespaces before the number
--  * number that less or equals to 0
positiveNum :: Parser Int
positiveNum = do
  _    <- lookAhead(digit)
  num <- inlineSpace *> int
  guard(num > 0)
  return num  

-- | Take one parser as argument, return a parser that will try the input parser
-- wihtout consuming the input string, but it will fail if
--  * input parser fails
-- FROM github copilot
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser f
  where
    f input = case p input of
      Result _ c -> Result input c
      Error e -> Error e        

-- | Take one parser and string as arguments, return a parser that will keep
-- consuming until the input parser is able to parse but without actual consume 
-- it, it will fail if 
--  * empty input string
consumeUntil :: Parser a -> String -> Parser String
consumeUntil p str = some checkNextChar
  where
    checkNextChar = do
      result <- optional p
      trueThenFailElseOther result (noneof("\n" ++ str))

-- | Take one maybe and a parser, return the input parser if the maybe is
-- nothing, it will fail if
--  * maybe is Just
trueThenFailElseOther :: Maybe a -> Parser b -> Parser b
trueThenFailElseOther (Just _) _ = failed (UnexpectedString "Invalid")
trueThenFailElseOther (Nothing) p = p

-- | Return a parser that will return the number of '#' of the input string,
-- but it will fail if 
--  * no '#'
--  * number of '#' more than 6
validHeading :: Parser Int
validHeading = do
  len <- inlineSpace *> some (is '#') 
  guard(length len <= 6)
  return (length len)

-- | Take a functor and replace the content of the functor to ()
-- FROM GITHUB COPILOT
void :: Functor f => f a -> f ()
void x = () <$ x

-- | Take one character and an integer as arguments, return a parser that will
-- return the input integer if the number of the character is more than 2, but
-- it will fail if
--  * empty input string
--  * after the newline, there are other charatcers
altHeading :: Char -> Int -> Parser Int
altHeading c n = do
    _ <- inlineSpace
    sign <- many(is c)
    _ <- inlineSpace 
    _ <- lookAhead(string "\n") <|> (eof *> pure "")
    guard(length sign >= 2)
    return n

-- | Return a parser that will reverse the input string
--FROM CHATGPT
reverseInput::Parser String
reverseInput = Parser f
  where
    f "" =  Result "" ""
    f x = Result (reverse x) ""

-- | Return a parser that will remove the leading and trailing whitespaces
-- FROM CHATGPT
trim ::Parser String
trim = reverseInput *> spaces *> reverseInput <* spaces

-- | Take an integer, return a parser that will return the number of columns
tableSeperator :: Int -> Parser Int
tableSeperator p = do
    _ <- inlineSpace *> (is '|') *> inlineSpace
    dashes <-     ((void(is '\n') <|> eof) *> pure (-1)) 
              <|> length <$> some(is '-' <* inlineSpace)
    validateDashes dashes p

-- | Take two inetegers, return a parser that will return the second integer 
-- or calling tableSeperator, but if will fail if
-- * first integer is lesser than 3 and it is not -1
validateDashes :: Int -> Int -> Parser Int
validateDashes dashes p
  | dashes < 3 && dashes /= -1 = failed (UnexpectedString "At least 3 dashes")
  | dashes == -1               = return p
  | otherwise                  = tableSeperator (p + 1)

-- | A function that will strip the input strings
-- FROM CHATGPT
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Return a parser that will consume that characters before the newline
getTitle :: Parser String
getTitle = many(consuming)
  where consuming = do
              try <- optional (inlineSpace *> is '\n') 
              trueThenFailElseOther try char

-- | A function that will return a string that wrapped with the input string
wrapWith :: String ->String -> String
wrapWith symbol content = "<"++symbol++">"++content++"</"++symbol++">"

wrapWithSpaces :: String -> String -> String
wrapWithSpaces whiteSpaces content = whiteSpaces ++ content ++ whiteSpaces
