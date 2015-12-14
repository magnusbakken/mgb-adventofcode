{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Data.Char
import Numeric
import System.IO
import Text.Parsec
import Text.Parsec.Combinator

pString = do
  char '"'
  result <- stringData
  char '"'
  return result

stringData = many (noneOf ['\\', '"'] <|> escapedChar)

escapedChar = do
  char '\\'
  choice [char '"', char '\\', hexEscape]

hexEscape = do
  char 'x'
  n1 <- hexDigit
  n2 <- hexDigit
  let [(num, _)] = readHex [n1, n2]
  return (chr num)

data CharType = SingleChar | EscapedChar | EscapedHexSequence deriving (Eq, Show, Read)

pStringN = do
  char '"'
  result <- stringDataN
  char '"'
  return result

stringDataN = many (SingleChar <$ noneOf ['\\', '"'] <|> escapedCharN)

escapedCharN = do
  char '\\'
  choice [EscapedChar <$ char '"', EscapedChar <$ char '\\', hexEscapeN]

hexEscapeN = do
  char 'x'
  n1 <- hexDigit
  n2 <- hexDigit
  let [(num, _)] = readHex [n1, n2]
  return EscapedHexSequence

parseString :: String -> String
parseString s = case parse pString ("ParseString: " ++ s) s of
                  Left err -> error (show err)
                  Right r -> r

parseStringN :: String -> [CharType]
parseStringN s = case parse pStringN ("ParseStringN: " ++ s) s of
                   Left err -> error (show err)
                   Right r -> r

countCode :: String -> Int
countCode = length

countChars :: String -> Int
countChars = length . parseString

countEncoded :: String -> Int
countEncoded s = 6 + sum (map nValue (parseStringN s)) where
    nValue SingleChar = 1
    nValue EscapedChar = 4
    nValue EscapedHexSequence = 5

countDiff :: String -> Int
countDiff s = countCode s - countChars s

countDiff2 :: String -> Int
countDiff2 s = countEncoded s - countCode s

readInput :: IO [String]
readInput = fmap lines (readFile "day8.txt")

main :: IO ()
main = do
  input <- readInput
  print (sum (map countDiff input))
  print (sum (map countDiff2 input))
