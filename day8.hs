{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Data.Char
import Numeric
import System.IO
import Text.Parsec
import Text.Parsec.Combinator

data CharType = SingleChar | EscapedChar | EscapedHexSequence deriving (Eq, Show, Read)

pString f g h = do
  char '"'
  result <- stringData f g h
  char '"'
  return result

stringData f g h = many (f <$> noneOf ['\\', '"'] <|> escapedChar g h)

escapedChar g h = char '\\' *> (choice [g <$> char '"', g <$> char '\\', hexEscape h])

hexEscape h = do
  char 'x'
  n1 <- hexDigit
  n2 <- hexDigit
  let [(num, _)] = readHex [n1, n2]
  return (h num)

parseString :: (Char -> a) -> (Char -> a) -> (Int -> a) -> String -> [a]
parseString f g h s = case parse (pString f g h) ("ParseString: " ++ s) s of
                        Left err -> error (show err)
                        Right r -> r

countCode :: String -> Int
countCode = length

countChars :: (Char -> a) -> (Char -> a) -> (Int -> a) -> String -> Int
countChars f g h = length . parseString f g h

countEncoded :: (Char -> CharType) -> (Char -> CharType) -> (Int -> CharType) -> String -> Int
countEncoded f g h s = 6 + sum (map nValue (parseString f g h s)) where
    nValue SingleChar = 1
    nValue EscapedChar = 4
    nValue EscapedHexSequence = 5

countDiff :: String -> Int
countDiff s = countCode s - countChars id id chr s

countDiff2 :: String -> Int
countDiff2 s = countEncoded (const SingleChar) (const EscapedChar) (const EscapedHexSequence) s - countCode s

readInput :: IO [String]
readInput = fmap lines (readFile "day8.txt")

main :: IO ()
main = do
  input <- readInput
  print (sum (map countDiff input))
  print (sum (map countDiff2 input))
