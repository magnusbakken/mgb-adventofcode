{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Data.Char
import Numeric
import System.IO
import Text.Parsec
import Text.Parsec.Combinator

data CharType = SingleChar | EscapedChar | EscapedHexSequence deriving (Eq, Show, Read)

pString f g h = between (string "\"") (string "\"") (stringData f g h)

stringData f g h = many (f <$> noneOf ['\\', '"'] <|> escapedChar g h)

escapedChar g h = char '\\' *> choice [g <$> char '"', g <$> char '\\', hexEscape h]

hexEscape h = do
  char 'x'
  n1 <- hexDigit
  n2 <- hexDigit
  let ((num, _):_) = readHex [n1, n2]
  return (h num)

parseString :: (Char -> a) -> (Char -> a) -> (Int -> a) -> String -> [a]
parseString f g h s = case parse (pString f g h) ("ParseString: " ++ s) s of
                        Left err -> error (show err)
                        Right r -> r

countCode :: String -> Int
countCode = length

countChars :: String -> Int
countChars = length . parseString id id chr

countEncoded :: String -> Int
countEncoded s = 6 + sum (map nValue (parse s)) where
    parse = parseString (const SingleChar) (const EscapedChar) (const EscapedHexSequence)
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
