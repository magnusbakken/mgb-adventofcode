import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Base16 as B16
import AdventOfCodeUtils (hasPrefixN)

myInput :: String
myInput = "bgvyzdsv"

generateMD5 :: String -> Int -> BC8.ByteString
generateMD5 key n = hash (BC8.pack (key ++ show n))

md5Digest :: BC8.ByteString -> String
md5Digest = BC8.unpack . B16.encode
              
digests :: String -> [(Int, String)]
digests key = fmap (\n -> (n, md5Digest (generateMD5 key n))) [0..]

firstDigest :: (String -> Bool) -> String -> Int
firstDigest f key = fst (head (filter (f . snd) (digests key)))

main :: IO ()
main = do
  print (firstDigest (hasPrefixN 5 '0') myInput)
  print (firstDigest (hasPrefixN 6 '0') myInput)
