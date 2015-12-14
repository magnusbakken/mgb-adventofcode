{-# LANGUAGE ViewPatterns #-}
import Control.Applicative
import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Word
import AdventOfCodeUtils (replaceIdx)

type Wire = String
type Wires = M.Map Wire Word16

data Input = WireInput Wire | ConstInput Word16

data Instruction =
    Store Input Wire |
    And Input Input Wire |
    Or Input Input Wire |
    LeftShift Input Int Wire |
    RightShift Input Int Wire |
    Not Input Wire

input :: [Instruction]
input = [makeAnd1 "af" "ah" "ai",
    makeNot1 "lk" "ll",
    makeRightShift1 "hz" 1 "is",
    makeNot1 "go" "gp",
    makeOr1 "du" "dt" "dv",
    makeRightShift1 "x" 5 "aa",
    makeOr1 "at" "az" "ba",
    makeLeftShift1 "eo" 15 "es",
    makeOr1 "ci" "ct" "cu",
    makeRightShift1 "b" 5 "f",
    makeOr1 "fm" "fn" "fo",
    makeNot1 "ag" "ah",
    makeOr1 "v" "w" "x",
    makeAnd1 "g" "i" "j",
    makeLeftShift1 "an" 15 "ar",
    makeAnd2 1 "cx" "cy",
    makeAnd1 "jq" "jw" "jy",
    makeRightShift1 "iu" 5 "ix",
    makeAnd1 "gl" "gm" "go",
    makeNot1 "bw" "bx",
    makeRightShift1 "jp" 3 "jr",
    makeAnd1 "hg" "hh" "hj",
    makeAnd1 "bv" "bx" "by",
    makeOr1 "er" "es" "et",
    makeOr1 "kl" "kr" "ks",
    makeRightShift1 "et" 1 "fm",
    makeAnd1 "e" "f" "h",
    makeLeftShift1 "u" 1 "ao",
    makeRightShift1 "he" 1 "hx",
    makeAnd1 "eg" "ei" "ej",
    makeAnd1 "bo" "bu" "bw",
    makeOr1 "dz" "ef" "eg",
    makeRightShift1 "dy" 3 "ea",
    makeOr1 "gl" "gm" "gn",
    makeLeftShift1 "da" 1 "du",
    makeOr1 "au" "av" "aw",
    makeOr1 "gj" "gu" "gv",
    makeOr1 "eu" "fa" "fb",
    makeOr1 "lg" "lm" "ln",
    makeOr1 "e" "f" "g",
    makeNot1 "dm" "dn",
    makeNot1 "l" "m",
    makeOr1 "aq" "ar" "as",
    makeRightShift1 "gj" 5 "gm",
    makeAnd1 "hm" "ho" "hp",
    makeLeftShift1 "ge" 15 "gi",
    makeRightShift1 "jp" 1 "ki",
    makeOr1 "hg" "hh" "hi",
    makeLeftShift1 "lc" 1 "lw",
    makeOr1 "km" "kn" "ko",
    makeLeftShift1 "eq" 1 "fk",
    makeAnd2 1 "am" "an",
    makeRightShift1 "gj" 1 "hc",
    makeAnd1 "aj" "al" "am",
    makeAnd1 "gj" "gu" "gw",
    makeAnd1 "ko" "kq" "kr",
    makeOr1 "ha" "gz" "hb",
    makeOr1 "bn" "by" "bz",
    makeOr1 "iv" "jb" "jc",
    makeNot1 "ac" "ad",
    makeOr1 "bo" "bu" "bv",
    makeAnd1 "d" "j" "l",
    makeLeftShift1 "bk" 1 "ce",
    makeOr1 "de" "dk" "dl",
    makeRightShift1 "dd" 1 "dw",
    makeAnd1 "hz" "ik" "im",
    makeNot1 "jd" "je",
    makeRightShift1 "fo" 2 "fp",
    makeLeftShift1 "hb" 1 "hv",
    makeRightShift1 "lf" 2 "lg",
    makeRightShift1 "gj" 3 "gl",
    makeOr1 "ki" "kj" "kk",
    makeNot1 "ak" "al",
    makeOr1 "ld" "le" "lf",
    makeRightShift1 "ci" 3 "ck",
    makeAnd2 1 "cc" "cd",
    makeNot1 "kx" "ky",
    makeOr1 "fp" "fv" "fw",
    makeAnd1 "ev" "ew" "ey",
    makeLeftShift1 "dt" 15 "dx",
    makeNot1 "ax" "ay",
    makeAnd1 "bp" "bq" "bs",
    makeNot1 "ii" "ij",
    makeAnd1 "ci" "ct" "cv",
    makeOr1 "iq" "ip" "ir",
    makeRightShift1 "x" 2 "y",
    makeOr1 "fq" "fr" "fs",
    makeRightShift1 "bn" 5 "bq",
    makeStore2 0 "c",
    makeStore2 14146 "b",
    makeOr1 "d" "j" "k",
    makeOr1 "z" "aa" "ab",
    makeOr1 "gf" "ge" "gg",
    makeOr1 "df" "dg" "dh",
    makeNot1 "hj" "hk",
    makeNot1 "di" "dj",
    makeLeftShift1 "fj" 15 "fn",
    makeRightShift1 "lf" 1 "ly",
    makeAnd1 "b" "n" "p",
    makeOr1 "jq" "jw" "jx",
    makeAnd1 "gn" "gp" "gq",
    makeRightShift1 "x" 1 "aq",
    makeAnd1 "ex" "ez" "fa",
    makeNot1 "fc" "fd",
    makeOr1 "bj" "bi" "bk",
    makeRightShift1 "as" 5 "av",
    makeLeftShift1 "hu" 15 "hy",
    makeNot1 "gs" "gt",
    makeAnd1 "fs" "fu" "fv",
    makeAnd1 "dh" "dj" "dk",
    makeAnd1 "bz" "cb" "cc",
    makeRightShift1 "dy" 1 "er",
    makeOr1 "hc" "hd" "he",
    makeOr1 "fo" "fz" "ga",
    makeOr1 "t" "s" "u",
    makeRightShift1 "b" 2 "d",
    makeNot1 "jy" "jz",
    makeRightShift1 "hz" 2 "ia",
    makeAnd1 "kk" "kv" "kx",
    makeAnd1 "ga" "gc" "gd",
    makeLeftShift1 "fl" 1 "gf",
    makeAnd1 "bn" "by" "ca",
    makeNot1 "hr" "hs",
    makeNot1 "bs" "bt",
    makeRightShift1 "lf" 3 "lh",
    makeAnd1 "au" "av" "ax",
    makeAnd2 1 "gd" "ge",
    makeOr1 "jr" "js" "jt",
    makeAnd1 "fw" "fy" "fz",
    makeNot1 "iz" "ja",
    makeLeftShift1 "c" 1 "t",
    makeRightShift1 "dy" 5 "eb",
    makeOr1 "bp" "bq" "br",
    makeNot1 "h" "i",
    makeAnd2 1 "ds" "dt",
    makeAnd1 "ab" "ad" "ae",
    makeLeftShift1 "ap" 1 "bj",
    makeAnd1 "br" "bt" "bu",
    makeNot1 "ca" "cb",
    makeNot1 "el" "em",
    makeLeftShift1 "s" 15 "w",
    makeOr1 "gk" "gq" "gr",
    makeAnd1 "ff" "fh" "fi",
    makeLeftShift1 "kf" 15 "kj",
    makeAnd1 "fp" "fv" "fx",
    makeOr1 "lh" "li" "lj",
    makeRightShift1 "bn" 3 "bp",
    makeOr1 "jp" "ka" "kb",
    makeOr1 "lw" "lv" "lx",
    makeAnd1 "iy" "ja" "jb",
    makeOr1 "dy" "ej" "ek",
    makeAnd2 1 "bh" "bi",
    makeNot1 "kt" "ku",
    makeOr1 "ao" "an" "ap",
    makeAnd1 "ia" "ig" "ii",
    makeNot1 "ey" "ez",
    makeRightShift1 "bn" 1 "cg",
    makeOr1 "fk" "fj" "fl",
    makeOr1 "ce" "cd" "cf",
    makeAnd1 "eu" "fa" "fc",
    makeOr1 "kg" "kf" "kh",
    makeAnd1 "jr" "js" "ju",
    makeRightShift1 "iu" 3 "iw",
    makeAnd1 "df" "dg" "di",
    makeAnd1 "dl" "dn" "do",
    makeLeftShift1 "la" 15 "le",
    makeRightShift1 "fo" 1 "gh",
    makeNot1 "gw" "gx",
    makeNot1 "gb" "gc",
    makeLeftShift1 "ir" 1 "jl",
    makeAnd1 "x" "ai" "ak",
    makeRightShift1 "he" 5 "hh",
    makeAnd2 1 "lu" "lv",
    makeNot1 "ft" "fu",
    makeOr1 "gh" "gi" "gj",
    makeRightShift1 "lf" 5 "li",
    makeRightShift1 "x" 3 "z",
    makeRightShift1 "b" 3 "e",
    makeRightShift1 "he" 2 "hf",
    makeNot1 "fx" "fy",
    makeAnd1 "jt" "jv" "jw",
    makeOr1 "hx" "hy" "hz",
    makeAnd1 "jp" "ka" "kc",
    makeAnd1 "fb" "fd" "fe",
    makeOr1 "hz" "ik" "il",
    makeRightShift1 "ci" 1 "db",
    makeAnd1 "fo" "fz" "gb",
    makeAnd1 "fq" "fr" "ft",
    makeRightShift1 "gj" 2 "gk",
    makeOr1 "cg" "ch" "ci",
    makeLeftShift1 "cd" 15 "ch",
    makeLeftShift1 "jm" 1 "kg",
    makeAnd1 "ih" "ij" "ik",
    makeRightShift1 "fo" 3 "fq",
    makeRightShift1 "fo" 5 "fr",
    makeAnd2 1 "fi" "fj",
    makeAnd2 1 "kz" "la",
    makeAnd1 "iu" "jf" "jh",
    makeAnd1 "cq" "cs" "ct",
    makeLeftShift1 "dv" 1 "ep",
    makeOr1 "hf" "hl" "hm",
    makeAnd1 "km" "kn" "kp",
    makeAnd1 "de" "dk" "dm",
    makeRightShift1 "dd" 5 "dg",
    makeNot1 "lo" "lp",
    makeNot1 "ju" "jv",
    makeNot1 "fg" "fh",
    makeAnd1 "cm" "co" "cp",
    makeAnd1 "ea" "eb" "ed",
    makeRightShift1 "dd" 3 "df",
    makeAnd1 "gr" "gt" "gu",
    makeOr1 "ep" "eo" "eq",
    makeAnd1 "cj" "cp" "cr",
    makeOr1 "lf" "lq" "lr",
    makeLeftShift1 "gg" 1 "ha",
    makeRightShift1 "et" 2 "eu",
    makeNot1 "jh" "ji",
    makeAnd1 "ek" "em" "en",
    makeLeftShift1 "jk" 15 "jo",
    makeOr1 "ia" "ig" "ih",
    makeAnd1 "gv" "gx" "gy",
    makeAnd1 "et" "fe" "fg",
    makeAnd1 "lh" "li" "lk",
    makeAnd2 1 "io" "ip",
    makeAnd1 "kb" "kd" "ke",
    makeRightShift1 "kk" 5 "kn",
    makeAnd1 "id" "if" "ig",
    makeNot1 "ls" "lt",
    makeOr1 "dw" "dx" "dy",
    makeAnd1 "dd" "do" "dq",
    makeAnd1 "lf" "lq" "ls",
    makeNot1 "kc" "kd",
    makeAnd1 "dy" "ej" "el",
    makeAnd2 1 "ke" "kf",
    makeOr1 "et" "fe" "ff",
    makeRightShift1 "hz" 5 "ic",
    makeOr1 "dd" "do" "dp",
    makeOr1 "cj" "cp" "cq",
    makeNot1 "dq" "dr",
    makeRightShift1 "kk" 1 "ld",
    makeAnd1 "jg" "ji" "jj",
    makeOr1 "he" "hp" "hq",
    makeAnd1 "hi" "hk" "hl",
    makeAnd1 "dp" "dr" "ds",
    makeAnd1 "dz" "ef" "eh",
    makeRightShift1 "hz" 3 "ib",
    makeOr1 "db" "dc" "dd",
    makeLeftShift1 "hw" 1 "iq",
    makeAnd1 "he" "hp" "hr",
    makeNot1 "cr" "cs",
    makeAnd1 "lg" "lm" "lo",
    makeOr1 "hv" "hu" "hw",
    makeAnd1 "il" "in" "io",
    makeNot1 "eh" "ei",
    makeLeftShift1 "gz" 15 "hd",
    makeAnd1 "gk" "gq" "gs",
    makeAnd2 1 "en" "eo",
    makeNot1 "kp" "kq",
    makeRightShift1 "et" 5 "ew",
    makeAnd1 "lj" "ll" "lm",
    makeRightShift1 "he" 3 "hg",
    makeRightShift1 "et" 3 "ev",
    makeAnd1 "as" "bd" "bf",
    makeAnd1 "cu" "cw" "cx",
    makeAnd1 "jx" "jz" "ka",
    makeOr1 "b" "n" "o",
    makeAnd1 "be" "bg" "bh",
    makeAnd2 1 "ht" "hu",
    makeAnd2 1 "gy" "gz",
    makeNot1 "hn" "ho",
    makeOr1 "ck" "cl" "cm",
    makeAnd1 "ec" "ee" "ef",
    makeLeftShift1 "lv" 15 "lz",
    makeAnd1 "ks" "ku" "kv",
    makeNot1 "ie" "if",
    makeAnd1 "hf" "hl" "hn",
    makeAnd2 1 "r" "s",
    makeAnd1 "ib" "ic" "ie",
    makeAnd1 "hq" "hs" "ht",
    makeAnd1 "y" "ae" "ag",
    makeNot1 "ed" "ee",
    makeLeftShift1 "bi" 15 "bm",
    makeRightShift1 "dy" 2 "dz",
    makeRightShift1 "ci" 2 "cj",
    makeNot1 "bf" "bg",
    makeNot1 "im" "in",
    makeOr1 "ev" "ew" "ex",
    makeOr1 "ib" "ic" "id",
    makeRightShift1 "bn" 2 "bo",
    makeRightShift1 "dd" 2 "de",
    makeOr1 "bl" "bm" "bn",
    makeRightShift1 "as" 1 "bl",
    makeOr1 "ea" "eb" "ec",
    makeAnd1 "ln" "lp" "lq",
    makeRightShift1 "kk" 3 "km",
    makeOr1 "is" "it" "iu",
    makeRightShift1 "iu" 2 "iv",
    makeOr1 "as" "bd" "be",
    makeLeftShift1 "ip" 15 "it",
    makeOr1 "iw" "ix" "iy",
    makeRightShift1 "kk" 2 "kl",
    makeNot1 "bb" "bc",
    makeRightShift1 "ci" 5 "cl",
    makeOr1 "ly" "lz" "ma",
    makeAnd1 "z" "aa" "ac",
    makeRightShift1 "iu" 1 "jn",
    makeLeftShift1 "cy" 15 "dc",
    makeLeftShift1 "cf" 1 "cz",
    makeRightShift1 "as" 3 "au",
    makeOr1 "cz" "cy" "da",
    makeAnd1 "kw" "ky" "kz",
    makeStore1 "lx" "a",
    makeAnd1 "iw" "ix" "iz",
    makeAnd1 "lr" "lt" "lu",
    makeRightShift1 "jp" 5 "js",
    makeAnd1 "aw" "ay" "az",
    makeAnd1 "jc" "je" "jf",
    makeOr1 "lb" "la" "lc",
    makeNot1 "cn" "co",
    makeLeftShift1 "kh" 1 "lb",
    makeAnd2 1 "jj" "jk",
    makeOr1 "y" "ae" "af",
    makeAnd1 "ck" "cl" "cn",
    makeOr1 "kk" "kv" "kw",
    makeNot1 "cv" "cw",
    makeAnd1 "kl" "kr" "kt",
    makeOr1 "iu" "jf" "jg",
    makeAnd1 "at" "az" "bb",
    makeRightShift1 "jp" 2 "jq",
    makeAnd1 "iv" "jb" "jd",
    makeOr1 "jn" "jo" "jp",
    makeOr1 "x" "ai" "aj",
    makeAnd1 "ba" "bc" "bd",
    makeOr1 "jl" "jk" "jm",
    makeRightShift1 "b" 1 "v",
    makeAnd1 "o" "q" "r",
    makeNot1 "p" "q",
    makeAnd1 "k" "m" "n",
    makeRightShift1 "as" 2 "at"]

makeStore1 :: String -> String -> Instruction
makeStore1 source target = Store (WireInput source) target

makeStore2 :: Word16 -> String -> Instruction
makeStore2 source target = Store (ConstInput source) target

makeAnd1 :: String -> String -> String -> Instruction
makeAnd1 source1 source2 target = And (WireInput source1) (WireInput source2) target

makeAnd2 :: Word16 -> String -> String -> Instruction
makeAnd2 source1 source2 target = And (ConstInput source1) (WireInput source2) target

makeOr1 :: String -> String -> String -> Instruction
makeOr1 source1 source2 target = Or (WireInput source1) (WireInput source2) target

makeLeftShift1 :: String -> Int -> String -> Instruction
makeLeftShift1 source1 source2 target = LeftShift (WireInput source1) source2 target

makeRightShift1 :: String -> Int -> String -> Instruction
makeRightShift1 source1 source2 target = RightShift (WireInput source1) source2 target

makeNot1 :: String -> String -> Instruction
makeNot1 source target = Not (WireInput source) target

addSignal :: Wires -> Wire -> Maybe Word16 -> Maybe Wires
addSignal _ _ Nothing = Nothing
addSignal wires target (Just x) = Just $ M.insert target x wires

runInstruction :: Wires -> Instruction -> Maybe Wires
runInstruction wires ins =
    case ins of
      (Store source target) -> addSignal wires target (inputValue wires source)
      (And source1 source2 target) -> addSignal wires target (wireAnd wires source1 source2)
      (Or source1 source2 target) -> addSignal wires target (wireOr wires source1 source2)
      (LeftShift source n target) -> addSignal wires target (wireLeftShift wires source n)
      (RightShift source n target) -> addSignal wires target (wireRightShift wires source n)
      (Not source target) -> addSignal wires target (wireNot wires source)

runInstructions :: Wires -> [Instruction] -> Wires
runInstructions wires is = go wires is [] where
    go wires [] [] = wires
    go wires [] leftovers = go wires (reverse leftovers) []
    go wires (i:is) leftovers =
        case runInstruction wires i of
          Nothing -> go wires is (i:leftovers)
          Just wires' -> go wires' is leftovers

runInstructionsOnce :: Wires -> [Instruction] -> (Wires, [Instruction])
runInstructionsOnce wires is = go wires is [] where
    go wires [] leftovers = (wires, leftovers)
    go wires (i:is) leftovers =
        case runInstruction wires i of
          Nothing -> go wires is (i:leftovers)
          Just wires' -> go wires' is leftovers

inputValue :: Wires -> Input -> Maybe Word16
inputValue _ (ConstInput value) = Just value
inputValue wires (WireInput wire) = wireGet wires wire

wireGet :: Wires -> Wire -> Maybe Word16
wireGet wires wire = M.lookup wire wires

wireAnd :: Wires -> Input -> Input -> Maybe Word16
wireAnd wires source1 source2 = wireBinOp (.&.) wires source1 source2

wireOr :: Wires -> Input -> Input -> Maybe Word16
wireOr wires source1 source2 = wireBinOp (.|.) wires source1 source2

wireBinOp :: (Word16 -> Word16 -> Word16) -> Wires -> Input -> Input -> Maybe Word16
wireBinOp f wires input1 input2 = f <$> inputValue wires input1 <*> inputValue wires input2

wireLeftShift :: Wires -> Input -> Int -> Maybe Word16
wireLeftShift wires input n = fmap (\v -> shift v n) (inputValue wires input)

wireRightShift :: Wires -> Input -> Int -> Maybe Word16
wireRightShift wires input n = fmap (\v -> shiftR v n) (inputValue wires input)

wireNot :: Wires -> Input -> Maybe Word16
wireNot wires input = fmap complement (inputValue wires input)

valueOfA :: Wires -> Word16
valueOfA wires =
    case wireGet wires "a" of
      Nothing -> error "Wire a not found"
      Just x -> x

replaceB :: Word16 -> [Instruction]
replaceB newB = replaceIdx 89 (makeStore2 newB "b") input

main :: IO ()
main = do
  let result1 = valueOfA (runInstructions M.empty input)
  print result1
  print (valueOfA (runInstructions M.empty (replaceB result1)))
