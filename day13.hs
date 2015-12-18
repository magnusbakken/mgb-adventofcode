import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

type Person = String
type Seating = [Person]
type Pairing = (Person, Person)
type Happiness = Int
type Preferences = M.Map Pairing Happiness

a, b, c, d, e, f, g, m :: Person
a = "Alice"
b = "Bob"
c = "Carol"
d = "David"
e = "Eric"
f = "Frank"
g = "George"
m = "Mallory"

inputPrefs :: Preferences
inputPrefs =
    M.fromList [
          ((a, b), -57),
          ((a, c), -62),
          ((a, d), -75),
          ((a, e), 71),
          ((a, f), -22),
          ((a, g), -23),
          ((a, m), -76),
          ((b, a), -14),
          ((b, c), 48),
          ((b, d), 89),
          ((b, e), 86),
          ((b, f), -2),
          ((b, g), 27),
          ((b, m), 19),
          ((c, a), 37),
          ((c, b), 45),
          ((c, d), 24),
          ((c, e), 5),
          ((c, f), -68),
          ((c, g), -25),
          ((c, m), 30),
          ((d, a), -51),
          ((d, b), 34),
          ((d, c), 99),
          ((d, e), 91),
          ((d, f), -38),
          ((d, g), 60),
          ((d, m), -63),
          ((e, a), 23),
          ((e, b), -69),
          ((e, c), -33),
          ((e, d), -47),
          ((e, f), 75),
          ((e, g), 82),
          ((e, m), 13),
          ((f, a), 77),
          ((f, b), 27),
          ((f, c), -87),
          ((f, d), 74),
          ((f, e), -41),
          ((f, g), -99),
          ((f, m), 26),
          ((g, a), -63),
          ((g, b), -51),
          ((g, c), -60),
          ((g, d), 30),
          ((g, e), -100),
          ((g, f), -63),
          ((g, m), 57),
          ((m, a), -71),
          ((m, b), -28),
          ((m, c), -10),
          ((m, d), 44),
          ((m, e), 22),
          ((m, f), 79),
          ((m, g), -16)]

inputPersons :: [Person]
inputPersons = [a, b, c, d, e, f, g, m]

inputPrefsWithMe :: Preferences
inputPrefsWithMe = M.union (M.fromList (pairedWithMe inputPersons)) inputPrefs where
    pairedWithMe [] = []
    pairedWithMe (x:xs) = ((x, "Me"), 0) : (("Me", x), 0) : pairedWithMe xs

inputPersonsWithMe :: [Person]
inputPersonsWithMe = "Me" : inputPersons

affinity :: Preferences -> Pairing -> Happiness
affinity pref pairing = fromMaybe 0 (M.lookup pairing pref)

pairings :: Seating -> [Pairing]
pairings [] = []
pairings [_] = []
pairings (first:rest) = duplicateAll (go first rest) where
    go curr [] = [(curr, first)]
    go curr (next:xs) = (curr, next) : go next xs
    duplicateAll [] = []
    duplicateAll (pair:pairs) = pair : swap pair : duplicateAll pairs

totalHappiness :: Preferences -> Seating -> Happiness
totalHappiness pref seating = sum $ map (affinity pref) (pairings seating)

seatings :: [Person] -> [Seating]
seatings = L.permutations

bestSeating :: Preferences -> [Person] -> Maybe (Happiness, Seating)
bestSeating pref persons =
    listToMaybe $ L.sortBy (flip compare) $ map (\x -> (totalHappiness pref x, x)) (seatings persons)

bestHappiness :: Preferences -> [Person] -> Happiness
bestHappiness pref persons =
    case bestSeating pref persons of
      Nothing -> error "No configuration found"
      Just (happiness, _) -> happiness

main :: IO ()
main = do
  print (bestHappiness inputPrefs inputPersons)
  print (bestHappiness inputPrefsWithMe inputPersonsWithMe)
