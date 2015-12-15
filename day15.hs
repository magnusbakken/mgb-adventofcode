import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import AdventOfCodeUtils (annotate)

data Ingredient = Ingredient {
      name :: String,
      capacity :: Int,
      durability :: Int,
      flavor :: Int,
      texture :: Int,
      calories :: Int
    } deriving (Eq, Ord)

instance Show Ingredient where
    show = name

type Recipe = M.Map Ingredient Int

input :: [Ingredient]
input = [Ingredient "Sprinkles" 2 0 (-2) 0 3,
         Ingredient "Butterscotch" 0 5 (-3) 0 3,
         Ingredient "Chocolate" 0 0 5 (-1) 8,
         Ingredient "Candy" 0 (-1) 0 5 8]

recipes :: [Ingredient] -> [Recipe]
recipes = go M.empty 0 where
    go recipe _ [] = [recipe]
    go recipe n (x:[]) = [M.insert x (100-n) recipe]
    go recipe n (x:xs) = [0..(100-n)] >>= \i -> go (M.insert x i recipe) (n+i) xs

componentScore :: (Ingredient -> Int) -> Recipe -> Int
componentScore f r = max 0 (M.foldlWithKey' componentFunc 0 r) where
    componentFunc x i n = x + (f i * n)

recipeScore :: Recipe -> Int
recipeScore r = capacities * durabilities * flavors * textures where
    capacities = componentScore capacity r
    durabilities = componentScore durability r
    flavors = componentScore flavor r
    textures = componentScore texture r

sortByScore :: [(Recipe, Int)] -> [(Recipe, Int)]
sortByScore = L.sortBy (\(_, x) (_, y) -> y `compare` x)

bestRecipeWithFilter :: (Recipe -> Bool) -> [Ingredient] -> Maybe (Recipe, Int)
bestRecipeWithFilter f = listToMaybe . sortByScore . annotate recipeScore . filter f . recipes
                        
bestRecipe :: [Ingredient] -> Maybe (Recipe, Int)
bestRecipe = bestRecipeWithFilter (const True)

bestRecipeWithCalories :: Int -> [Ingredient] -> Maybe (Recipe, Int)
bestRecipeWithCalories n = bestRecipeWithFilter (\r -> componentScore calories r == n)

bestScore :: [Ingredient] -> Int
bestScore is = case bestRecipe is of
                 Nothing -> error "Unable to find best recipe"
                 Just (_, n) -> n

bestScoreWithCalories :: Int -> [Ingredient] -> Int
bestScoreWithCalories n is = case bestRecipeWithCalories n is of
                 Nothing -> error "Unable to find best recipe"
                 Just (_, n) -> n

main :: IO ()
main = do
  print (bestScore input)
  print (bestScoreWithCalories 500 input)
