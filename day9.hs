import Control.Applicative
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import AdventOfCodeUtils (sortWith)

type Node = String
type Edge = (Node, Node)
type Cost = Int
type Graph = M.Map Edge Cost
type Step = (Node, Cost)
type Journey = [Node]

input :: Graph
input = M.fromList [
    (("Faerun", "Tristram"), 65),
    (("Faerun", "Tambi"), 129),
    (("Faerun", "Norrath"), 144),
    (("Faerun", "Snowdin"), 71),
    (("Faerun", "Straylight"), 137),
    (("Faerun", "AlphaCentauri"), 3),
    (("Faerun", "Arbre"), 149),
    (("Tristram", "Tambi"), 63),
    (("Tristram", "Norrath"), 4),
    (("Tristram", "Snowdin"), 105),
    (("Tristram", "Straylight"), 125),
    (("Tristram", "AlphaCentauri"), 55),
    (("Tristram", "Arbre"), 14),
    (("Tambi", "Norrath"), 68),
    (("Tambi", "Snowdin"), 52),
    (("Tambi", "Straylight"), 65),
    (("Tambi", "AlphaCentauri"), 22),
    (("Tambi", "Arbre"), 143),
    (("Norrath", "Snowdin"), 8),
    (("Norrath", "Straylight"), 23),
    (("Norrath", "AlphaCentauri"), 136),
    (("Norrath", "Arbre"), 115),
    (("Snowdin", "Straylight"), 101),
    (("Snowdin", "AlphaCentauri"), 84),
    (("Snowdin", "Arbre"), 96),
    (("Straylight", "AlphaCentauri"), 107),
    (("Straylight", "Arbre"), 14),
    (("AlphaCentauri", "Arbre"), 46)]
    
exampleGraph :: Graph
exampleGraph = M.fromList [londonToDublin, londonToBelfast, dublinToBelfast] where
    londonToDublin = (("London", "Dublin"), 464)
    londonToBelfast = (("London", "Belfast"), 518)
    dublinToBelfast = (("Dublin", "Belfast"), 141)

allNodes :: Graph -> [Node]
allNodes graph = map head $ L.group $ L.sort $ concat $ map (\(from, to) -> [from, to]) (M.keys graph)

endsAt :: Node -> Edge -> Bool
endsAt node (_, to) = node == to

startsFrom :: Node -> Edge -> Bool
startsFrom node (from, _) = node == from

allConnected :: Graph -> Node -> Graph
allConnected graph node = M.filterWithKey (\k _ -> startsFrom node k || endsAt node k) graph

edgesFor :: Graph -> Node -> [Edge]
edgesFor graph node = M.keys $ allConnected graph node
              
stepsFor :: Graph -> Node -> [Step]
stepsFor graph node = map getStep $ M.toList $ allConnected graph node where
    getStep ((from, to), cost) = (if from == node then to else from, cost)

unseenStepsFor :: Graph -> Node -> S.Set Node -> [Step]
unseenStepsFor graph node seen = do
  (dest, cost) <- stepsFor graph node
  guard $ dest `S.notMember` seen
  return (dest, cost)

allJourneysFor :: Graph -> Node -> [Journey]
allJourneysFor graph firstNode = go firstNode (S.insert firstNode S.empty) [firstNode] [] where
    graphSize = length (allNodes graph)
    go node seen nodes journeys =
        case unseenStepsFor graph node seen of
          [] -> if length nodes == graphSize then reverse nodes : journeys else journeys
          steps -> do
            (to, _) <- steps
            go to (S.insert to seen) (to:nodes) journeys

allJourneys :: Graph -> [Journey]
allJourneys graph = allNodes graph >>= allJourneysFor graph

journeyEdges :: Journey -> [Edge]
journeyEdges journey = zip journey (drop 1 journey)

edgeCost :: Graph -> Edge -> Cost
edgeCost graph edge = fromMaybe 0 (M.lookup edge graph <|> M.lookup reverseEdge graph) where
    reverseEdge = (snd edge, fst edge)

journeyCost :: Graph -> Journey -> Cost
journeyCost graph journey = sum (map (edgeCost graph) (journeyEdges journey))

allJourneysWithCosts :: Graph -> [(Journey, Cost)]
allJourneysWithCosts graph = map (\j -> (j, journeyCost graph j)) (allJourneys graph)

bestJourney :: Graph -> Maybe (Journey, Cost)
bestJourney = listToMaybe . sortWith snd . allJourneysWithCosts

bestJourneyCost :: Graph -> Cost
bestJourneyCost graph = case bestJourney graph of
                          Nothing -> 0
                          Just (_, cost) -> cost

worstJourney :: Graph -> Maybe (Journey, Cost)
worstJourney = listToMaybe . reverse . sortWith snd . allJourneysWithCosts

worstJourneyCost :: Graph -> Cost
worstJourneyCost graph = case worstJourney graph of
                          Nothing -> 0
                          Just (_, cost) -> cost

main :: IO ()
main = do
  print (bestJourneyCost input)
  print (worstJourneyCost input)
