module Year2017.Day05 ( day05 ) where

import           Data.Array

import           Types


type Idx = Int
type Val = Int
type Step = Int
type Maze = (Idx, Array Idx Val, Step)


day05 :: Mode -> AB -> String -> IO [String]
day05 Test ab = return . return . test ab
day05 Run  ab = return . return . show . (\(_,_,s) -> s) . run ab

test :: AB -> String -> String
test A = show . (\(i,a,_) -> (i, elems a)) . travelUsing A . buildMazePos
test B = show . (\(_,a,s) -> (elems a, s)) . run B

run :: AB -> String -> Maze
run ab = until outOfBounds (travelUsing ab) . buildMaze


buildMaze :: String -> Maze
buildMaze str =
  let ns = zip [0..] . map read . words $ str
   in (0, array (0, length ns - 1) ns, 0)

buildMazePos :: String -> Maze
buildMazePos str =
  let (idx,ns) = read str
   in (idx, array (0, length ns - 1) (zip [0..] ns), 0)

travelUsing :: AB -> Maze -> Maze
travelUsing ab (from,arr,step) =
  let val = arr ! from
      to = from + val
      offset = to - from
      val' = if offset>=3 && ab == B then -1 else 1
      arr' = arr//[(from, val + val')]
   in (to, arr', step+1)

outOfBounds :: Maze -> Bool
outOfBounds (idx,arr,_) =
  let (min',max') = bounds arr
   in idx < min' || idx > max'
