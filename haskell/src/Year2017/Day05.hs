module Year2017.Day05 ( day05 ) where

import           Data.Array

import           Types


type Idx = Int
type Step = Int
type MazePos = (Idx, [Int])
type Maze = (Idx, Array Int Int, Step)


day05 :: Mode -> AB -> String -> IO [String]
day05 mode _ input = do
  case mode of
    Test -> return [ show . testA $ input ]
    Run  -> return [ show . runA $ input ]


runA :: String -> Int
runA = (\(_,_,s) -> s) . until outOfBounds travel . buildMaze
  where
    buildMaze :: String -> Maze
    buildMaze str =
      let ns = zip [0..] . map read . words $ str
       in (0, array (0, length ns - 1) ns, 0)

testA :: String -> MazePos
testA =
  (\(i,a,_) -> (i, elems a)) . travel . buildMazePos
    where
      buildMazePos :: String -> Maze
      buildMazePos str =
        let (idx,ns) = read str
         in (idx, array (0, length ns - 1) (zip [0..] ns), 0)


travel :: Maze -> Maze
travel (from,arr,step) =
  let val = arr ! from
      arr' = arr//[(from, val+1)]
      to = from + val
   in (to, arr', step+1)

outOfBounds :: Maze -> Bool
outOfBounds (idx,arr,_) =
  let (min',max') = bounds arr
   in idx < min' || idx > max'
