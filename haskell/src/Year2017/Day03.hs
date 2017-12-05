module Year2017.Day03 ( day03 ) where

-- import           Text.Printf   (printf)
import           Data.List     (find, sortBy)
import           Control.Monad (join)

import           Types


type Coords = (Int, Int)
type Address = Int
type Val = Int
type Grid = [[Cell]]
type Cell = (Coords,Address,Val)


day03 :: Mode -> AB -> String -> IO [String]
day03 _ _ _ = do
  -- let val = read input
      -- gridSize = ceiling . sqrt $ (fromIntegral val :: Double)
      -- grid = buildGrid gridSize
      -- (Just (oneX,oneY)) = getCoordsForAddress 1 grid
      -- (Just (locX,locY)) = getCoordsForAddress val grid
      -- dist = (abs (oneX - locX)) + (abs (oneY - locY))

  let gridSize = ceiling . sqrt $ (368078 :: Double)
      grid = buildGrid gridSize
      (b,_) = populateGridAndFindFirstGreaterThan 368078 grid

  -- printCells cs
  -- print cs
  -- print b
  return . map show $ [ b ]


buildGrid :: Int -> Grid
buildGrid size =
  grid . until (\g -> gridSize g >= size) incPreGrid $ [[1]]
  where
    grid :: [[Int]] -> Grid
    grid g =
      map (\(y,r) -> map (\(x,a) -> ((x,y),a,0)) . zip [0..] $ r) . zip [0..] $ g

    gridSize :: [[a]] -> Int
    gridSize (row:_) = length row
    gridSize _ = 0

    incPreGrid :: [[Int]] -> [[Int]]
    incPreGrid g = addRow . addCols $ g
      where
        addCols =
          if odd s then reverse . map cf . reverse . zip cs
                   else map cf . zip cs
        addRow g' = rf (r,g')
        cf = if odd s then push else unshift
        rf = if odd s then unshift else push
        unshift (a,as) = a:as
        push (a,as) = as ++ [a]
        s  = gridSize g
        sq = flip (^) (2 :: Int)
        ns = [((sq s)+1)..(sq (s+1))]
        cs = (if odd s then reverse else id) . take s $ ns
        rs = drop s $ ns
        r  = if even s then rs else reverse rs

-- getCoordsForAddress :: Address -> Grid -> Maybe Coords
-- getCoordsForAddress addr =
--   fmap (\(c,_,_) -> c) . find (\(_,a,_) -> a == addr) . join

populateGridAndFindFirstGreaterThan :: Val -> Grid -> (Val, [Cell])
populateGridAndFindFirstGreaterThan target grid =
  let cells = foldr populate (join grid) addrs
      cells' = sortBy cellValue cells
      (Just cell) = find (valGT target) cells'
   in (getVal cell, cells')
  where
    getAddr (_,a,_) = a
    getVal (_,_,v) = v
    cellValue c1 c2 = compare (getVal c1) (getVal c2)
    valGT t c = getVal c > t
    addrs = reverse [1 .. maxAddr]

    maxAddr :: Int
    maxAddr =
      case grid of
        (row:_) -> length row
        _ -> 0

    populate :: Address -> [Cell] -> [Cell]
    populate addr cells =
      let (c,a,_) = findByAddr addr cells
          neighborVals = map getVal . neighbors c $ cells
          val' = if a == 1 then 1 else sum neighborVals
          cell' = (c,a,val')
       in updateCell cell' cells
      where
        updateCell :: Cell -> [Cell] -> [Cell]
        updateCell c@(_,a,_) cs =
          let cs' = filter (not . ((==) a) . getAddr) cs
           in c:cs'

    findByAddr :: Address -> [Cell] -> Cell
    findByAddr addr cells =
      let (Just cell) = find (\(_,a,_) -> a == addr) cells
       in cell

    neighbors :: Coords -> [Cell] -> [Cell]
    neighbors c@(x,y) cells =
      let xs  = [x-1,x,x+1]
          ys  = [y-1,y,y+1]
          xys = [(x',y') | x' <- xs, y' <- ys, (x',y') /= c]
       in filter (\(xy,_,_) -> xy `elem` xys) cells


-- printCells :: [Cell] -> IO ()
-- printCells = mapM_ id . snd . foldl f (0, []) . reverse . sortBy xy
--   where
--     xy :: Cell -> Cell -> Ordering
--     xy ((x1,y1),_,_) ((x2,y2),_,_) = compare (y1,x1) (y2,x2)
--
--     f :: (Int, [IO ()]) -> Cell -> (Int, [IO ()])
--     f (y,ps) ((_,y'),_,v) =
--       -- let p = if y == y' then pr else pn
--       let p = if y' == y then pr else pn
--        in (y',(p v):ps)
--     pr :: Int -> IO ()
--     pr = printf "%03d "
--     pn :: Int -> IO ()
--     pn i = pr i >> putStrLn "\n"
-- -- printGrid :: Grid -> IO ()
-- -- printGrid = mapM_ id . snd . foldl f (0, []) . reverse . sortBy xy . join
-- --   where
-- --     xy :: Cell -> Cell -> Ordering
-- --     xy ((x1,y1),_,_) ((x2,y2),_,_) = compare (y1,x1) (y2,x2)
-- --
-- --     f :: (Int, [IO ()]) -> Cell -> (Int, [IO ()])
-- --     f (y,ps) ((_,y'),a,_) =
-- --       -- let p = if y == y' then pr else pn
-- --       let p = if y' == y then pr else pn
-- --        in (y',(p a):ps)
-- --     pr :: Int -> IO ()
-- --     pr = printf "%02d "
-- --     pn :: Int -> IO ()
-- --     pn i = pr i >> putStrLn "\n"



--      from 1 to 2 -- size is odd
-- 4 3  2. unshift reverse row
-- 1 2  1. push col bot-to-top
--
--        from 2 to 3 -- size is even
-- 5 4 3  1. unshift col top-to-bot
-- 6 1 2
-- 7 8 9  2. push row
--
--              from 3 to 4 -- size is odd
-- 16 15 14 13  2. unshift reverse row
--  5  4  3 12
--  6  1  2 11
--  7  8  9 10  1. push col bot-to-top
--
-- 17 16 15 14 13
-- 18  5  4  3 12
-- 19  6  1  2 11
-- 20  7  8  9 10
-- 21 22 23 24 25

--    y 0  1  2  3  4
-- x
-- 0   16 15 14 13
-- 1    5  4  3 12
-- 2    6  1  2 11
-- 3    7  8  9 10
-- 4

