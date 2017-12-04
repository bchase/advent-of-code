module Year2017.Day03 ( day03 ) where

import           Data.List  (find)
import           Data.Array (Array, array, assocs)
import           Data.Maybe (fromJust)
import           Control.Monad (join)

-- [ [1]
-- ]
--
-- -- lower left
-- [ [4, 3] -- < new row
-- , [1, 2]
-- ]
-- --    ^ new column
--
-- -- upper right
-- [ [ 5,  4,  3 ]
-- , [ 6,  1,  2 ]
-- , [ 7,  8,  9 ] -- < new row
-- ]
-- --  ^ new column
--
-- -- lower left
-- [ [ 16, 15, 14, 13 ] -- < new row
-- , [  5,  4,  3, 12 ]
-- , [  6,  1,  2, 11 ]
-- , [  7,  8,  9, 10 ]
-- ]
-- --               ^ new column
--
-- -- upper right
-- [ [ 17, 16, 15, 14, 13 ]
-- , [ 18,  5,  4,  3, 12 ]
-- , [ 19,  6,  1,  2, 11 ]
-- , [ 20,  7,  8,  9, 10 ]
-- , [ 21, 22, 23, 24, 25 ] -- < new row
-- ]
-- --   ^ new column
--
-- -- [ [ 17, 16, 15, 14, 13 ] -- 4x4 4^2 EVEN
-- -- , [ 18,  5,  4,  3, 12 ] -- 2x2 2^2 EVEN
-- -- , [ 19,  6,  1,  2, 11 ] -- 1x1 1^2 ODD (ONE)
-- -- , [ 20,  7,  8,  9, 10 ] -- 3x3 3^2 ODD
-- -- , [ 21, 22, 23, 24, 25 ] -- 5%5 5^2 ODD
-- -- ]
--
-- -- [ [  4,  3,  2,  3,  4 ] -- 4x4 4^2 EVEN
-- -- , [  3,  2,  1,  2,  3 ] -- 2x2 2^2 EVEN
-- -- , [  2,  1,  0,  1,  2 ] -- 1x1 1^2 ODD (ONE)
-- -- , [  3,  2,  1,  2,  3 ] -- 3x3 3^2 ODD
-- -- , [  4,  3,  2,  3,  4 ] -- 5%5 5^2 ODD
-- -- ]


type Coords = (Int, Int)
type Address = Int
type Val = Int
type Grid = Array Coords (Address, Val)


-- day02b :: Val -> Grid -> Int
-- day02b _ [] = 0
-- day02b target grid =
--   let vals = join . map (\(y,row) -> map (\(x,val) -> ((x,y),(val,0))) . zip [0..] $ row) . zip [0..] $ grid
--       one = fromJust $ getCoords 1 grid
--       maxIdx = length grid
--       arr = array ((0,maxIdx),(0,maxIdx)) vals :: Grid
--    in sequentiallyFillInGridArrayUntilTarget maxIdx target arr
--   where
--     sequentiallyFillInGridArrayUntilTarget :: Int -> Val -> Grid -> Val
--     sequentiallyFillInGridArrayUntilTarget maxIdx target arr =
--
--       addressForNext = getCoords


    -- writeValsUntil :: Int -> Val -> Grid -> Int
    -- writeValsUntil maxIdx target arr = writeValsUntil' 1 maxIdx target arr
    --
    -- writeValsUntil' :: Int -> Address -> Val -> Grid -> Int
    -- writeValsUntil' maxIdx 1 target arr = arr//[()]
    -- writeValsUntil' maxIdx address target arr =


  -- let vals = join . map (\(y,row) -> map (\(x,val) -> ((x,y),(val,0))) . zip [0..] $ row) . zip [0..] $ grid
  --     one = fromJust $ getCoords 1 grid
  --     maxIdx = length grid
  --     arr = array ((0,maxIdx),(0,maxIdx)) vals :: Grid
  --  in writeValsUntil target arr
  -- where
  --   writeValsUntil :: Val -> Grid -> Int
  --   writeValsUntil target arr = writeValsUntil' 1 target arr
  --
  --   writeValsUntil' :: Address -> Val -> Grid -> Int
  --   writeValsUntil' address target arr =



-- tagGrid :: Grid -> TaggedGrid
-- tagGrid []    = []
-- tagGrid [[]]  = []
-- tagGrid [[1]] = [[(1,0)]]
-- tagGrid grid@(row:_) =
--   let height = length grid
--       width  = length row
--   grid
-- [ [  3,  2,  3,  4 ]
-- , [  2,  1,  2,  3 ]
-- , [  1,  0,  1,  2 ]
-- , [  2,  1,  2,  3 ]
-- ]
--
-- [ [  4,  3,  2,  3,  4 ]
-- , [  3,  2,  1,  2,  3 ]
-- , [  2,  1,  0,  1,  2 ]
-- , [  3,  2,  1,  2,  3 ]
-- , [  4,  3,  2,  3,  4 ]
-- ]


-- day03a, day03b :: Grid -> Int
-- day03a :: Int -> Int
-- day03a _ = 1

day03parse :: String -> Int
day03parse = read

day03 :: String -> IO [String]
day03 input = do
  let val = day03parse input
      gridSize = ceiling . sqrt $ (fromIntegral val :: Double)
      grid = buildGridArray gridSize
      (oneX,oneY) = fromJust $ getCoordsForAddress 1 grid
      (locX,locY) = fromJust $ getCoordsForAddress val grid
      dist = (abs (oneX - locX)) + (abs (oneY - locY))
  -- mapM_ print grid >> putStrLn input
  return . map show $ [ dist ]

getCoordsForAddress :: Address -> Grid -> Maybe (Int, Int)
getCoordsForAddress addr = fmap fst . find (((==) addr) . fst . snd) . assocs

-- xs = join [[xp,xm] | xp <- x+1, xm <- x-1]
-- yx = join [[yp,ym] | yp <- y+1, ym <- y-1]
-- xy = [(x',y') | x' <- xs, y' <- yx, x' >= 0 && x' <= max' && y' >= 0 && y' <= max' ]
-- getValForCoords :: (Coords, Coords) -> Coords -> Grid -> Maybe (Int, Int)
-- getValForCoords (min',max') val grid = undefined

buildGridArray :: Int -> Grid
buildGridArray size =
  let grid = buildListGrid size
      maxIdx = length grid - 1
      vals = join . map (\(y,row) -> map (\(x,a) -> ((x,y),(a,0))) . zip [0..] $ row) . zip [0..] $ grid
   in array ((0,0),(maxIdx,maxIdx)) vals
  where
    buildListGrid :: Int -> [[Int]]
    buildListGrid square =
      snd . foldr (\_ (curr, grid) -> (succ curr, buildListGrid' curr grid)) (1, [[1]]) $ [1..square]

    buildListGrid' :: Int -> [[Int]]-> [[Int]]
    buildListGrid' 1 grid = grid
    buildListGrid' curr grid = do
      let from = succ ((pred curr) ^ (2 :: Int))
          line = reverse . drop curr $ [(pred from)..(curr ^ (2 :: Int))]
      case even curr of
        False -> do -- from ODD TO EVEN: unshift to first-to-last, then push new list
          let grid' = snd . foldl (\(n, nss) ns -> (succ n, nss ++ [n:ns])) (from, []) $ grid
          grid' ++ [reverse line]
        True -> do -- from EVEN to ODD: push to last-to-first, then unshift new list
          let grid' = snd . foldr (\ns (n, nss) -> (succ n, nss ++ [ns ++ [n]])) (from, []) $ grid
          line:grid'
