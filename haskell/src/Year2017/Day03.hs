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


type Grid = [[Int]]

type Address = Int
type Val = Int
type GridArray = Array (Int, Int) (Address, Val)


buildGrid :: Int -> Grid
buildGrid square = snd . foldr (\_ (curr, grid) -> (succ curr, buildGrid' curr grid)) (1, [[1]]) $ [1..square]
  where
    buildGrid' :: Int -> Grid-> Grid
    buildGrid' 1 grid = grid
    buildGrid' curr grid = do
      let from = succ ((pred curr) ^ (2 :: Int))
          line = reverse . drop curr $ [(pred from)..(curr ^ (2 :: Int))]
      case even curr of
        False -> do -- from ODD TO EVEN: unshift to first-to-last, then push new list
          let grid' = snd . foldl (\(n, nss) ns -> (succ n, nss ++ [n:ns])) (from, []) $ grid
          grid' ++ [reverse line]
        True -> do -- from EVEN to ODD: push to last-to-first, then unshift new list
          let grid' = snd . foldr (\ns (n, nss) -> (succ n, nss ++ [ns ++ [n]])) (from, []) $ grid
          line:grid'

-- day02b :: Val -> Grid -> Int
-- day02b _ [] = 0
-- day02b target grid =
--   let vals = join . map (\(y,row) -> map (\(x,val) -> ((x,y),(val,0))) . zip [0..] $ row) . zip [0..] $ grid
--       one = fromJust $ getCoords 1 grid
--       maxIdx = length grid
--       arr = array ((0,maxIdx),(0,maxIdx)) vals :: GridArray
--    in sequentiallyFillInGridArrayUntilTarget maxIdx target arr
--   where
--     sequentiallyFillInGridArrayUntilTarget :: Int -> Val -> GridArray -> Val
--     sequentiallyFillInGridArrayUntilTarget maxIdx target arr =
--
--       addressForNext = getCoords


    -- writeValsUntil :: Int -> Val -> GridArray -> Int
    -- writeValsUntil maxIdx target arr = writeValsUntil' 1 maxIdx target arr
    --
    -- writeValsUntil' :: Int -> Address -> Val -> GridArray -> Int
    -- writeValsUntil' maxIdx 1 target arr = arr//[()]
    -- writeValsUntil' maxIdx address target arr =


  -- let vals = join . map (\(y,row) -> map (\(x,val) -> ((x,y),(val,0))) . zip [0..] $ row) . zip [0..] $ grid
  --     one = fromJust $ getCoords 1 grid
  --     maxIdx = length grid
  --     arr = array ((0,maxIdx),(0,maxIdx)) vals :: GridArray
  --  in writeValsUntil target arr
  -- where
  --   writeValsUntil :: Val -> GridArray -> Int
  --   writeValsUntil target arr = writeValsUntil' 1 target arr
  --
  --   writeValsUntil' :: Address -> Val -> GridArray -> Int
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
      grid = buildGrid gridSize
      arr  = buildGridArray grid
      (oneX,oneY) = fromJust $ getCoords 1 arr
      (locX,locY) = fromJust $ getCoords val arr
      dist = (abs (oneX - locX)) + (abs (oneY - locY))
  -- mapM_ print grid >> putStrLn input
  return . map show $ [ dist ]
  where
    buildGridArray :: Grid -> GridArray
    buildGridArray grid =
      let vals = join . map (\(y,row) -> map (\(x,a) -> ((x,y),(a,0))) . zip [0..] $ row) . zip [0..] $ grid
          maxIdx = length grid - 1
       in array ((0,0),(maxIdx,maxIdx)) vals

    getCoords :: Int -> GridArray -> Maybe (Int, Int)
    getCoords val arr = fmap fst . find (\(_, (address,_)) -> address == val) . assocs $ arr
