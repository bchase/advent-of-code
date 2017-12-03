module Year2017.Day03 ( day03 ) where

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

type Grid = [[Int]]

buildGrid :: Int -> Grid
buildGrid square = snd . foldr (\_ (curr, grid) -> (succ curr, buildGrid' curr grid)) (1, [[1]]) $ [1..square]
-- buildGrid' 1 square [[1]]
  where
    -- f :: [Int] -> (Int, [[Int]]) -> (Int, [[Int]])
    -- f = undefined

    buildGrid' :: Int -> Grid-> Grid
    buildGrid' 1 grid = grid
    -- buildGrid' from to grid = [[]]
    buildGrid' curr grid =
      case even curr of
        False ->   -- from ODD TO EVEN: unshift to first-to-last, then push new list
          [ [ 5,  4,  3 ]
          , [ 6,  1,  2 ]
          , [ 7,  8,  9 ]
          ]
        True -> do  -- from EVEN to ODD: push to last-to-first, then unshift new list
          let grid' = snd . foldr (\ns (n, nss) -> (succ n, nss ++ [ns ++ [n]])) (curr, []) $ grid
              line  = reverse [succ curr .. curr ^ (2 :: Int)]

          line:grid'

          -- [ [4, 3]
          -- , [1, 2]
          -- ]



-- day03a, day03b :: Grid -> Int
-- day03a :: Int -> Int
-- day03a _ = 1

day03parse :: String -> Int
day03parse = read

day03 :: String -> [String]
day03 input =
  let loc = day03parse input
   in map show [ buildGrid loc, [[1]] ]
