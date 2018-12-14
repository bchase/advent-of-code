#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc

import Data.Char
import Data.Function
import Data.List


type Point = (Int, Int)



main :: IO ()
main = do
  -- testA

  ps <- map read . map (\str -> "(" ++ str ++ ")") . lines <$> readFile "input/06.txt"
  print $ gridCountByChar ps

  return ()


type Grid a = [((Int, Int), a)]

---- PRINT ----

gridCountByChar :: [Point] -> [(Char, Int)]
gridCountByChar ps = sortBy (flip compare `on` snd) numPerChar
  where
    grid = allClosest ps

    cs     = nub . map snd $ grid
    ignore = nub . map snd . filter (onBorder . fst) $ grid

    numPerChar :: [(Char, Int)]
    numPerChar = map (\ch -> (ch, length $ filter (\(xy,ch') -> ch == ch' && ch' `notElem` ignore) grid)) cs

    (xs,ys) = unzip ps

    onBorder (x,y)
      =  x == minimum xs
      || x == maximum xs
      || y == minimum ys
      || y == maximum ys

allClosest :: [Point] -> Grid Char
allClosest ps = map f grid'
  where
    grid  = markGrid ps
    grid' = map (\(xy,ch) -> (xy, closest xy grid)) grid

    f (xy, [ch]) = (xy, ch)
      -- | xy `elem` ps = (xy, ch )
      -- | otherwise    = (xy, toLower ch )
    f (xy, _) = (xy, '.')


closest :: (Int, Int) -> Grid Char -> [Char]
closest xy grid = map (snd) . filter ((== min') . fst) $ dists
  where
    ps = filter (isAlpha . snd) grid

    dists = map (\(xy',ch) -> (dist xy xy', ch)) ps
    dist (x1,y1) (x2,y2) = abs (y2 - y1) + abs (x2 - x1)

    min' = minimum $ map fst dists


markGrid :: [Point] -> Grid Char
markGrid ps = map (\(xy,_) -> (xy, ch xy)) grid
  where
    ch :: (Int, Int) -> Char
    ch xy = maybe '.' id . fmap snd . find ((== xy) . fst) $ chs

    chs = zip ps $ concat [['A'..'Z'], ['a'..'z']]

    grid = newGrid (mx,my) ()

    (xs,ys) = unzip ps

    mx  = maximum xs
    my  = maximum ys

newGrid :: (Int, Int) -> a -> Grid a
newGrid (mx,my) v = [ ((x,y),v) | x <- [0..mx+1], y <- [0..my+1] ]

gs :: Grid Char -> String
gs grid =
  intercalate "\n" $ map (\y -> map (\x -> snd . head . filter (\(xy,ch) -> xy == (x,y)) $ grid) [0..mx]) [0..my]
  where
    xys = map fst grid
    xs  = map fst xys
    ys  = map snd xys
    mx  = maximum xs
    my  = maximum ys


p :: [Point] -> String
p ps = intercalate "\n" $
  -- map (\(r,cols) -> map (\(c,_) -> pt (c,r)) cols) grid'
  foo pt grid
  where
    foo :: ((Int, Int) -> a -> b) -> [[a]] -> [[b]]
    foo f grid = map (\(r,cols) -> map (\(c,v) -> f (c,r) v) cols) grid'
      where
        grid' = zip [0..] . map (zip [0..]) $ grid

        fromJust (Just x) = x
        fromJust Nothing  = error "`fromJust` called with `Nothing`"

    pt xy _
      | xy `elem` ps = 'X'
      | otherwise    = '.'

    -- grid' = zip [0..] . map (zip [0..]) $ grid :: [(Int, [(Int, Char)])]
    grid  = replicate (maxY+2) (replicate (maxX+2) '.')

    xy x y
      | (x,y) `elem` ps = 'X'
      | otherwise =  '.'

    -- lefts   = undefined
    -- rights  = undefined
    -- tops    = undefined
    -- bottoms = undefined

    (xs, ys) = unzip ps

    minX = minimum xs
    maxX = maximum xs
    minY = minimum xs
    maxY = maximum xs



---- TEST ----

testA :: IO ()
testA = do
  putStrLn $ gs $ allClosest testPoints
  mapM_ print $ gridCountByChar testPoints

testPoints :: [Point]
testPoints =
  -- [ ('A', (1, 1))
  -- , ('B', (1, 6))
  -- , ('C', (8, 3))
  -- , ('D', (3, 4))
  -- , ('E', (5, 5))
  -- , ('F', (8, 9))
  -- ]
  [ (1, 1)
  , (1, 6)
  , (8, 3)
  , (3, 4)
  , (5, 5)
  , (8, 9)
  ]
