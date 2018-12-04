#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc --package parsec

import Control.Applicative (some)
-- import Data.List (find)
import Text.ParserCombinators.Parsec



main :: IO ()
main = do
  claims <- map (parse' pClaim) . lines <$> readFile "input/03.txt"

  -- print $ numberOverlapping . applyClaims (newCanvas testClaims) $ testClaims
  --
  -- print $ numberOverlapping . applyClaims (newCanvas claims) $ claims

  -- print $ notOverlapping testClaims
  --
  print $ notOverlapping claims

testClaims :: [Claim]
testClaims = map (parse' pClaim)
  [ "#1 @ 1,3: 4x4"
  , "#2 @ 3,1: 4x4"
  , "#3 @ 5,5: 2x2"
  ]


---- TYPES ----

type Id = String
type Point = (Int, Int)
type Dimensions = (Int, Int)
data Claim = Claim Id Point Dimensions
  deriving ( Show )

type Canvas = [[Int]]

data Bounds = Bounds Id (Int, Int) (Int, Int)
  deriving ( Show )



---- PARSING ----

parse' :: Parser a -> String -> a
parse' p str = either err id $ parse p "" str
  where err = error $ "Failed to parse `Claim`: " ++ str

-- #ID @ X,Y: WxH
pClaim :: Parser Claim
pClaim = do
  char '#'
  id' <- some digit
  string " @ "
  x <- read <$> some digit
  char ','
  y <- read <$> some digit
  string ": "
  w <- read <$> some digit
  char 'x'
  h <- read <$> some digit
  return $ Claim ('#':id') (x,y) (w,h)



---- A ----

numberOverlapping :: Canvas -> Int
numberOverlapping = length . filter (>1) . concat

applyClaims :: Canvas -> [Claim] -> Canvas
applyClaims canvas claims = foldr applyClaim canvas claims

applyClaim :: Claim -> Canvas -> Canvas
applyClaim (Claim _ (x,y) (w,h)) canvas =
  map' (\row -> map' (+1) xs (zip [0..] row)) ys (zip [0..] canvas)
  where
    xs = [x..(x+w-1)]
    ys = [y..(y+h-1)]

    map' :: (a -> a) -> [Int] -> [(Int,a)] -> [a]
    map' _ _  []          = []
    map' f is ((i,x):ixs) = x' : map' f is ixs
      where x' = if i `elem` is then f x else x

bottomRight :: Claim -> (Int, Int)
bottomRight (Claim _ (x,y) (w,h)) = (x+w,y+h)

newCanvas :: [Claim] -> Canvas
newCanvas cs = replicate h . replicate w $ 0
  where
    brs = map bottomRight cs
    w   = maximum $ map fst brs
    h   = maximum $ map snd brs



---- B ----

notOverlapping :: [Claim] -> [Bounds]
notOverlapping cs = [ b | b <- bs, all (not . overlap b) bs ]
  where bs = map toBounds cs

toBounds :: Claim -> Bounds
toBounds (Claim id' (x,y) (w,h)) = Bounds id' (x,y) (x',y')
  where
    x' = x + w - 1
    y' = y + h - 1

overlap :: Bounds -> Bounds -> Bool
overlap b1@(Bounds id1 _ _) b2@(Bounds id2 _ _)
  = (id1 /= id2) && (any (`elem` (xys b1)) (xys b2))
  where
    xys (Bounds _ (x,y) (x',y')) =
      [ (x,y) | x <- [x..x'], y <- [y..y'] ]
