#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc

-- import Debug.Trace (trace)
import Data.List ((\\), delete, nub, find)


main :: IO ()
main = do
  txt <- readFile "aoc201802.txt"
  let ids = lines txt

  ---- A ----
  -- -- mapM_ print $ failures doublesAndTriples dataA
  -- print $ chksum (map fst dataA)
  -- print $ chksum ids

  ---- B ----
  -- print $ failures differBy1 dataB
  print $ differBy1 ids



---- A ----

chksum :: [String] -> Int
chksum ss = ds * ts
  where
    dts = map doublesAndTriples ss
    ds  = length . filter id . map fst $ dts
    ts  = length . filter id . map snd $ dts

doublesAndTriples :: String -> (Bool, Bool)
doublesAndTriples str = (any (== 2) occs, any (== 3) occs)
  where occs = map snd $ occCount str

occCount :: String -> [(Char, Int)]
occCount str = map (\c -> (c, length (filter (== c) str))) cs
  where cs = nub str



---- B ----

differBy1 :: [String] -> Maybe String
differBy1 ss =
  case ss' of
    []         -> Nothing
    ((s,s'):_) -> Just $ overlap s s'
  where ss' = [ (s,s') | s <- ss, s' <- ss, differingChars s s' == 1 ]

overlap :: ( Eq a ) => [a] -> [a] -> [a]
overlap xs ys = foldr delete xs (xs \\ ys)

differingChars :: String -> String -> Int
differingChars = differingChars' 0

differingChars' :: Int -> String -> String -> Int
differingChars' count []       _        = count
differingChars' count _        []       = count
differingChars' count (c1:cs1) (c2:cs2) = differingChars' count' cs1 cs2
  where count' = if c1 /= c2 then count + 1 else count


----

dataA :: [(String, (Bool, Bool))]
dataA =
  [ ("abcdef", (False , False)) -- contains no letters that appear exactly two or three times.
  , ("bababc", (True  , True )) -- contains two a and three b, so it counts for both.
  , ("abbcde", (True  , False)) -- contains two b, but no letter appears exactly three times.
  , ("abcccd", (False , True )) -- contains three c, but no letter appears exactly two times.
  , ("aabcdd", (True  , False)) -- contains two a and two d, but it only counts once.
  , ("abcdee", (True  , False)) -- contains two e.
  , ("ababab", (False , True )) -- contains three a and three b, but it only counts once.
  ]

dataB :: [([String], Maybe String)]
dataB = [(lines "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz", Just "fgij")]


failures :: ( Eq b ) => (a -> b) -> [(a,b)] -> [(a,b,b)]
failures f = filter (\(i,o,o') -> o /= o') . map (\(i,o) -> (i,o,f i))
