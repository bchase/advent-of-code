module Year2017.Day06 ( day06 ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)

import           Types


type MemoryBank = [Block]
type Block = (Idx, Val)
type Idx = Int
type Val = Int


day06 :: Mode -> AB -> String -> IO [String]
day06 mode ab input = return $ [ result ]
  where
    result =
      case (mode, ab) of
        (Test, A) -> show . map snd . redistribute . parse $ input
        (Run,  A) -> do
          -- let mb = parse "[0,2,7,0]"
          let mb = parse "[0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11]"
          show . length . snd . until stateHasBeenSeenBefore redistributeAndTrack $ (mb, [])

        _ -> error "NOTIMPL"


parse :: String -> MemoryBank
parse = zip [0..] . read

stateHasBeenSeenBefore :: (MemoryBank, [MemoryBank]) -> Bool
stateHasBeenSeenBefore (mb, mbs) = mb `elem` mbs
redistributeAndTrack :: (MemoryBank, [MemoryBank]) -> (MemoryBank, [MemoryBank])
redistributeAndTrack (mb, mbs) = let mb' = redistribute mb in (mb', mb:mbs)

redistribute :: MemoryBank -> MemoryBank
redistribute bank =
  let (i, val', dist) = blockToRedistribute bank
   in setBlockAndRedistributeVal (i,val') dist bank
  where
    blockToRedistribute :: MemoryBank -> (Idx, Val, Val)
    blockToRedistribute mb = do
      let max' = maximum . map snd $ mb
          blks = filter (flip (==) max' . snd) mb
      case blks of
        -- [(idx,val)] -> (idx, 1, val-1)
        [(idx,val)] -> (idx, 0, val)
        (idx,val):_ -> (idx, 0, val)
        _           -> error "EMPTY-MemoryBank"

    setBlockAndRedistributeVal :: Block -> Int -> MemoryBank -> MemoryBank
    setBlockAndRedistributeVal (ignore, val') dist mb =
      let mb' = setValAtIdx ignore val' mb
       in (\(_,_,r,_) -> r) . until blockRedistributed incNextBlockVal $ (ignore, dist, mb', ignore)

    blockRedistributed :: (Idx, Int, MemoryBank, Idx) -> Bool
    blockRedistributed (_, 0, _, _) = True
    blockRedistributed _ = False

    incNextBlockVal :: (Idx, Int, MemoryBank, Idx) -> (Idx, Int, MemoryBank, Idx)
    incNextBlockVal (idx, dist, mb, ignore) = do
      let (idx',val) = nextBlock (idx+1) mb
          -- (mb', dist') = if idx' == ignore then (mb, dist) else (setValAtIdx idx' (val+1) mb, dist-1)
          (mb', dist') = if False then (mb, dist) else (setValAtIdx idx' (val+1) mb, dist-1)
       in (idx', dist', mb', ignore)

    nextBlock :: Idx -> MemoryBank -> Block
    nextBlock idx mb =
      case getBlockAtIdx idx mb of
        Nothing  -> fromJust $ getBlockAtIdx 0 mb
        Just blk -> blk

    setValAtIdx :: Idx -> Val -> MemoryBank -> MemoryBank
    setValAtIdx idx val =
      map (\blk@(idx',_) -> if idx' == idx then (idx,val) else blk)

    getBlockAtIdx :: Idx -> MemoryBank -> Maybe Block
    getBlockAtIdx idx = find (flip (==) idx . fst)
