module Year2017.Day06 ( day06 ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)

import           Types


type MemoryBank = [Block]
type Block = (Idx, Val)
type Idx = Int
type Val = Int


day06 :: Mode -> AB -> String -> IO [String]
day06 mode ab input =
  case (mode, ab) of
    (Test, A) -> do
      return [ show . map snd . redistribute . parse $ input ]
    _ -> error "NOTIMPL"


parse :: String -> MemoryBank
parse = zip [0..] . read


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
        [(idx,val)] -> (idx, 1, val-1)
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
          (mb', dist') = if idx' == ignore then (mb, dist) else (setValAtIdx idx' (val+1) mb, dist-1)
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
