module Year2017.Day06 ( day06 ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)

import           Types


type MemoryBank = [Block]
type Block = (Idx, Val)
type Idx = Int
type Val = Int

type State = (Idx, MemoryBank)
type History = (State, [State])


day06 :: Mode -> AB -> String -> IO [String]
day06 mode ab input = return [ result ]
  where
    result =
      case (mode, ab) of
        (Test, A) -> show . map snd . redistribute . parse $ input
        (Run,  A) -> do -- let mb = parse "[0,2,7,0]" -- ==> 5
          let mb = parse "[0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11]" -- ==> 7864
          show $ stepsUntilStateHasBeenSeenBefore mb
        (Run,  B) -> do -- let mb = parse "[0,2,7,0]" -- ==> 4
          let mb = parse "[0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11]" -- ==> 1695
          show $ stepsBackToFirstPreviouslySeenState mb
        _ -> error "NOT-IMPLEMENTED"


parse :: String -> MemoryBank
parse = zip [0..] . read

stepsUntilStateHasBeenSeenBefore :: MemoryBank -> Int
stepsUntilStateHasBeenSeenBefore =
  length . snd . redistributeUntilStateIsRepeated

stepsBackToFirstPreviouslySeenState :: MemoryBank -> Int
stepsBackToFirstPreviouslySeenState mb =
  let ((curr, mb'), mbs) = redistributeUntilStateIsRepeated mb
      prev = fromJust $ fmap fst . find ((==) mb' . snd) $ mbs
   in curr - prev

redistributeUntilStateIsRepeated :: MemoryBank -> History
redistributeUntilStateIsRepeated =
  until stateHasBeenSeenBefore redistributeAndTrack . emptyHistory
  where
    emptyHistory mb = ((0, mb), [])
    stateHasBeenSeenBefore ((_, mb), mbs) = mb `elem` map snd mbs
    redistributeAndTrack((itr, mb), mbs) =
      let mb' = redistribute mb in ((itr+1, mb'), (itr, mb):mbs)


redistribute :: MemoryBank -> MemoryBank
redistribute = zeroBlockAndRedistributeVal . blockToRedistribute
  where
    blockToRedistribute :: MemoryBank -> (Block, MemoryBank)
    blockToRedistribute mb = do
      let max' = maximum . map snd $ mb
       in (head . filter (flip (==) max' . snd) $ mb, mb)

    zeroBlockAndRedistributeVal :: (Block, MemoryBank) -> MemoryBank
    zeroBlockAndRedistributeVal ((idx, dist), mb)=
      let mb' = setValAtIdx idx 0 mb
          trd (_,_,a) = a
       in trd . until blockRedistributed incNextBlockVal $ (idx, dist, mb')

    blockRedistributed :: (Idx, Int, MemoryBank) -> Bool
    blockRedistributed (_, 0, _) = True
    blockRedistributed _ = False

    incNextBlockVal :: (Idx, Int, MemoryBank) -> (Idx, Int, MemoryBank)
    incNextBlockVal (idx, dist, mb) = do
      let (idx',val) = nextBlock (idx+1) mb
          (mb', dist') = (setValAtIdx idx' (val+1) mb, dist-1)
       in (idx', dist', mb')

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
