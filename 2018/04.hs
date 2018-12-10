#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc --package parsec

import Control.Applicative (some)
import Data.Function (on)
import Data.List (intercalate, nub, sort, sortBy)
import Text.Parsec (Parsec, runP, putState, getState)
import Text.ParserCombinators.Parsec



type Id       = Int
data DateTime = DateTime (Int, Int, Int) (Int, Int)   deriving (Show, Eq, Ord)
data Event    = ClockIn Id | FallAsleep | WakeUp      deriving (Show, Eq)
data Entry    = Entry DateTime Event                  deriving (Show, Eq)

data Shift  = Shift Id [(DateTime, Asleep)]           deriving (Show)
data Asleep = Asleep | Awake                          deriving (Show, Eq)


data GuardData = GuardData
  { guardId     :: Id
  , guardShifts :: [Shift]
  , guardSleep  :: Int
  } deriving ( Show )



main :: IO ()
main = do
  entries <- sort . parseEntries . lines <$> readFile "input/04.txt"

  -- print $ sleepiestGuardAndMinuteProduct entries

  print $ mostFrequentSleepMinuteAndGuardProd entries

  return ()



---- B ----

mostFrequentSleepMinuteAndGuardProd :: [Entry] -> Int
mostFrequentSleepMinuteAndGuardProd es = id' * min'
  where (id', (min', _)) = mostFrequentSleepMinuteAndGuard es

mostFrequentSleepMinuteAndGuard :: [Entry] -> (Id, (Int, Int))
mostFrequentSleepMinuteAndGuard es =
  head . sortBy (flip compare `on` (snd . snd)) $ guardMinuteSleep es

mostFrequentSleepMinuteAndGuard' :: [Entry] -> [(Id, (Int, Int))]
mostFrequentSleepMinuteAndGuard' es =
  sortBy (flip compare `on` (snd . snd)) $ guardMinuteSleep es

guardMinuteSleep :: [Entry] -> [(Id, (Int, Int))]
guardMinuteSleep es = map guardMinuteSleep' gids
  where
    shifts = buildShifts es
    gids   = guards shifts

    guardMinuteSleep' gid =
      let ss = shiftsFor gid shifts
       in (gid, sleepiestMinute ss)




---- A ----

sleepiestGuardAndMinuteProduct :: [Entry] -> Int
sleepiestGuardAndMinuteProduct es = id' * min'
  where (id', min') = sleepiestGuardAndMinute es

sleepiestGuardAndMinute :: [Entry] -> (Id, Int)
sleepiestGuardAndMinute es = (guardId guard, fst $ sleepiestMinute (guardShifts guard))
  where guard = sleepiestGuard es

sleepiestGuard :: [Entry] -> GuardData
sleepiestGuard = head . sortBy (flip compare `on` guardSleep) . sleepSchedule

sleepiestMinute :: [Shift] -> (Int, Int)
sleepiestMinute ss = head . sortBy (flip compare `on` snd) $ sleepByMinute
  where
    ms = concat $ map shiftSleep ss :: [(Int, Asleep)]

    sleepByMinute :: [(Int, Int)]
    sleepByMinute = map (\m -> (m, sleepForMinute m ms)) [1..60]

    sleepForMinute :: Int -> [(Int, Asleep)] -> Int
    sleepForMinute m = length . filter (\(m', s) -> m' == m && s == Asleep )

sleepSchedule :: [Entry] -> [GuardData]
sleepSchedule es = map buildData gids
  where
    buildData gid =
      let s = shiftsFor gid shifts
       in GuardData gid s (tallySleep s)

    shifts = buildShifts es
    gids   = guards shifts

tallySleep :: [Shift] -> Int
tallySleep = length . filter (== Asleep) . map snd . concat . map shiftSleep

shiftSleep :: Shift -> [(Int, Asleep)]
shiftSleep (Shift _ evts) = mark awakeHour evts
  where
    awakeHour = map (\m -> (m, Awake)) [1..60]

    mark :: [(Int, Asleep)] -> [(DateTime, Asleep)] -> [(Int, Asleep)]
    mark hour []                 = hour
    mark hour ((dt,status) : es) = mark (set status dt hour) es

    set :: Asleep -> DateTime -> [(Int, Asleep)] -> [(Int, Asleep)]
    set aa (DateTime _ (00,from)) hour =
      map (\t@(m, b) -> if m `elem` ms then (m, aa) else t) hour
      where ms = [from..60]
    set _ _ hour = hour

guards :: [Shift] -> [Id]
guards = nub . map (\(Shift id' _) -> id')

shiftsFor :: Id -> [Shift] -> [Shift]
shiftsFor id' = filter (\(Shift i _) -> i == id')

buildShifts :: [Entry] -> [Shift]
buildShifts = map (\(Shift id' evts) -> Shift id' (reverse evts)) . foldl buildShift []
  where
    buildShift :: [Shift] -> Entry -> [Shift]
    buildShift []     (Entry dt (ClockIn id')) = [newShift id' dt]
    buildShift []      _                       = []
    buildShift    ss  (Entry dt (ClockIn id')) = (newShift id' dt) : ss
    buildShift (s:ss) (Entry dt  WakeUp      ) = (wake dt s)       : ss
    buildShift (s:ss) (Entry dt  FallAsleep  ) = (sleep dt s)      : ss

    newShift id' dt = Shift id' [(dt, Awake)]

    sleep = awake Asleep
    wake  = awake Awake

    awake :: Asleep -> DateTime -> Shift -> Shift
    awake asleep dt (Shift id' xs) = Shift id' ((dt, asleep):xs)



---- PARSING ----

parseEntries :: [String] -> [Entry]
parseEntries = map (fromRight . parse pEntry "parseEntries")

pEntry :: Parser Entry
pEntry = do
  ts <- pTimestamp
  char ' '
  evt <- pEvent
  return $ Entry ts evt

pEvent :: Parser Event
pEvent = pClockIn <|> pFallAsleep <|> pWakeUp
  where
    pClockIn :: Parser Event
    pClockIn = do
      string "Guard #"
      id' <- int
      string " begins shift"
      return $ ClockIn id'

    pFallAsleep :: Parser Event
    pFallAsleep = do
      string "falls asleep"
      return FallAsleep

    pWakeUp :: Parser Event
    pWakeUp = do
      string "wakes up"
      return WakeUp

pTimestamp :: Parser DateTime
pTimestamp = do
  char '['
  y <- int
  char '-'
  m <- int
  char '-'
  d <- int
  char ' '
  hh <- int
  char ':'
  mm <- int
  char ']'
  return (DateTime (y,m,d) (hh,mm))

int :: Parser Int
int = read <$> some digit



---- TEST ----

testEntries :: [Entry]
testEntries = sort . parseEntries $
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-05 00:55] wakes up"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  ]



---- HELPERS ----

fromRight :: ( Show e ) => Either e a -> a
fromRight (Right x) = x
fromRight (Left  e) =
  error $ "`fromRight` received `Left`: " ++ show e



---- INSTANCES ----

instance Ord Entry where
  compare (Entry t1 _) (Entry t2 _) = compare t1 t2
