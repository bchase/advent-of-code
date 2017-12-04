module Types
  ( Mode (..)
  , AB (..)
  ) where


data Mode = Run | Test | NotMode
data AB = A | B | NotAB

instance Read Mode where
  readsPrec _ str =
    case str of
      "test" -> return (Test,    str)
      "run"  -> return (Run,     str)
      _      -> return (NotMode, str)

instance Read AB where
  readsPrec _ str =
    case str of
      "a" -> return (A,     str)
      "b" -> return (B,     str)
      "A" -> return (A,     str)
      "B" -> return (B,     str)
      _   -> return (NotAB, str)

