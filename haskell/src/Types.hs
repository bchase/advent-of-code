module Types
  ( Mode (..)
  , AB (..)
  ) where


data Mode = Run | Test deriving (Show)
data AB = A | B deriving (Show)

instance Read Mode where
  readsPrec _ str =
    case str of
      "test" -> return (Test, "")
      "run"  -> return (Run,  "")
      _      -> error ("Not a valid `Mode`: " ++ str)

instance Read AB where
  readsPrec _ str =
    case str of
      "a" -> return (A, "")
      "b" -> return (B, "")
      "A" -> return (A, "")
      "B" -> return (B, "")
      _   -> error ("Not a valid `AB`: " ++ str)

