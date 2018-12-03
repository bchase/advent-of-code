module Types exposing (..)


type alias Flags =
  { mode : String
  , ab : String
  , input : String
  }


type alias Input =
  { mode : Mode
  , ab : AB
  , input : String
  }


type alias Model =
  Result Flags Input


type Mode
  = Run
  | Test


type AB
  = A
  | B


type alias Output =
  { success : Bool
  , payload : String
  }
