port module Helpers exposing (wrap)



port exit : String -> Cmd msg

wrap f = Platform.worker
  { init          = \ input -> ((), exit (f input))
  , update        = \ _ _ -> ((), Cmd.none)
  , subscriptions = always Sub.none
  }
