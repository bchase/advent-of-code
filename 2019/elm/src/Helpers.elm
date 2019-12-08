port module Helpers exposing (wrap, fromJust)



port exit : String -> Cmd msg

wrap f = Platform.worker
  { init          = \ input -> ((), exit (f input))
  , update        = \ _ _ -> ((), Cmd.none)
  , subscriptions = always Sub.none
  }


fromJust : Maybe a -> a
fromJust mx = case mx of
  Just x  -> x
  Nothing -> Debug.todo "`fromJust` received `Nothing`"
