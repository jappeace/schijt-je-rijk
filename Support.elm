-- this file gives some elm core extensions that don't exists for some reason
module Support exposing (..)
-- elm-lang/core
import Task

--join : List (List a) -> List a
join =
  List.foldr (++) []


{-|-}
--flatMap : (a -> List b) -> List a -> List b
flatMap f list =
  List.map f list
    |> join


{-|-}
flatMap2 : (a -> b -> List c) -> List a -> List b -> List c
flatMap2 f list1 list2 =
  List.map2 f list1 list2
    |> join



{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
https://github.com/shmookey/cmd-extra/blob/1.0.0/src/Cmd/Extra.elm
-}
message : msg -> Cmd msg
message x = Task.perform identity (Task.succeed x) 
