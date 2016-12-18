{-
This is a lottery game called "schijt je rijk".
Copyright (C) 2016 Jappie Klooster

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.If not, see <http://www.gnu.org/licenses/>.
-}
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
