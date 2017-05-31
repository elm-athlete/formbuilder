module Helpers.ActiveRecord exposing (..)

import List exposing (head)


findAllBy : (a -> b) -> String -> List a -> List a
findAllBy field value list =
    list |> List.filter (\e -> (toString (field e) == value))


findBy : (a -> b) -> String -> List a -> Maybe a
findBy field value list =
    findAllBy field value list |> head


find : String -> List { b | id : a } -> Maybe { b | id : a }
find value list =
    list |> findBy .id value


id : Maybe { b | id : a } -> String
id object =
    case object of
        Just o ->
            o.id |> toString

        Nothing ->
            ""


isNew : { b | id : Maybe a } -> Bool
isNew element =
    element.id == Nothing
