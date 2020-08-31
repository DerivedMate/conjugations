module Helpers exposing (..)


listFind : (a -> Bool) -> List a -> Maybe a
listFind f xs_ =
    case xs_ of
        [] ->
            Nothing

        x :: xs ->
            if f x then
                Just x

            else
                listFind f xs
