module Helpers exposing (..)
import Array
import Array exposing (Array)


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


indexOfFragment : String -> Int
indexOfFragment f =
    case String.split "/" f of
        [ _, iS ] ->
            String.toInt iS |> Maybe.withDefault 0

        _ ->
            0


groupBy : (a -> b) -> List a -> List (List a)
groupBy f a =
    let
        aux ext xs_ ys_ zs_ =
            case ( xs_, ys_, zs_ ) of
                ( [], ys, zs ) ->
                    (zs :: ys) |> List.map List.reverse |> List.reverse

                ( x :: xs, ys, [] ) ->
                    aux ext xs ys [ x ]

                ( x :: xs, ys, (z :: _) as zt ) ->
                    if ext x == ext z then
                        aux ext xs ys (x :: zt)

                    else
                        aux ext xs (zt :: ys) [ x ]
    in
    aux f a [] []

arrayGroupBy : (a -> b) -> List a -> Array (Array a)
arrayGroupBy f a = groupBy f a |> List.map Array.fromList |> Array.fromList