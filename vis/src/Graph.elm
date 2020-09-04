module Graph exposing (..)

import Array
import Browser exposing (Document)
import Data exposing (Category(..), Data, Group(..), Idd(..), L, L2, iddId)
import Helpers exposing (indexOfFragment)
import Html exposing (Html, button, div, text)
import Pie
import Store exposing (Model, Msg)
import Url exposing (Url)
import View exposing (ViewT, makeUrl)


type alias AccessFunction c =
    Data -> List (Idd (Group String (L c)))


match : String -> Model -> Bool
match l model =
    case model.url.fragment of
        Just f ->
            String.startsWith l f

        Nothing ->
            False


pieElem : Url -> Int -> Idd (Group String (L c)) -> Pie.PrePie
pieElem url j (Idd i (Group ms _)) =
    { x = List.length ms
    , url = makeUrl url [ "l1", String.fromInt i, String.fromInt j ]
    }


render : AccessFunction c -> Model -> Document Msg
render d model =
    case model.data of
        Just data ->
            let
                i =
                    indexOfFragment (Maybe.withDefault "" model.url.fragment)
            in
            { title = "l1/" ++ String.fromInt i
            , body =
                [ Pie.pie <|
                    Pie.pieify <|
                        List.indexedMap (pieElem model.url) <|
                            List.filter ((==) i << iddId) (d data)
                ]
            }

        _ ->
            { title = "NO DATA l1/..."
            , body = [ div [] [ text "It's no shit, dawg" ] ]
            }


makeView : String -> AccessFunction c -> ViewT Model Msg
makeView l d =
    { match = match l
    , render = render d
    }
