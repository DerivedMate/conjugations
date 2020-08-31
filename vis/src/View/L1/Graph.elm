module View.L1.Graph exposing (..)

import Array
import Browser exposing (Document)
import Data exposing (Category(..), Group(..), Idd(..), L2)
import Helpers exposing (indexOfFragment)
import Html exposing (Html, button, div, text)
import Store exposing (Model, Msg)
import View exposing (ViewT)
import Data exposing (L)


match : Model -> Bool
match model =
    case model.url.fragment of
        Just f ->
            String.startsWith "l1" f

        Nothing ->
            False


renderIdd : Idd (Group String (L Category)) -> Html msg
renderIdd (Idd i (Group ms c)) =
    button [] [text <| Debug.toString c]


render : Model -> Document Msg
render model =
    case model.data of
        Just data ->
            let
                i =
                    indexOfFragment (Maybe.withDefault "" model.url.fragment)
            in
            { title = "l1/" ++ String.fromInt i
            , body =
                case Array.get i data.l0 of
                    Just idds ->
                        List.map renderIdd idds

                    _ ->
                        []
            }

        _ ->
            { title = "NO DATA l1/..."
            , body = [ div [] [ text "It's no shit, dawg" ] ]
            }

 
view : ViewT Model Msg
view =
    { match = match
    , render = render
    }
