module L exposing (..)

import Browser exposing (Document)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Store exposing (Model, Msg(..))
import View exposing (ViewT, makeUrl)


match : String -> Model -> Bool
match l model =
    model.url.fragment == Just l


render : String -> List String -> Model -> Document Msg
render l groups { url } =
    { title = l
    , body =
        [ div [] (List.indexedMap (\i s -> button [ onClick <| Store.ChangeUrl <| makeUrl url [ l, String.fromInt i ] ] [ text s ]) groups)
        ]
    }


makeView : String -> List String -> ViewT Model Msg
makeView l groups =
    { match = match l
    , render = render l groups
    }
