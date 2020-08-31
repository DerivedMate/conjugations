module View.L1 exposing (..)

import Browser exposing (Document)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Store exposing (Model, Msg(..))
import View exposing (ViewT, makeUrl)


match : Model -> Bool
match model =
    model.url.fragment == Just "l1"


groups : List String
groups =
    [ "participles"
    , "indicative"
    , "subjunctive"
    , "imperative"
    ]


render : Model -> Document Msg
render { url } =
    { title = "l1"
    , body =
        [ div [] (List.indexedMap (\i s -> button [ onClick <| Store.ChangeUrl <| makeUrl url [ "l1", String.fromInt i ] ] [ text s ]) groups)
        ]
    }


view : ViewT Model Msg
view =
    { match = match
    , render = render
    }
