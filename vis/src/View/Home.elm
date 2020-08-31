module View.Home exposing (view)

import Browser exposing (Document)
import Browser.Navigation exposing (pushUrl)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Store exposing (Model, Msg(..))
import View exposing (ViewT, makeUrl)


match : Model -> Bool
match model =
    model.url.fragment == Nothing


render : Model -> Document Msg
render { url } =
    { title = "home"
    , body =
        [ div []
            [ button [ onClick (Store.ChangeUrl <| makeUrl url [ "l0" ]) ] [ text "l0" ]
            , button [ onClick (Store.ChangeUrl <| makeUrl url [ "l1" ]) ] [ text "l1" ]
            , button [ onClick (Store.ChangeUrl <| makeUrl url [ "l2" ]) ] [ text "l2" ]
            ]
        ]
    }


view : ViewT Model Msg
view =
    { match = match
    , render = render
    }
