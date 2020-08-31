module View.Home exposing (view)

import Html exposing (div, text)
import View
import Store exposing (Msg(..), Model)
import Browser exposing (Document)


match : Model -> Bool
match model =
    case ( model.l, model.i ) of
        ( Nothing, Nothing ) ->
            True

        _ ->
            False

render : Model -> Document Msg
render _ =
    { title = "home"
    , body =
        [ div [] [ text "home" ]
        ]
    }


view : View.ViewT Model Msg
view =
    { match = match
    , render = render
    }
