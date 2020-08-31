module View exposing (ViewT, router)

import Browser exposing (Document)
import Helpers exposing (listFind)
import Html exposing (div, text)


type alias ViewT model msg =
    { match : model -> Bool
    , render : model -> Document msg
    }


router : List (ViewT model msg) -> model -> Document msg
router views model =
    let
        finder route =
            route.match model
    in
    case listFind finder views of
        Just r ->
            r.render model

        Nothing ->
            { title = "Error 404: Page not found"
            , body = [ div [] [ text "page not found" ] ]
            }
