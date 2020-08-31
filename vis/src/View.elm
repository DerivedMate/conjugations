module View exposing (ViewT, makeUrl, onUrlChange, router)

import Browser exposing (Document)
import Helpers exposing (listFind)
import Html exposing (div, text)
import Store exposing (Model, Msg(..))
import Url


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


makeUrl : Url.Url -> List String -> Url.Url
makeUrl url0 parts =
    let
        frag =
            if List.length parts == 0 then
                Nothing

            else
                Just <| String.join "/" parts
    in
    { url0 | fragment = frag }


onUrlChange : Url.Url -> Msg
onUrlChange url =
    Store.ChangeUrl url
