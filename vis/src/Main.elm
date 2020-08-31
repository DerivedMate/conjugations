module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import Data
import Html exposing (div, text)
import Http
import Json.Decode exposing (Error)
import Pie
import Store exposing (..)
import Url exposing (Url)
import View exposing (ViewT, onUrlChange)
import View.Home
import View.L1
import View.L1.Graph

    

{- URL structure:
   home : /
   overview: /l{0,1,2}/
   view-chart:
     { /l0/{0..12}/
     , /l1/{0..3}/
     , /l2/0/
     }
   view-category: <*view-chart*>/[i]
-}
--


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


routes : List (ViewT Model Msg)
routes =
    [ View.Home.view
    , View.L1.view
    , View.L1.Graph.view
    ]
 

view : Model -> Document Msg
view model =
    View.router routes model



{- case ( l, i ) of
   _ ->
       { title = "home"
       , body = [ div [] [ text "hello" ], Pie.pie (List.map (\a -> { x = a, url = "https://google.com" }) [ 1, 2, 3 ] |> Pie.pieify) ]
       }
-}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest _ =
    Identity
