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
import View exposing (ViewT)
import View.Home



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


init : flags -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , url = url
      , l = Nothing
      , i = Nothing
      , data = Nothing
      , dataStatus = Loaded
      }
    , Cmd.none
    )


routes : List (ViewT Model Msg)
routes =
    [ View.Home.view
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        FailedData e ->
            ( { state | dataStatus = Failed e }, Cmd.none )

        LoadData data ->
            ( { state | data = Just data, dataStatus = Loaded }, Cmd.none )

        Identity ->
            ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlChange _ =
    Identity


onUrlRequest _ =
    Identity


fetchData : String -> Cmd Msg
fetchData url =
    let
        processResponse : Result Http.Error Data.Data -> Msg
        processResponse res =
            case res of
                Ok d ->
                    LoadData d

                Err e ->
                    FailedData e
    in
    Http.get
        { url = url
        , expect = Http.expectJson processResponse Data.decodeData
        }
