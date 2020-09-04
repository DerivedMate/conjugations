module Store exposing (..)

import Browser.Navigation exposing (Key, pushUrl)
import Data exposing (Data)
import Http
import Url exposing (Url)


type DataStatus
    = Loaded
    | Loading
    | Failed Http.Error


type alias Model =
    { navKey : Key
    , url : Url
    , l : Maybe Int
    , i : Maybe Int
    , data : Maybe Data
    , dataStatus : DataStatus
    }


type Msg
    = FetchData String -- url
    | LoadData Data -- stringified json
    | FailedData Http.Error
    | ChangeUrl Url
    | ChangeL (Maybe Int)
    | ChangeI (Maybe Int)
    | Identity


init : flags -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , url = url
      , l = Nothing
      , i = Nothing
      , data = Nothing
      , dataStatus = Loaded
      }
    , Cmd.batch
        [ fetchData dataUrl
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        -- `ChangeUrl` ensues from changing the actual url, hence it calls no Cmd-s itself
        ChangeUrl url ->
            ( { state | url = url }
            , if url /= state.url then
                pushUrl state.navKey <| Url.toString url

              else
                Cmd.none
            )

        ChangeI i ->
            ( { state | i = i }, Cmd.none )

        ChangeL l ->
            ( { state | l = l }, Cmd.none )

        FailedData e ->
            ( { state | dataStatus = Failed e }, Cmd.none )

        LoadData data ->
            ( { state | data = Just data, dataStatus = Loaded }, Cmd.none )

        Identity ->
            ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


dataUrl : String
dataUrl =
    "https://raw.githubusercontent.com/DerivedMate/conjugations/master/out.4.2.min.json"


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
