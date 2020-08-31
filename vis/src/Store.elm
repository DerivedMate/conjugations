module Store exposing (..)
import Data exposing (Data)
import Url exposing (Url)
import Browser.Navigation exposing (Key)
import Http

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
    | ChangeL (Maybe Int)
    | ChangeI (Maybe Int)
    | Identity