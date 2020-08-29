module Main exposing (..)

type alias Model = 
  {

  }

type Msg
  = FetchData Data

type Data 
  = Subsection Subsec
  | Graph Section

type Subsec 
  = Tenses
  | Moods

type Section = Section Int 

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