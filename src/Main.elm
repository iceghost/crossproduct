module Main exposing (..)

import Browser
import Html exposing (..)



-- MODEL


type alias Vector =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Model =
    { firstVector : Vector
    , secondVector : Vector
    , resultInput : Vector
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { firstVector = Vector 0 1 2
      , secondVector = Vector 3 4 5
      , resultInput = Vector 0 0 0
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Hello world!" ]



-- UPDATE


type Msg
    = InputX
    | InputY
    | InputZ


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
