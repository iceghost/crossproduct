module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (readonly, size, value)
import Html.Events exposing (onClick, onInput)
import Random



-- MODEL


type alias Vector a =
    { x : a
    , y : a
    , z : a
    }


type alias Model =
    { firstVector : Vector Int
    , secondVector : Vector Int
    , resultInput : Vector String
    , status : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { firstVector = Vector 0 0 0
      , secondVector = Vector 0 0 0
      , resultInput = Vector "" "" ""
      , status = "Good luck!"
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ div []
            [ text "Vector a : "
            , viewVector model.firstVector
            ]
        , div []
            [ text "Vector b : "
            , viewVector model.secondVector
            ]
        , div []
            [ text "Result : "
            , div []
                [ input
                    [ value model.resultInput.x
                    , size 3
                    , onInput InputX
                    ]
                    []
                , input
                    [ value model.resultInput.y
                    , size 3
                    , onInput InputY
                    ]
                    []
                , input
                    [ value model.resultInput.z
                    , size 3
                    , onInput InputZ
                    ]
                    []
                ]
            ]
        , div [] [ text model.status ]
        , button [ onClick GenerateNew ] [ text "Generate" ]
        ]


viewVector : Vector Int -> Html Msg
viewVector vector =
    div []
        [ viewVectorComponent vector.x
        , viewVectorComponent vector.y
        , viewVectorComponent vector.z
        ]


viewVectorComponent : Int -> Html Msg
viewVectorComponent int =
    input
        [ value <| String.fromInt int
        , size 3
        , readonly True
        ]
        []



-- UPDATE


type Msg
    = InputX String
    | InputY String
    | InputZ String
    | CheckInput
    | CheckResult Int Int Int
    | GenerateNew
    | GenerateFirstVector (List Int)
    | GenerateSecondVector (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputX xInput ->
            let
                newModel =
                    { model | resultInput = setX xInput model.resultInput }
            in
            update CheckInput newModel

        InputY yInput ->
            let
                newModel =
                    { model | resultInput = setY yInput model.resultInput }
            in
            update CheckInput newModel

        InputZ zInput ->
            let
                newModel =
                    { model | resultInput = setZ zInput model.resultInput }
            in
            update CheckInput newModel

        CheckInput ->
            case
                ( String.toInt model.resultInput.x
                , String.toInt model.resultInput.y
                , String.toInt model.resultInput.z
                )
            of
                ( Nothing, _, _ ) ->
                    ( { model | status = "Invalid x input" }, Cmd.none )

                ( _, Nothing, _ ) ->
                    ( { model | status = "Invalid y input" }, Cmd.none )

                ( _, _, Nothing ) ->
                    ( { model | status = "Invalid z input" }, Cmd.none )

                ( Just xInput, Just yInput, Just zInput ) ->
                    update (CheckResult xInput yInput zInput) model

        CheckResult xInput yInput zInput ->
            let
                correctAns =
                    crossProduct model.firstVector model.secondVector
            in
            if xInput == correctAns.x && yInput == correctAns.y && zInput == correctAns.z then
                ( { model | status = "You got it correct!" }, Cmd.none )

            else
                ( { model | status = "You did it wrong..." }, Cmd.none )

        GenerateNew ->
            ( model, generateNumbers GenerateFirstVector )

        GenerateFirstVector numbers ->
            case numbers of
                [ a, b, c ] ->
                    ( { model | firstVector = Vector a b c }, generateNumbers GenerateSecondVector )

                _ ->
                    ( model, Cmd.none )

        GenerateSecondVector numbers ->
            case numbers of
                [ a, b, c ] ->
                    ( { model
                        | secondVector = Vector a b c
                        , resultInput = Vector "" "" ""
                        , status = "Good luck!"
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


negFiveToFive : Random.Generator Int
negFiveToFive =
    Random.int -5 5


threeNumbers : Random.Generator (List Int)
threeNumbers =
    Random.list 3 negFiveToFive


generateNumbers : (List Int -> Msg) -> Cmd Msg
generateNumbers msg =
    Random.generate msg threeNumbers


crossProduct : Vector Int -> Vector Int -> Vector Int
crossProduct first second =
    Vector
        (first.y * second.z - first.z * second.y)
        (first.z * second.x - first.x * second.z)
        (first.x * second.y - first.y * second.x)


setX : a -> Vector a -> Vector a
setX newX vector =
    { vector | x = newX }


setY : a -> Vector a -> Vector a
setY newY vector =
    { vector | y = newY }


setZ : a -> Vector a -> Vector a
setZ newZ vector =
    { vector | z = newZ }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
