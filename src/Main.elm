module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (readonly, size, style, value)
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
      , status = "Chào mừng bạn đã đến đây :D Hãy tính tích có hướng rồi nhập vào 3 ô trên nhé! Chúc bạn may mắn :3"
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ style "max-width" "500px"
        , style "margin" "auto"
        ]
        [ h1 [] [ text "Luyện tập Tích có hướng" ]
        , div []
            [ text "Vec-tơ a : "
            , viewVector model.firstVector
            ]
        , div []
            [ text "Vec-tơ b : "
            , viewVector model.secondVector
            ]
        , div []
            [ text "[Vec-tơ a, Vec-tơ b] : "
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
        , button [ onClick GenerateNew ] [ text "Tạo vec-tơ mới" ]
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
                    ( { model | status = "Không đọc được tọa độ x..." }, Cmd.none )

                ( _, Nothing, _ ) ->
                    ( { model | status = "Không đọc được tọa độ y..." }, Cmd.none )

                ( _, _, Nothing ) ->
                    ( { model | status = "Không đọc được tọa độ z..." }, Cmd.none )

                ( Just xInput, Just yInput, Just zInput ) ->
                    update (CheckResult xInput yInput zInput) model

        CheckResult xInput yInput zInput ->
            let
                correctAns =
                    crossProduct model.firstVector model.secondVector
            in
            if xInput == correctAns.x && yInput == correctAns.y && zInput == correctAns.z then
                ( { model | status = "Đúng rồi bạn hiền! :D" }, Cmd.none )

            else
                ( { model | status = "Hình như bạn nhân sai chỗ nào rồi..." }, Cmd.none )

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
                        , status = "Đã tạo các vec-tơ mới!"
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
