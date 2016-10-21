module Watch exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, millisecond)
import Task


(=>) =
    (,)



-- Model ---------------------------------------------------------------------


type alias Model =
    { max_times : Maybe Int
    , start_times : List Time
    , time : Time
    , times : List Float
    }



-- Init ----------------------------------------------------------------------


init : ( Model, Cmd Msg )
init =
    let
        model : Model
        model =
            Model
                Nothing
                []
                0
                []
    in
        model ! []



-- Msg -----------------------------------------------------------------------


type Msg
    = MaxTimes Int
    | Start
    | Stop
    | Reset
    | Tick Time



-- Additional Functions ------------------------------------------------------


isEven : Int -> Bool
isEven i =
    i `rem` 2 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven



-- Subscriptions -------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick



-- Update --------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MaxTimes i ->
            let
                newModel : Model
                newModel =
                    { model
                        | max_times = Just i
                    }
            in
                newModel ! []

        Start ->
            let
                newStartTimes : List Time
                newStartTimes =
                    case model.max_times of
                        Just max_times ->
                            List.drop (List.length (model.start_times ++ [ model.time ]) - max_times) (model.start_times ++ [ model.time ])

                        Nothing ->
                            []

                newModel : Model
                newModel =
                    { model
                        | start_times = newStartTimes
                    }
            in
                newModel ! []

        Stop ->
            let
                newTimes : List Float
                newTimes =
                    case List.head model.start_times of
                        Just time ->
                            (model.time - time) :: model.times

                        Nothing ->
                            model.times

                newModel : Model
                newModel =
                    { model
                        | start_times = List.drop 1 model.start_times
                        , times = newTimes
                    }
            in
                newModel ! []

        Reset ->
            init

        Tick newTime ->
            let
                newModel : Model
                newModel =
                    { model
                        | time = newTime
                    }
            in
                newModel ! []



-- Material Elements ---------------------------------------------------------


col : String -> List (Html Msg) -> Html Msg
col size content =
    div [ classList [ size => True, "col" => True ] ]
        content


row : List (Html Msg) -> Html Msg
row content =
    div [ class "row" ]
        content


button : String -> String -> Msg -> Html Msg
button color txt msg =
    a
        [ href "#!"
        , onClick msg
        , classList
            [ color => True
            , "btn btn-large" => True
            ]
        ]
        [ text txt ]



-- Custom Elements -----------------------------------------------------------
-- Constructors --------------------------------------------------------------


displayTime : Time -> Html Msg
displayTime time =
    Time.inMilliseconds time
        |> toString
        |> text


displayTimes : List Time -> Html Msg
displayTimes times =
    div []
        (List.map displayTime times)


displayAbsoluteTime : Time -> List Time -> Html Msg
displayAbsoluteTime time times =
    case List.head times of
        Just ft ->
            displayTime (time - ft)

        Nothing ->
            text "Start"


displayMaxTimesOption : Int -> Html Msg
displayMaxTimesOption i =
    let
        color : String
        color =
            if isOdd i then
                "black white-text"
            else
                "white black-text"
    in
        col "s12" [ button color (toString i) (MaxTimes i) ]



-- View ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        app : List (Html Msg)
        app =
            [ row [ displayAbsoluteTime model.time model.start_times ]
            , row [ displayTimes model.times ]
            , row [ col "s12" [ button "black white-text" "Reset" Reset ] ]
            , row
                [ col "s12 m12 l6" [ button "white black-text" "Start" Start ]
                , col "s12 m12 l6" [ button "black white-text" "Stop" Stop ]
                ]
            ]

        setup : List (Html Msg)
        setup =
            (List.map displayMaxTimesOption [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ])

        content : List (Html Msg)
        content =
            case model.max_times of
                Just _ ->
                    app

                Nothing ->
                    setup
    in
        content
            |> div []



-- App -----------------------------------------------------------------------


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ---------------------------------------------------------------------------
