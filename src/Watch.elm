module Watch exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, millisecond)
import Task


-- Model ---------------------------------------------------------------------


type alias Model =
    { max_times : Int
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
                5
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
                        | max_times = i
                    }
            in
                newModel ! []

        Start ->
            let
                newStartTimes : List Time
                newStartTimes =
                    List.drop (List.length (model.start_times ++ [ model.time ]) - model.max_times) (model.start_times ++ [ model.time ])

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



-- View ----------------------------------------------------------------------


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


button : String -> String -> Msg -> Html Msg
button color txt msg =
    a
        [ href "#!"
        , onClick msg
        ]
        [ text txt ]


view : Model -> Html Msg
view model =
    [ div [] [ displayAbsoluteTime model.time model.start_times ]
    , div [] [ displayTimes model.times ]
    , div [] [ button "" "Reset" Reset ]
    , div []
        [ button "" "Start" Start
        , text "   :     "
        , button "" "Stop" Stop
        ]
    ]
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
