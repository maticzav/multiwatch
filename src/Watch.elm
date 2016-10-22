module Watch exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, millisecond)
import Task
import String


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


indexList : List a -> List ( Int, a )
indexList list =
    List.foldr (\new ( i, s ) -> ( i + 1, ( i, new ) :: s )) ( 0, [] ) list
        |> snd



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
col styles content =
    div [ classList [ styles => True, "col" => True ] ]
        content


row : String -> List (Html Msg) -> Html Msg
row styles content =
    div [ classList [ "row" => True, styles => True ] ]
        content


section : String -> List (Html Msg) -> Html Msg
section styles content =
    div [ classList [ "section" => True, styles => True ] ]
        content


button : String -> String -> Msg -> Html Msg
button color txt msg =
    a
        [ href "#!"
        , onClick msg
        , classList
            [ color => True
            , "btn btn-xlarge" => True
            ]
        ]
        [ text txt ]



-- Custom Elements -----------------------------------------------------------
-- Constructors --------------------------------------------------------------


displayTime : Time -> String
displayTime time =
    let
        minutes =
            Time.inMinutes time
                |> floor

        seconds =
            Time.inSeconds (time - (toFloat minutes) * Time.minute)
                |> floor

        miliseconds =
            Time.inMilliseconds (time - (toFloat seconds) * Time.second)
                |> floor
    in
        List.map toString [ minutes, seconds, miliseconds ]
            |> String.join " : "


displayTimes : List Time -> Html Msg
displayTimes times =
    let
        present : ( Int, Time ) -> Html Msg
        present ( i, time ) =
            col "s6 flow-text" [ text ((toString (i + 1)) ++ " | " ++ (displayTime time)) ]
    in
        indexList times
            |> List.map present
            |> div []


displayAbsoluteTime : Time -> List Time -> Html Msg
displayAbsoluteTime time times =
    case List.head times of
        Just ft ->
            displayTime (time - ft)
                |> text

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
        col "s12 " [ button (String.join " " [ "full-width", color ]) (toString i) (MaxTimes i) ]



-- View ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        app : List (Html Msg)
        app =
            [ section "valign-wrapper no-pad-bot"
                [ row "valign full-width center"
                    [ col "s6" [ p [ class "flow-text abs-time" ] [ displayAbsoluteTime model.time model.start_times ] ]
                    , col "s6" [ p [ class "flow-text abs-time" ] [ (text << toString) (List.length model.start_times) ] ]
                    ]
                ]
            , section "no-pad-bot" [ row "valign full-width center large-line" [ displayTimes ((List.reverse <| List.map ((-) model.time) model.start_times) ++ model.times) ] ]
            , section "bottom full-width no-pad-bot"
                [ row "no-pad-bot no-margin-bot" [ col "s12 no-pad" [ button "black white-text full-width" "Reset" Reset ] ]
                , row "no-margin-bot"
                    [ col "s12 m12 l6 no-pad" [ button "white black-text full-width" "Start" Start ]
                    , col "s12 m12 l6 no-pad" [ button "black white-text full-width" "Stop" Stop ]
                    ]
                ]
            ]

        setup : List (Html Msg)
        setup =
            [ section "valign-wrapper no-pad-bot white black-text"
                [ row "valign center container" [ p [ class "flow-text" ] [ text "Pick the number of maximum times that are being watch at the same time." ] ]
                ]
            , section "no-pad-top no-pad-bot" (List.map displayMaxTimesOption [1..20])
            ]

        content : List (Html Msg)
        content =
            case model.max_times of
                Just _ ->
                    app

                Nothing ->
                    setup
    in
        content
            |> div [ class "full-width full-height white black-text no-pad-bot" ]



-- App -----------------------------------------------------------------------


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ---------------------------------------------------------------------------
