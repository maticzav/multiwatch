module Watch exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, millisecond)
import Task
import String
import Debug exposing (..)


(=>) =
    (,)



-- Model ---------------------------------------------------------------------


type alias ID =
    Int


type alias Watch =
    { id : ID
    , start : Time
    , laps : List Time
    , end : Maybe Time
    , view : Maybe Int
    }


type alias Model =
    { current_id : ID
    , time : Time
    , times : List Watch
    , max_times : Maybe Int
    }



-- Init ----------------------------------------------------------------------


newWatch : ID -> Time -> Watch
newWatch id time =
    Watch
        id
        time
        []
        Nothing
        Nothing


defaultModel : Model
defaultModel =
    Model
        0
        0
        []
        Nothing


init : ( Model, Cmd Msg )
init =
    defaultModel ! []



-- Msg -----------------------------------------------------------------------


type Msg
    = NoOp
    | Tick Time
    | MaxTimes Int
    | Start
    | Stop
    | Lap ID
    | Display ID
    | Reset



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


isJust : Maybe a -> Bool
isJust myb =
    case myb of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing =
    not << isJust


slice : Int -> Int -> List -> List
slice i s l =
    List.drop i l
        |> List.take s



-- Subscriptions -------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick



-- Update --------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Tick newTime ->
            let
                newModel : Model
                newModel =
                    { model
                        | time = newTime
                    }
            in
                newModel ! []

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
                --TODO: Remove firt unfinished added when starting new
                unfinished : List Watch
                unfinished =
                    List.filter (\t -> isNothing t.end) model.times

                newTimes : List Watch
                newTimes =
                    case model.max_times of
                        Just max_times ->
                            List.drop (1 + List.length unfinished - max_times) (newWatch model.current_id model.time :: model.times)

                        Nothing ->
                            []

                max_id : Int
                max_id =
                    case List.maximum (List.map .id newTimes) of
                        Just max ->
                            max

                        Nothing ->
                            model.current_id

                newModel : Model
                newModel =
                    { model
                        | current_id = 1 + max_id
                        , times = newTimes
                    }
            in
                newModel ! []

        Stop ->
            let
                unfinished : List Watch
                unfinished =
                    List.filter (\t -> isNothing t.end) model.times

                end : ID -> Watch -> Watch
                end id watch =
                    if watch.id == id then
                        { watch | end = Just model.time }
                    else
                        watch

                newTimes : List Watch
                newTimes =
                    case List.head (List.sortBy .id unfinished) of
                        Just watch ->
                            List.map (end watch.id) model.times

                        Nothing ->
                            model.times

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                    }
            in
                newModel ! []

        Lap id ->
            let
                unfinished : List Watch
                unfinished =
                    List.filter (\t -> isNothing t.end) model.times

                lap : ID -> Watch -> Watch
                lap id watch =
                    if watch.id == id then
                        { watch | laps = model.time :: watch.laps }
                    else
                        watch

                newTimes : List Watch
                newTimes =
                    if List.member id (List.map .id unfinished) then
                        List.map (lap id) model.times
                    else
                        model.times

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                    }
            in
                newModel ! []

        Display id ->
            let
                nv : Watch -> Maybe Int
                nv watch =
                    case watch.view of
                        Just v ->
                            if (v + 1) >= List.length watch.laps then
                                Nothing
                            else
                                Just (v + 1)

                        Nothing ->
                            Just 0

                display : Watch -> Watch
                display watch =
                    if watch.id == id then
                        { watch | view = nv watch }
                    else
                        watch

                newTimes : List Watch
                newTimes =
                    List.map display model.times

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                    }
            in
                newModel ! []

        Reset ->
            init



-- Material Elements ---------------------------------------------------------


col : Msg -> String -> List (Html Msg) -> Html Msg
col msg styles content =
    div [ onClick msg, classList [ styles => True, "col" => True ] ]
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



-- Watch here is meant as Watch type


displayWatch : Time -> Watch -> Html Msg
displayWatch ct watch =
    let
        action : ID -> Msg
        action =
            if isNothing watch.end then
                Lap
            else
                Display

        time : Time
        time =
            case watch.view of
                Just v ->
                    0

                Nothing ->
                    0
    in
        col (action watch.id) "s6 flow-text" [ text ((toString (watch.id + 1)) ++ " | " ++ (displayTime time)) ]


displayTimes : Model -> Html Msg
displayTimes model =
    -- div [] [ (text << toString) model.times ]
    List.map (displayWatch model.time) model.times
        |> div []


displayAbsoluteTime : Time -> List Watch -> Html Msg
displayAbsoluteTime time times =
    case List.head (List.sortBy .id times) of
        Just ft ->
            displayTime (time - ft.start)
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
        col NoOp "s12" [ button (String.join " " [ "full-width", color ]) (toString i) (MaxTimes i) ]



-- View ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        unfinished : List Watch
        unfinished =
            List.filter (\t -> isNothing t.end) model.times

        app : List (Html Msg)
        app =
            [ section "valign-wrapper no-pad-bot"
                [ row "valign full-width center"
                    [ col NoOp "s6" [ p [ class "flow-text abs-time" ] [ displayAbsoluteTime model.time unfinished ] ]
                    , col NoOp "s6" [ p [ class "flow-text abs-time" ] [ (text << toString << List.length) unfinished ] ]
                    ]
                ]
            , section "no-pad-bot" [ row "valign full-width center large-line" [ displayTimes model ] ]
            , section "bottom full-width no-pad-bot"
                [ row "no-pad-bot no-margin-bot" [ col NoOp "s12 no-pad" [ button "black white-text full-width" "Reset" Reset ] ]
                , row "no-margin-bot"
                    [ col NoOp "s12 m12 l6 no-pad" [ button "white black-text full-width" "Start" Start ]
                    , col NoOp "s12 m12 l6 no-pad" [ button "black white-text full-width" "Stop" Stop ]
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
