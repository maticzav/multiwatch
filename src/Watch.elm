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
    , parallels : Maybe Int
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
    | Parallels Int
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


take2 : List a -> Maybe ( a, a )
take2 list =
    case list of
        [] ->
            Nothing

        _ :: [] ->
            Nothing

        x :: xs :: _ ->
            Just ( x, xs )



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

        Parallels i ->
            let
                newModel : Model
                newModel =
                    { model
                        | parallels = Just i
                    }
            in
                newModel ! []

        Start ->
            let
                unfinished : List Watch
                unfinished =
                    List.filter (\t -> isNothing t.end) model.times

                newTimes : List Watch
                newTimes =
                    case model.parallels of
                        Just parallels ->
                            List.map (flip newWatch model.time) [model.current_id..(model.current_id + parallels - 1)] ++ model.times

                        Nothing ->
                            model.times

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
                        { watch | laps = watch.laps ++ [ model.time ] }
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
                            if (v + 1) > List.length watch.laps then
                                Nothing
                            else
                                Just (v + 1)

                        Nothing ->
                            if (not << List.isEmpty) watch.laps then
                                Just 0
                            else
                                Nothing

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
            , "btn btn-large" => True
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
            Time.inMilliseconds (time - (toFloat seconds) * Time.second - (toFloat minutes) * Time.minute)
                |> (flip (//) 10 << floor)
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

        laps : List Time
        laps =
            (List.map Just (watch.start :: watch.laps) ++ [ watch.end ])
                |> List.filterMap identity

        time : Time
        time =
            case watch.view of
                Just v ->
                    case take2 (List.drop v laps) of
                        Just ( i, f ) ->
                            (f - i)

                        Nothing ->
                            0

                Nothing ->
                    case watch.end of
                        Just et ->
                            (et - watch.start)

                        Nothing ->
                            (ct - watch.start)

        v =
            case watch.end of
                Just _ ->
                    case watch.view of
                        Just x ->
                            toString (x + 1)

                        Nothing ->
                            "•"

                Nothing ->
                    if (not << List.isEmpty) watch.laps then
                        (toString << List.length) watch.laps
                    else
                        ""
    in
        col (action watch.id) "s6 flow-text" [ text (String.join " " [ toString (watch.id + 1), v, "|", displayTime time ]) ]


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
        col NoOp "s12" [ button (String.join " " [ "full-width", color ]) (toString i) (Parallels i) ]



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
            , section "no-pad-bot" [ row "valign full-width center large-line" (List.map (displayWatch model.time) model.times) ]
            , section "bottom full-width no-pad-bot"
                [ row "no-margin-bot"
                    [ col NoOp "s6 no-pad" [ button "white black-text full-width" "Start" Start ]
                    , col NoOp "s6 no-pad" [ button "black white-text full-width" "Stop" Stop ]
                    ]
                ]
            ]

        setup : List (Html Msg)
        setup =
            [ section "valign-wrapper no-pad-bot white black-text"
                [ row "valign center container" [ p [ class "flow-text" ] [ text "How many people start at the same time?" ] ]
                ]
            , section "no-pad-top no-pad-bot" (List.map displayMaxTimesOption [1..8])
            ]

        content : List (Html Msg)
        content =
            case model.parallels of
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
