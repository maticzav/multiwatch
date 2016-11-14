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


type Screen
    = Stopwatch
    | OrderSwitch
    | Settings
    | About


type alias Watch =
    { id : ID
    , start : Time
    , laps : List Time
    , end : Maybe Time
    , view : Maybe Int
    }


type alias Model =
    { screen : Screen
    , current_id : ID
    , time : Time
    , times : List Watch
    , parallels : Int
    , switch : Maybe Int
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
        Stopwatch
        0
        0
        []
        3
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
    | Switch (Maybe ID) ID
    | SwitchSreen Screen



-- Additional Functions ------------------------------------------------------


isEven : Int -> Bool
isEven i =
    i `rem` 2 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


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
            { model | time = newTime } ! []

        Parallels i ->
            let
                newModel : Model
                newModel =
                    { model
                        | parallels = i
                        , current_id = 0
                        , times = []
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
                    List.map (flip newWatch model.time) [model.current_id..(model.current_id + model.parallels - 1)] ++ model.times

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

        Switch fs sn ->
            let
                switch : ID -> ID -> Watch -> Watch
                switch fs sn w =
                    if w.id == fs || w.id == sn then
                        if w.id == fs then
                            { w | id = sn }
                        else
                            { w | id = fs }
                    else
                        w

                newTimes : List Watch
                newTimes =
                    case model.switch of
                        Just fs ->
                            List.map (switch fs sn) model.times

                        Nothing ->
                            model.times

                newSwitch : Maybe Int
                newSwitch =
                    case model.switch of
                        Just fs ->
                            Nothing

                        Nothing ->
                            Just sn

                newScreen : Screen
                newScreen =
                    case model.switch of
                        Just fs ->
                            Stopwatch

                        Nothing ->
                            model.screen

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                        , switch = newSwitch
                        , screen = newScreen
                    }
            in
                newModel ! []

        SwitchSreen screen ->
            { model | screen = screen } ! []



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


button : String -> List (Html Msg) -> Msg -> Html Msg
button color content msg =
    a
        [ href "#!"
        , onClick msg
        , classList
            [ color => True
            , "btn" => True
            ]
        ]
        content


icon : Msg -> String -> String -> Html Msg
icon onclick styl icon =
    i [ onClick onclick, class ("material-icons " ++ styl) ] [ text icon ]



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


displayParallelsOption : Model -> Int -> Html Msg
displayParallelsOption model i =
    let
        color : String
        color =
            if i == model.parallels then
                "red white-text"
            else if isOdd i then
                "black white-text"
            else
                "white black-text"
    in
        col NoOp "s12" [ button (String.join " " [ "full-width", "btn-xlarge", color ]) [ (text << toString) i ] (Parallels i) ]


displaySwitchText : Model -> Html Msg
displaySwitchText model =
    case model.switch of
        Just fs ->
            text <| String.join " " [ "Select the change for", toString (fs + 1), "!" ]

        Nothing ->
            text "Chose which time would you like to switch!"


displaySwitchOptions : Model -> Watch -> Html Msg
displaySwitchOptions model w =
    let
        color : String
        color =
            if isOdd w.id then
                "black white-text"
            else
                "white black-text"
    in
        col NoOp "s12" [ button (String.join " " [ "full-width", "btn-xlarge", color ]) [ (text << toString) (w.id + 1) ] (Switch model.switch w.id) ]



-- View ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        unfinished : List Watch
        unfinished =
            List.filter (\t -> isNothing t.end) model.times

        times : List (Html Msg)
        times =
            List.sortBy .id model.times
                |> List.reverse
                |> List.map (displayWatch model.time)

        stopwatch : List (Html Msg)
        stopwatch =
            [ section "valign-wrapper no-pad-bot"
                [ row "valign full-width center"
                    [ col (SwitchSreen Settings) "s6" [ p [ class "flow-text abs-time" ] [ displayAbsoluteTime model.time unfinished ] ]
                    , col (SwitchSreen OrderSwitch) "s6" [ p [ class "flow-text abs-time" ] [ (text << toString) model.parallels ] ]
                    ]
                ]
            , section "no-pad-bot scroll-wrapper" [ row "valign full-width center large-line" times ]
            , section "bottom full-width no-pad-bot"
                [ row "no-margin-bot"
                    [ col NoOp "s6 no-pad" [ button "btn-large white black-text full-width" [ text "Start" ] Start ]
                    , col NoOp "s6 no-pad" [ button "btn-large black white-text full-width" [ text "Stop" ] Stop ]
                    ]
                ]
            ]

        switch : List (Html Msg)
        switch =
            [ section "valign-wrapper no-pad-bot white black-text"
                [ row "valign center container" [ p [ class "flow-text" ] [ displaySwitchText model ] ]
                ]
            , section "no-pad-top no-pad-bot" (List.map (displaySwitchOptions model) (List.reverse <| List.sortBy .id <| List.filter (isNothing << .end) model.times))
            , section "no-pad-top no-pad-bot"
                [ row "center" [ col NoOp "s12 no-pad" [ button "btn btn-xlarge white black-text full-width" [ icon (NoOp) "center" "clear" ] (SwitchSreen Stopwatch) ] ] ]
            ]

        settings : List (Html Msg)
        settings =
            [ section "valign-wrapper no-pad-bot white black-text"
                [ row "valign center container" [ p [ class "flow-text" ] [ text "How many people start at the same time?" ] ]
                ]
            , section "no-pad-top no-pad-bot" (List.map (displayParallelsOption model) [1..8])
            , section "no-pad-top no-pad-bot"
                [ row "center" [ col NoOp "s12 no-pad" [ button "btn btn-xlarge white black-text full-width" [ icon (NoOp) "center" "clear" ] (SwitchSreen Stopwatch) ] ] ]
            ]

        about : List (Html Msg)
        about =
            []

        content : List (Html Msg)
        content =
            case model.screen of
                Stopwatch ->
                    stopwatch

                OrderSwitch ->
                    switch

                Settings ->
                    settings

                About ->
                    about
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
