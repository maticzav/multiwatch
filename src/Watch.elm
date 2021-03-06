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
    = ScreenStopwatch
    | ScreenSwitch
    | ScreenSettings
    | ScreenIntro
    | ScreenAbout


type alias Watch =
    { id : ID
    , order : Int
    , start : Time
    , laps : List Time
    , end : Maybe Time
    , view : Maybe Int
    }


type alias Settings =
    { startLstopR : Bool
    , parallels : Int
    }


type alias Model =
    { screen : Screen
    , time : Time
    , times : List Watch
    , switch : Maybe Int
    , settings : Settings
    }



-- Init ----------------------------------------------------------------------


newWatch : Time -> ID -> Int -> Watch
newWatch time id ord =
    Watch
        id
        ord
        time
        []
        Nothing
        Nothing


defaultModel : Model
defaultModel =
    Model
        ScreenIntro
        0
        []
        Nothing
        (Settings False 2)


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
    | Clear
    | Lap ID
    | Display ID
    | Switch (Maybe ID) ID
    | SwitchScreen Screen



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


addBeforeLast : List a -> a -> List a
addBeforeLast l el =
    List.concat [ List.take (List.length l - 1) l, [ el ], List.drop (List.length l - 1) l ]


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xBack, y :: yBack ) ->
            ( x, y ) :: zip xBack yBack

        ( _, _ ) ->
            []



-- Subscriptions -------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick



-- Update --------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        NoOp ->
            model ! []

        Tick newTime ->
            { model | time = newTime } ! []

        Parallels i ->
            let
                newSettings : Settings
                newSettings =
                    { settings
                        | parallels = i
                    }

                newModel : Model
                newModel =
                    { model
                        | settings = newSettings
                        , times = []
                    }
            in
                newModel ! []

        Start ->
            let
                unfinished : List Watch
                unfinished =
                    List.filter (\t -> isNothing t.end) model.times

                next_id : ID
                next_id =
                    case List.maximum (List.map .id model.times) of
                        Just max ->
                            max + 1

                        Nothing ->
                            0

                next_order : ID
                next_order =
                    case List.maximum (List.map .order model.times) of
                        Just max ->
                            max + 1

                        Nothing ->
                            0

                newTimesIDRange : List Int
                newTimesIDRange =
                    [next_id..(next_id + settings.parallels - 1)]

                newTimesOrderRange : List Int
                newTimesOrderRange =
                    [next_order..(next_order + settings.parallels - 1)]

                newTimes : List Watch
                newTimes =
                    (List.map (uncurry (newWatch model.time)) (zip newTimesIDRange newTimesOrderRange)) ++ model.times

                max_id : Int
                max_id =
                    case List.maximum (List.map .id newTimes) of
                        Just max ->
                            max

                        Nothing ->
                            next_id

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                    }
            in
                newModel ! []

        Stop ->
            let
                unfinished : List Watch
                unfinished =
                    List.filter (isNothing << .end) model.times

                end : Int -> Watch -> Watch
                end order watch =
                    if watch.order == order then
                        { watch | end = Just model.time }
                    else
                        watch

                newTimes : List Watch
                newTimes =
                    case List.head (List.sortBy .order unfinished) of
                        Just watch ->
                            List.map (end watch.order) model.times

                        Nothing ->
                            model.times

                newModel : Model
                newModel =
                    { model
                        | times = newTimes
                    }
            in
                newModel ! []

        Clear ->
            let
                newModel : Model
                newModel =
                    { model
                        | times = []
                        , switch = Nothing
                        , screen = ScreenStopwatch
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
                switch : Int -> Int -> Watch -> Watch
                switch fs sn w =
                    if w.order == fs || w.order == sn then
                        if w.order == fs then
                            { w | order = sn }
                        else
                            { w | order = fs }
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
                            ScreenStopwatch

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

        SwitchScreen screen ->
            let
                newModel : Model
                newModel =
                    { model
                        | screen = screen
                        , switch = Nothing
                    }
            in
                newModel ! []



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
            [ color => True ]
        ]
        content


icon : Msg -> String -> String -> Html Msg
icon onclick styl icon =
    i [ onClick onclick, class ("material-icons " ++ styl) ] [ text icon ]


divider : String -> Html Msg
divider styl =
    div [ classList [ "divider" => True, styl => True ] ] []



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
            |> (if miliseconds < 10 then
                    String.toList >> flip addBeforeLast '0' >> String.fromList
                else
                    identity
               )



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
            if i == model.settings.parallels then
                "red white-text"
            else
                "white black-text"
    in
        col NoOp "s2" [ button (String.join " " [ "center", "btn-floating", color ]) [ (text << toString) i ] (Parallels i) ]


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
            case model.switch of
                Just id ->
                    if id == w.id then
                        "blue white-text"
                    else
                        "white black-text"

                Nothing ->
                    "white black-text"

        colSize : String
        colSize =
            String.join "" [ "s", toString <| 12 // model.settings.parallels ]

        btnStyle : String
        btnStyle =
            String.join " " [ "btn-floating", color ]
    in
        col NoOp colSize [ button btnStyle [ (text << toString) (w.id + 1) ] (Switch model.switch w.id) ]



-- Screens -------------------------------------------------------------------


stopwatch : Model -> List (Html Msg)
stopwatch model =
    let
        unfinished : List Watch
        unfinished =
            List.filter (isNothing << .end) model.times

        times : List (Html Msg)
        times =
            List.sortBy .order model.times
                |> List.reverse
                |> List.map (displayWatch model.time)

        info : List (Html Msg)
        info =
            [ col (SwitchScreen ScreenSettings) "s6" [ p [ class "flow-text large-text" ] [ displayAbsoluteTime model.time unfinished ] ]
            , col (SwitchScreen ScreenSwitch) "s6" [ p [ class "flow-text large-text" ] [ (text << toString) model.settings.parallels ] ]
            ]

        navButtons : List (Html Msg)
        navButtons =
            [ col NoOp "s6 no-pad" [ button "btn btn-large btn-no-radius white black-text full-width" [ text "Start" ] Start ]
            , col NoOp "s6 no-pad" [ button "btn btn-large btn-no-radius black white-text full-width" [ text "Stop" ] Stop ]
            ]
                |> (if model.settings.startLstopR then
                        identity
                    else
                        List.reverse
                   )
    in
        [ section "full-width valign-wrapper black white-text no-pad-bot flex-start"
            [ row "valign full-width center" (info) ]
        , section "scroll-wrapper flex-fill" [ row "valign full-width center large-line" times ]
        , section "full-width no-pad-bot flex-end"
            [ row "" (navButtons) ]
        ]


switch : Model -> List (Html Msg)
switch model =
    let
        instructionText : String
        instructionText =
            case model.switch of
                Just fs ->
                    String.join " " [ "Select the change for", toString (fs + 1), "!" ]

                Nothing ->
                    "Chose which time would you like to switch!"

        times : List Watch
        times =
            List.filter (isNothing << .end) model.times
                |> List.sortBy .id
                |> List.reverse
    in
        [ section "full-width white black-text container flex-start"
            [ row "center" [ p [ class "flow-text" ] [ text instructionText ] ]
            , divider "container"
            ]
        , section "full-width no-pad-top no-pad-bot flex-fill scroll-wrapper container"
            [ row "center large-line" (List.map (displaySwitchOptions model) times) ]
        , section "full-width center flex-end"
            [ button "btn btn-floating black white-text" [ icon (NoOp) "" "clear" ] (SwitchScreen ScreenStopwatch) ]
        ]


settings : Model -> List (Html Msg)
settings model =
    [ section "no-pad-bot white black-text container flex-start"
        [ row "center" [ p [ class "flow-text large-text center" ] [ text "Settings" ] ]
        , divider "container"
        ]
    , section "no-pad-bot white black-text container flex-fill"
        [ row "center" [ p [ class "flow-text" ] [ text "Clear all the current times!" ] ]
        , row "center" [ button "btn black white-text" [ text "Clear" ] Clear ]
        , divider "container"
        , row "center" [ p [ class "flow-text" ] [ text "How many people start at the same time?" ] ]
        , row "center" (List.map (displayParallelsOption model) [ 1, 2, 3, 4, 5, 6 ])
        , divider "container"
        , row "center" [ p [ class "flow-text" ] [ text "Replay intro." ] ]
        , row "center" [ button "btn black white-text" [ text "Intro" ] (SwitchScreen ScreenIntro) ]
        , divider "container"
        , row "center" [ p [ class "flow-text" ] [ text "About Me." ] ]
        , row "center" [ button "btn black white-text" [ text "About Me" ] (SwitchScreen ScreenAbout) ]
        , divider "container"
        ]
    , section "full-width center flex-end"
        [ button "btn btn-floating black white-text" [ icon (NoOp) "" "clear" ] (SwitchScreen ScreenStopwatch) ]
    ]


intro : Model -> List (Html Msg)
intro model =
    let
        infoText : String
        infoText =
            """
                Press on the 'Start' text to enter Settings or
                '3' to enter Switch screen.
            """

        info : List (Html Msg)
        info =
            [ col (SwitchScreen ScreenSettings) "s6" [ p [ class "flow-text large-text" ] [ text "Settings" ] ]
            , col (SwitchScreen ScreenSwitch) "s6" [ p [ class "flow-text large-text" ] [ text "Switch" ] ]
            ]
    in
        [ section "full-width valign-wrapper black white-text no-pad-bot flex-start"
            [ row "valign full-width center" (info) ]
        , section "no-pad-bot white black-text flex-start full-width"
            [ row "full-width center" [ p [ class "flow-text large-text container center" ] [ text infoText ] ]
            ]
        , section "full-width center flex-bot white"
            [ button "btn btn-floating black" [ icon (NoOp) "white-text" "clear" ] (SwitchScreen ScreenStopwatch) ]
        ]


about : Model -> List (Html Msg)
about model =
    let
        aboutText : String
        aboutText =
            """
                About Me!


            """
    in
        [ section "no-pad-bot white black-text container flex-start"
            [ row "center" [ p [ class "flow-text large-text center" ] [ text "About" ] ]
            , divider "container"
            ]
        , section "scroll-wrapper flex-fill" [ row "valign container center large-line flow-text" [ text aboutText ] ]
        , section "full-width center flex-end"
            [ button "btn btn-floating black white-text" [ icon (NoOp) "" "clear" ] (SwitchScreen ScreenStopwatch) ]
        ]



-- View ----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        content : Model -> List (Html Msg)
        content =
            case model.screen of
                ScreenStopwatch ->
                    stopwatch

                ScreenSwitch ->
                    switch

                ScreenSettings ->
                    settings

                ScreenIntro ->
                    intro

                ScreenAbout ->
                    about
    in
        div [ class "flex-container white black-text" ] (content model)



-- App -----------------------------------------------------------------------


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ---------------------------------------------------------------------------
