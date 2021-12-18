module Main exposing (main)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Json.Decode as Json
import Json.Encode
import String
import Time
import Widget
import Widget.Material as Material


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentTime : Time.Posix
    , tasks : List Task
    , timeZone : Time.Zone
    }


type Seconds
    = Seconds Int


addSeconds : Seconds -> Seconds -> Seconds
addSeconds (Seconds a) (Seconds b) =
    Seconds <| a + b


formatSeconds : Seconds -> String
formatSeconds (Seconds seconds) =
    let
        hours =
            seconds // 3600

        hoursInSeconds =
            hours * 3600

        minutes =
            (seconds - hoursInSeconds) // 60

        minutesInSeconds =
            minutes * 60
    in
    String.concat
        [ String.fromInt >> String.padLeft 2 '0' <| hours
        , ":"
        , String.fromInt >> String.padLeft 2 '0' <| minutes
        , ":"
        , String.fromInt
            >> String.padLeft 2 '0'
          <|
            seconds
                - hoursInSeconds
                - minutesInSeconds
        ]


type alias Task =
    { timeTaken : Seconds
    , name : String
    , active : Bool
    }


init : Json.Value -> ( Model, Cmd Msg )
init flags =
    let
        currentTimeParser =
            Json.field "currentTime" Json.int

        timeOffsetParser =
            Json.field "timeOffset" Json.int

        model =
            { currentTime =
                case Json.decodeValue currentTimeParser flags of
                    Ok value ->
                        Time.millisToPosix value

                    Err err ->
                        Time.millisToPosix 0
            , tasks =
                [ { name = "", timeTaken = Seconds 0, active = False }
                , { name = "", timeTaken = Seconds 0, active = False }
                , { name = "", timeTaken = Seconds 0, active = False }
                , { name = "", timeTaken = Seconds 0, active = False }
                , { name = "", timeTaken = Seconds 0, active = False }
                ]
            , timeZone =
                Time.customZone
                    (case Json.decodeValue timeOffsetParser flags of
                        Ok value ->
                            value

                        Err err ->
                            0
                    )
                    []
            }
    in
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Element.column
            [ Element.width Element.fill
            , Element.spacing 30
            ]
            [ formatTime model.timeZone
                >> Element.text
                >> Element.el [ Element.centerX, Element.alignTop ]
              <|
                model.currentTime
            , viewTable model
            , addRowButton
            ]
        )


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    String.concat
        [ Time.toHour zone
            >> String.fromInt
            >> String.padLeft 2 '0'
          <|
            time
        , ":"
        , Time.toMinute zone
            >> String.fromInt
            >> String.padLeft 2 '0'
          <|
            time
        , ":"
        , Time.toSecond zone
            >> String.fromInt
            >> String.padLeft 2 '0'
          <|
            time
        ]


viewTable : Model -> Element.Element Msg
viewTable model =
    Element.indexedTable
        []
        { data = model.tasks
        , columns =
            [ { header =
                    Element.el
                        [ Element.centerX
                        ]
                        (Element.text "Activity")
              , width = Element.fill
              , view =
                    \index task ->
                        Element.Input.text
                            []
                            { onChange = SetActivityName index
                            , text = task.name
                            , placeholder = Nothing
                            , label =
                                Element.Input.labelHidden "activity"
                            }
              }
            , { header = Element.text "Time"
              , width = Element.fill
              , view =
                    \index task ->
                        formatSeconds
                            >> Element.text
                        <|
                            task.timeTaken
              }
            , { header = Element.text ""
              , width = Element.fill
              , view =
                    \index task ->
                        Widget.switch
                            (Material.switch Material.darkPalette)
                            { description = "active"
                            , onPress = Just <| ToggleActive index
                            , active = task.active
                            }
              }
            ]
        }


addRowButton : Element.Element Msg
addRowButton =
    Widget.textButton
        (Material.containedButton Material.darkPalette)
        { onPress = Just AddRow
        , text = "add row"
        }



-- UPDATE


type Msg
    = SetCurrentTime Time.Posix
    | SetTimeZone Time.Zone
    | ToggleActive Int
    | SetActivityName Int String
    | AddRow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCurrentTime currentTime ->
            ( { model
                | currentTime = currentTime
                , tasks =
                    updateTasks
                        (timeDifference model.currentTime currentTime)
                        model.tasks
              }
            , Cmd.none
            )

        SetTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        ToggleActive toggledIndex ->
            ( { model
                | tasks =
                    List.indexedMap
                        (\index task ->
                            { task
                                | active =
                                    if toggledIndex == index then
                                        not task.active

                                    else
                                        task.active
                            }
                        )
                        model.tasks
              }
            , Cmd.none
            )

        SetActivityName activityIndex name ->
            ( { model
                | tasks =
                    List.indexedMap
                        (\index task ->
                            { task
                                | name =
                                    if index == activityIndex then
                                        name

                                    else
                                        task.name
                            }
                        )
                        model.tasks
              }
            , Cmd.none
            )

        AddRow ->
            ( { model
                | tasks =
                    model.tasks
                        ++ [ { active = False
                             , name = ""
                             , timeTaken = Seconds 0
                             }
                           ]
              }
            , Cmd.none
            )


updateTasks : Seconds -> List Task -> List Task
updateTasks seconds tasks =
    List.map
        (\task ->
            { task
                | timeTaken =
                    if task.active then
                        addSeconds seconds task.timeTaken

                    else
                        task.timeTaken
            }
        )
        tasks


timeDifference : Time.Posix -> Time.Posix -> Seconds
timeDifference from to =
    abs
        >> Seconds
    <|
        (Time.posixToMillis to - Time.posixToMillis from)
            // 1000



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 SetCurrentTime
