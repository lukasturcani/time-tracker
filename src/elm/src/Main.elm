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
    , index : Int
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
                [ { name = "", timeTaken = Seconds 0, active = False, index = 0 }
                , { name = "", timeTaken = Seconds 0, active = False, index = 1 }
                , { name = "", timeTaken = Seconds 0, active = False, index = 2 }
                , { name = "", timeTaken = Seconds 0, active = False, index = 3 }
                , { name = "", timeTaken = Seconds 0, active = False, index = 4 }
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
    Widget.sortTable
        (Material.sortTable Material.darkPalette)
        { content =
            model.tasks
        , columns =
            [ Widget.elementColumn
                { title = "Activity"
                , value = \task ->
                    Widget.textInput
                        (Material.textInput Material.defaultPalette)
                        { chips = []
                        , text = task.name
                        , placeholder = Nothing
                        , label = "activity"
                        , onChange = SetActivityName task.index
                        }

                , width = Element.fill
                }
            , Widget.stringColumn
                { title = "Time"
                , value = .timeTaken >> formatSeconds
                , toString = identity
                , width = Element.fill
                }
            , Widget.elementColumn
                { title = ""
                , value = \task ->
                    Element.el
                        []
                        (Widget.switch
                            (Material.switch Material.darkPalette)
                            { description = "active"
                            , onPress = Just <| ToggleActive task.index
                            , active = task.active
                            }
                        )
                , width = Element.fill
                }
            ]
        , sortBy =
            "Time"
        , asc = True
        , onChange = ChangedSorting
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
    | ChangedSorting String
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
                    List.map
                        (\task ->
                            { task
                                | active =
                                    if toggledIndex == task.index then
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
                    List.map
                        (\task ->
                            { task
                                | name =
                                    if task.index == activityIndex then
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
                             , index = List.length model.tasks
                             }
                           ]
              }
            , Cmd.none
            )

        ChangedSorting _ ->
            ( model, Cmd.none )


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
