module Main exposing (main)

import Browser
import Html exposing (Html, div)

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
    { activeTasks : List ActiveTask
    }

type TimeDelta =
    TimeDelta Hours Minutes Seconds


type Hours =
    Hours Int

type Minutes =
    Minutes Int

type Seconds =
    Seconds Int


type ActiveTask =
    ActiveTask
        { timeTaken : TimeDelta
        }


init : () -> (Model, Cmd Msg)
init flags =
    ({ a = 12 }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model = div [] []


-- UPDATE

type Msg =
    Placeholder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model
    = Sub.none
