module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (value, id)
import Random


type Model
    = Init
    | Win
    | Ongoing { target: Int, n: Int }

makeRandomInt : Cmd Msg
makeRandomInt = Random.generate MsgGotRandomInt (Random.int 100 1000)

init : () -> (Model, Cmd Msg)
init _ = (Init, makeRandomInt)

type Msg
    = MsgIncrease
    | MsgDecrease
    | MsgRestart
    | MsgGotRandomInt Int

view: Model -> Html Msg
view model =
    case model of
        Init ->
            text "Loading game..."
        Win ->
            div []
            [ text "You won!"
            , button [onClick MsgRestart] [text "Restart"]
            ]
        Ongoing data ->
            div []
            [ text ("Target: " ++ (String.fromInt data.target))
            , button [onClick MsgDecrease] [text "N // 2"]
            , text (String.fromInt data.n)
            , button [onClick MsgIncrease] [text "3N+1"]
            ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (MsgGotRandomInt rand, _) ->
            (Ongoing {target = rand, n = 1}, Cmd.none)
        (MsgIncrease, Ongoing data) ->
            if 3 * data.n + 1 == data.target then
                (Win, Cmd.none)
            else
                (Ongoing { data | n = data.n * 3 + 1 }, Cmd.none)
        (MsgDecrease, Ongoing data) ->
            if data.n // 2 == data.target then
                (Win, Cmd.none)
            else
                (Ongoing { data | n = data.n // 2 }, Cmd.none)
        (MsgRestart, _) ->
            init ()
        _ ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }