module AddOne exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
    Browser.element
        {
            init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
        }

type Model =
    Exit |
    Ongoing 
        {
            userInput : String,
            added : Int
        }

init : () -> (Model, Cmd Msg)
init _ =
    (
        Ongoing {
                userInput = "",
                added = 0
            },
        Cmd.none
    )

type Msg =
    UserInputting String |
    Add |
    Exiting

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (UserInputting s, Ongoing data) ->
            (
                Ongoing {data | userInput = s},
                Cmd.none
            )
        (Add, Ongoing data) ->
            case (String.toInt data.userInput) of
                Just x ->
                    (
                        Ongoing {data | added = x + 1},
                        Cmd.none
                    )
                Nothing ->
                    (model, Cmd.none)
        (Exiting, Ongoing data) ->
            (Exit, Cmd.none)
        (_, _) ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
    case model of
        Ongoing data ->
            pre []
            [
                input [type_ "text", placeholder "enter input", value data.userInput, onInput UserInputting] [],
                button [onClick Add] [text "ADD"],
                text ("\nUser Input :" ++ data.userInput),
                text ("\nAdded : " ++ (String.fromInt data.added)),
                text "\n",
                button [onClick Exiting] [text "EXIT"]
            ]
        Exit ->
            pre []
            [
                text "byebye"
            ]