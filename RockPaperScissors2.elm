module RockPaperScissors exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random

main = Browser.element 
        {
            init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
        }

type alias Model =
    {
        player2 : Int,
        outcome : String
    }

init : () -> (Model, Cmd Msg)
init _ = 
    (
        {
            player2 = 0,
            outcome = ""
        },
        Random.generate Draw (Random.int 1 3)
    )

type Msg = 
    Bet Int |
    Draw Int 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Bet i ->
                if i == 1 && model.player2 == 1 then
                    ({model | outcome = "player 1 is rock and player 2 is rock -> draw"}, Random.generate Draw (Random.int 1 3))
                else if i == 1 && model.player2 == 2 then
                    ({model | outcome = "player 1 is rock and player 2 is paper -> player 2 wins"}, Random.generate Draw (Random.int 1 3))
                else if i == 1 && model.player2 == 3 then
                    ({model | outcome = "player 1 is rock and player 2 is scissors -> player 1 wins"}, Random.generate Draw (Random.int 1 3))
                else if i == 2 && model.player2 == 1 then
                    ({model | outcome = "player 1 is paper and player 2 is rock -> player 1 wins"}, Random.generate Draw (Random.int 1 3))
                else if i == 2 && model.player2 == 2 then
                    ({model | outcome = "player 1 is paper and player 2 is paper -> draw"}, Random.generate Draw (Random.int 1 3))
                else if i == 2 && model.player2 == 3 then
                    ({model | outcome = "player 1 is paper and player 2 is scissors -> player 2 wins"}, Random.generate Draw (Random.int 1 3))
                else if i == 3 && model.player2 == 1 then
                    ({model | outcome = "player 1 is scissors and player 2 is rock -> player 2 wins"}, Random.generate Draw (Random.int 1 3))
                else if i == 3 && model.player2 == 2 then
                    ({model | outcome = "player 1 is scissors and player 2 is paper -> player 1 wins"}, Random.generate Draw (Random.int 1 3))
                else
                    ({model | outcome = "player 1 is scissors and player 2 is scissors -> draw"}, Random.generate Draw (Random.int 1 3))
        
        Draw i ->
            (
                {model | player2 = i},
                Cmd.none
            )

view : Model -> Html Msg
view model =
    pre []
    [
        button [ onClick (Bet 1)] [ text "Rock" ],
        button [ onClick (Bet 2)] [ text "Paper" ],
        button [ onClick (Bet 3)] [ text "Scissors" ],
        text ("\n" ++ model.outcome)
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none