module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { player1Score : Int
    , player2Score : Int
    , player1Choice : String
    , player2Choice : String
    }

init : Model
init =
    { player1Score = 0
    , player2Score = 0
    , player1Choice = ""
    , player2Choice = ""
    }

type Msg
    = MsgClickedPlayer1A
    | MsgClickedPlayer1B
    | MsgClickedPlayer2A
    | MsgClickedPlayer2B
    | MsgContinue
    | MsgReset


update : Msg -> Model -> Model
update msg model =
    let
        updateScoreOfWinner : Model -> Model
        updateScoreOfWinner oldModel =
            if oldModel.player1Choice /= "" && oldModel.player2Choice /= "" then
                -- We can determine the winner
                if oldModel.player1Choice == oldModel.player2Choice then
                    -- Player 1 is the winner
                    { oldModel | player1Score = oldModel.player1Score + 1 }
                else
                    -- Player 2 is the winner
                    { oldModel | player2Score = oldModel.player2Score + 1 }
            else
                -- No winner yet
                oldModel
    in
    case msg of
        MsgClickedPlayer1A ->
            { model | player1Choice = "A" }
            |> updateScoreOfWinner

        MsgClickedPlayer1B ->
            { model | player1Choice = "B" }
            |> updateScoreOfWinner

        MsgClickedPlayer2A ->
            { model | player2Choice = "A" }
            |> updateScoreOfWinner

        MsgClickedPlayer2B ->
            { model | player2Choice = "B" }
            |> updateScoreOfWinner

        MsgContinue ->
            { model | player1Choice = "", player2Choice = "" }
            
        MsgReset ->
            init

view : Model -> Html Msg
view model =
    let
        getWinner : String -> String -> Int
        getWinner p1 p2 =
            if p1 == p2 then
                1
            else
                2

        player1Elements : List (Html Msg)
        player1Elements =
            if model.player1Choice == "" then
                [ button [onClick MsgClickedPlayer1A] [text "A"]
                , button [onClick MsgClickedPlayer1B] [text "B"]
                ]
            else
                [ text " is done" ]
    
        player2Elements =
            if model.player2Choice == "" then
                [ button [onClick MsgClickedPlayer2A] [text "A"]
                , button [onClick MsgClickedPlayer2B] [text "B"]
                ]
            else
                [ text " is done" ]
            
        children =
            [ div [] ([ text "Player 1 "
                     , text ("(" ++ (String.fromInt model.player1Score) ++ ")")
                     ] ++ player1Elements)
            , div [] ([ text "Player 2 "
                     , text ("(" ++ (String.fromInt model.player2Score) ++ ")")
                     ] ++ player2Elements)
            ]
    in
    if model.player1Score == 3 then
        div [] [ text "Player 1 won"
               , button [onClick MsgReset] [text "Reset"]
               ]

    else if model.player2Score == 3 then
        div [] [ text "Player 2 won"
               , button [onClick MsgReset] [text "Reset"]
               ]

    else if model.player1Choice /= "" && model.player2Choice /= "" then
        -- Both players have chosen
        let
            winner = getWinner model.player1Choice model.player2Choice
        in
        div []
            [ div [] [text ("Player " ++ (String.fromInt winner) ++ " wins")]
            , div [] [text ("Player 1: " ++ model.player1Choice)]
            , div [] [text ("Player 2: " ++ model.player2Choice)]
            , button [onClick MsgContinue] [text "Continue"] 
            ]
    else
        -- Still waiting for player choice/s
        div [] children


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }