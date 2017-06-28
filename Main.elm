module Main exposing (..)

import Html exposing (program)
import Model exposing (..)
import Msg exposing (Msg)
import Time exposing (Time, second)
import Update exposing (update, init)
import View exposing (view)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Issue either a Tick or a NoOp once a second depending on whether we're playing or not
    Time.every second
        (\_ ->
            case model.currentPage of
                PlayingGame ->
                    Msg.Tick

                _ ->
                    Msg.NoOp
        )
