module Model exposing (..)

import Tweet exposing (Tweet)
import User exposing (UserData)


type Page
    = GameOver
    | BeforeRound
    | PlayingGame


type alias Model =
    { uid : Int
    , currentInput : String
    , unseenTimeline : List Tweet
    , timeline : List Tweet
    , users : List UserData
    , score : Int
    , health : Int
    , roundNumber : Int
    , currentPage : Page
    }
