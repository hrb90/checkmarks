module Msg exposing (..)

import String
import Tweet exposing (Tweet)
import User exposing (UserData)


type Msg
    = NoOp
    | Reset
    | Tick
    | ShowMoreTweets
    | StartRound
    | EndRound
    | EndGame
    | CreateUser UserData
    | UpdateInput String
    | SendTweet Tweet
    | Like Tweet
    | Unlike Tweet
    | Block UserData
