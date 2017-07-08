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
    | CreateUser UserData
    | UpdateInput String
    | SendTweets (List Tweet)
    | Like Tweet
    | Unlike Tweet
    | Block UserData
