module Tweet exposing (..)

import User exposing (..)


type alias Tweet =
    { id : Int
    , content : String
    , user : User
    , liked : Bool
    }



-- Constructors


makeTweet : User -> String -> Tweet
makeTweet user str =
    { id = 0
    , content = str
    , user = user
    , liked = False
    }



--


resistanceTweet : Tweet -> Bool
resistanceTweet tweet =
    case tweet.user of
        Player ->
            False

        NPC data ->
            resists data


getRep : Tweet -> Float
getRep tweet =
    case tweet.user of
        Player ->
            0.0

        NPC data ->
            data.reputation
