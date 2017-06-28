module Generators exposing (userGenerator, tweetGenerator)

import Array exposing (fromList, get)
import List.Nonempty as NE exposing (Nonempty, sample, (:::))
import Random exposing (..)
import Tweet exposing (..)
import User exposing (..)


-- Tweet generators


magaTweets =
    NE.Nonempty "You tell em Mr President sir!!"
        [ "fuck me daddy"
        , "build the wall mr president"
        ]


resistTweets =
    NE.Nonempty "LOL COVFEFE YOU IDIOT DUMMY MORON, GO BACK TO RUSSIA"
        [ "you are a total dumbfuck"
        , "fuck me harder daddy"
        ]


boringTweets =
    NE.Nonempty "i dont know why im here"
        [ "you are not my daddy" ]


npcTextGenerator : Alignment -> Generator String
npcTextGenerator alignment =
    case alignment of
        User.Maga ->
            sample magaTweets

        User.Resist ->
            sample resistTweets

        User.Boring ->
            sample boringTweets


tweetGenerator : UserData -> Generator Tweet
tweetGenerator data =
    Random.map (makeTweet (User.NPC data)) (npcTextGenerator data.alignment)



-- User generators


nickList =
    NE.Nonempty "William Legate" []


handleList =
    NE.Nonempty "williamlegate" []


aviList =
    NE.Nonempty "" []


alignmentList =
    NE.Nonempty User.Maga [ User.Resist, User.Resist, User.Boring ]


userGenerator : Generator UserData
userGenerator =
    Random.map5 createUser
        (sample nickList)
        (sample handleList)
        (sample aviList)
        (sample alignmentList)
        (float 0 1)
