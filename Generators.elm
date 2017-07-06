module Generators exposing (userGenerator, tweetGenerator)

import Array exposing (fromList, get)
import List.Nonempty as NE exposing (Nonempty, sample, (:::))
import Random exposing (..)
import Tweet exposing (..)
import User exposing (..)


-- Tweet generators


type TweetType
    = StraightInsult
    | SuggestionWithInsult
    | TrumpSlogan
    | RandomNonsense


slogans =
    NE.Nonempty "maga"
        [ "jobs jobs jobs"
        , "high energy"
        , "make america great again"
        ]


insultNounPhrases =
    NE.Nonempty "colossal fucking idiot"
        [ "misogynistic jerk"
        , "disgrace to our country"
        , "fucking lunatic"
        , "crazy whackjob"
        , "disgusting man"
        , "puppet asshole"
        , "ravenous cocksocket"
        , "fat, out of shape sack of garbage"
        , "absolute disgrace"
        , "failing pile of garbage"
        ]


suggestionClauses =
    NE.Nonempty "go fuck yourself"
        [ "resign"
        , "stop tweeting"
        , "shut up"
        , "delete your account"
        , "fire up the cheeto dust cannons"
        ]


laughter =
    NE.Nonempty "😂😂😂sweet baby Jesus" [ "HAHAHAHAHA" ]


boringTweets =
    NE.Nonempty "i dont know why im here"
        [ "you are not my daddy" ]


tweetFromType : TweetType -> Generator String
tweetFromType tweetType =
    case tweetType of
        StraightInsult ->
            Random.map ((++) "You ") (sample insultNounPhrases)

        SuggestionWithInsult ->
            Random.map2 (\x -> \y -> x ++ ", you " ++ y)
                (sample suggestionClauses)
                (sample insultNounPhrases)

        TrumpSlogan ->
            sample slogans

        RandomNonsense ->
            sample laughter


npcTextGenerator : Alignment -> Generator String
npcTextGenerator alignment =
    let
        magaTweetTypes =
            NE.Nonempty TrumpSlogan []

        resistTweetTypes =
            NE.Nonempty StraightInsult [ SuggestionWithInsult ]

        boringTweetTypes =
            NE.Nonempty RandomNonsense []

        pickTweetTypes =
            case alignment of
                User.Maga ->
                    magaTweetTypes

                User.Resist ->
                    resistTweetTypes

                User.Boring ->
                    boringTweetTypes
    in
        sample pickTweetTypes
            |> Random.andThen tweetFromType


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
