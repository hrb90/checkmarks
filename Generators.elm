module Generators exposing (..)

import Array exposing (fromList, get)
import Random exposing (..)
import Tweet exposing (..)
import User exposing (..)

-- Helper functions

indexWithDefault : a -> List a -> Int -> a
indexWithDefault default l idx =
  Maybe.withDefault default (get idx (fromList l))

pickFromList : a -> List a -> Generator a
pickFromList default l =
  Random.map (indexWithDefault default l) (int 0 (List.length l - 1))

-- Tweet generators

magaTweets : List String
magaTweets = [ "You tell em Mr President sir!!"
             , "fuck me daddy"
             , "build the wall mr president"
             ]

resistTweets : List String
resistTweets = [ "LOL COVFEFE YOU IDIOT DUMMY MORON, GO BACK TO RUSSIA"
               , "you are a total dumbfuck"
               , "fuck me harder daddy"
               ]

boringTweets : List String
boringTweets = [ "i dont know why im here"
               , "you are not my daddy"
               ]

npcTextGenerator : Alignment -> Generator String
npcTextGenerator alignment =
  let picker = pickFromList ""
  in
  case alignment of
    User.Maga -> picker magaTweets
    User.Resist -> picker resistTweets
    User.Boring -> picker boringTweets

tweetGenerator : UserData -> Generator Tweet
tweetGenerator data =
  Random.map (makeTweet (User.NPC data)) (npcTextGenerator data.alignment)

-- User generators

nickGenerator =
  pickFromList "William Legate" []

handleGenerator =
  pickFromList "williamlegate" []

aviGenerator =
  pickFromList "" []

alignmentGenerator =
  pickFromList User.Maga [ User.Maga, User.Resist, User.Boring ]

userGenerator : Generator UserData
userGenerator =
  Random.map4 createUser nickGenerator handleGenerator aviGenerator alignmentGenerator
