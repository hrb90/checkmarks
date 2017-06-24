module Tweet exposing (..)

import Array exposing (fromList, get)
import Random exposing (..)
import User exposing (User, Alignment)

type alias Tweet =
  { id : Int
  , content : String
  , user : User
  , liked: Bool }

-- Constructors

makeTweet : Int -> User -> String -> Tweet
makeTweet iden user str =
  { id = iden
  , content = str
  , user = user
  , liked = False }

-- Generators

indexWithDefault : a -> List a -> Int -> a
indexWithDefault default l idx =
  Maybe.withDefault default (get idx (fromList l))

pickFromList : a -> List a -> Generator a
pickFromList default l =
  Random.map (indexWithDefault default l) (int 0 (List.length l - 1))

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

playerTextGenerator : Generator String
playerTextGenerator =
  Random.map (\b -> if b then
                "Healthy young child goes to doctor, gets pumped with massive shot of many vaccines, doesn't feel good and changes - AUTISM. Many such cases!"
             else
               "Despite the constant negative press covfefe,") bool

tweetGenerator : User -> Int -> Generator Tweet
tweetGenerator user id =
  let makeFromStringGen = Random.map (makeTweet id user)
  in
  case user of
    User.Player -> makeFromStringGen playerTextGenerator
    User.NPC data -> makeFromStringGen (npcTextGenerator data.alignment)
