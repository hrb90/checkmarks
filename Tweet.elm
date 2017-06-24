module Tweet exposing (..)

import User exposing (..)

type alias Tweet =
  { id : Int
  , content : String
  , user : User
  , liked: Bool }

-- Constructors

makeTweet : User -> String -> Tweet
makeTweet user str =
  { id = 0
  , content = str
  , user = user
  , liked = False }
