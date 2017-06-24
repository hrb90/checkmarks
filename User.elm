module User exposing (..)

import Random exposing (..)

type Alignment = Maga | Resist | Boring

type alias UserData =
  { nickname : String
  , username : String
  , avatarUrl : String
  , userId : Int
  , alignment : Alignment }

createUser_: Int -> String -> String -> String -> Alignment -> UserData
createUser_ id nick handle avi alignment =
  { nickname = nick
  , username = handle
  , avatarUrl = avi
  , userId = id
  , alignment = alignment
  }

createUser = createUser_ 0

trumpData =
  createUser_ -1 "Donald J. Trump" "realDonaldTrump" "" Maga

type User = Player | NPC UserData

legate =
  { nickname = "William LeGate"
  , username = "williamlegate"
  , avatarUrl = ""
  , userId = 0
  , alignment = Resist }

resists : UserData -> Bool
resists ud =
  case ud.alignment of
    Resist -> True
    _ -> False
