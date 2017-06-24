module User exposing (..)

type Alignment = Maga | Resist | Boring

type alias UserData =
  { nickname : String
  , username : String
  , avatarUrl : String
  , userId : Int
  , alignment : Alignment }

type alias RandomData = List Float

trumpData =
  { nickname = "Donald J. Trump"
  , username = "realDonaldTrump"
  , avatarUrl = ""
  , userId = -1
  , alignment = Maga }

type User = Player | NPC UserData

legate =
  NPC { nickname = "William LeGate"
  , username = "williamlegate"
  , avatarUrl = ""
  , userId = 0
  , alignment = Resist }
