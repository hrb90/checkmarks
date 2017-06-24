module User exposing (..)

type Alignment = Maga | Resist | Boring

type alias UserData =
  { nickname : String
  , username : String
  , avatarUrl : String
  , userId : Maybe Int
  , alignment : Alignment }

type alias RandomData = List Float

trumpData =
  { nickname = "Donald J. Trump"
  , username = "realDonaldTrump"
  , avatarUrl = ""
  , userId = Nothing
  , alignment = Maga }

type User = Player | NPC UserData

legate =
  NPC { nickname = "William LeGate"
  , username = "williamlegate"
  , avatarUrl = ""
  , userId = Nothing
  , alignment = Resist }
