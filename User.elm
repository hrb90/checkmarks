module User exposing (..)

type Alignment = Maga | Resist | Boring

type alias UserData =
  { nickname : String
  , username : String
  , avatarUrl : String
  , userId : Int
  , alignment : Alignment }

type User = Player | NPC UserData

generateText : Alignment -> String
generateText alignment =
  case alignment of
    Maga -> "You tell em Mr President sir!!"
    Resist -> "LOL COVFEFE YOU IDIOT DUMMY MORON, GO BACK TO RUSSIA"
    Boring -> "i dont know why im here"
