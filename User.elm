module User exposing (..)

import Random exposing (..)


type Alignment
    = Maga
    | Resist
    | Boring


type alias UserData =
    { nickname : String
    , username : String
    , avatarUrl : String
    , userId : Int
    , alignment : Alignment
    , reputation : Float
    }


createUser_ : Int -> String -> String -> String -> Alignment -> Float -> UserData
createUser_ id nick handle avi alignment reputation =
    { nickname = nick
    , username = handle
    , avatarUrl = avi
    , userId = id
    , alignment = alignment
    , reputation = reputation
    }


createUser =
    createUser_ 0


trumpData =
    createUser_ -1 "Donald J. Trump" "realDonaldTrump" "" Maga 0


type User
    = Player
    | NPC UserData


legate =
    { nickname = "William LeGate"
    , username = "williamlegate"
    , avatarUrl = ""
    , userId = 0
    , alignment = Resist
    }


resists : UserData -> Bool
resists ud =
    case ud.alignment of
        Resist ->
            True

        _ ->
            False
