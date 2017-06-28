module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (ul)
import Css.Namespace exposing (namespace)


type CssClass
    = TweetList
    | TweetListItem
    | TweetInput
    | LoadMore
    | StartRoundButton
    | RestartGameButton
    | LikeButton
    | UnlikeButton
    | BlockButton


css =
    (stylesheet << namespace "checkmarks")
        [ ul
            [ listStyle none
            , marginLeft (px 0)
            ]
        , class TweetList
            [ margin auto
            ]
        , class TweetListItem []
        , class TweetInput []
        , class LoadMore []
        , class StartRoundButton []
        , class RestartGameButton []
        , class LikeButton []
        , class UnlikeButton []
        , class BlockButton []
        ]
