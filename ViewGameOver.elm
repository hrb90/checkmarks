module ViewGameOver exposing (viewGameOver)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Style exposing (..)
import Style.Border exposing (rounded, none, all)
import Style.Color as Color


-- Stylesheet


type Styles
    = None
    | Button
    | Box
    | Page



-- Colors


twitterButtonBlue =
    rgb 74 179 244


twitterTextBlue =
    rgb 29 161 242


twitterLightBlue =
    rgb 232 232 255


twitterLightGrey =
    rgb 245 248 250


twitterGrey =
    rgb 230 236 240



-- Stylesheet


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None []
        , style Page [ Color.background twitterGrey ]
        , style Button
            [ Color.background twitterButtonBlue
            , Color.text Color.white
            , rounded 100
            , none
            ]
        , style Box
            [ all 1
            , Color.background twitterLightGrey
            , Color.border Color.black
            , rounded 20
            ]
        ]



-- Views


viewGameOver model =
    layout stylesheet <|
        el
            Page
            [ height (px 500) ]
            (gameOverBox model.score)


gameOverBox : Int -> Element Styles a Msg
gameOverBox score =
    let
        col =
            column
                None
                [ verticalCenter
                , spacing 20
                ]
                [ text <| "Game Over! Score: " ++ (toString score)
                , button <|
                    el
                        Button
                        [ onClick Msg.Reset ]
                        (text "New Game")
                ]
    in
        el
            Box
            [ verticalCenter
            , center
            , padding 20
            ]
            col
