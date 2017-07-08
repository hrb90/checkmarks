module StylishView exposing (viewGameOver, viewStartRound, health)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Style exposing (..)
import Style.Border exposing (rounded, none, all, bottom)
import Style.Color as Color


-- Stylesheet


type Styles
    = None
    | Button
    | Box
    | TextInput
    | TweetInput
    | HealthBar
    | HealthRed
    | Page



-- Colors


twitterButtonBlue =
    rgb 74 179 244


twitterTextBlue =
    rgb 29 161 242


twitterLightBlue =
    rgb 232 245 252


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
        , style TweetInput
            [ Color.background twitterLightBlue ]
        , style TextInput
            [ all 1
            , Color.border twitterTextBlue
            , rounded 5
            ]
        , style HealthBar
            [ all 1
            , Color.border Color.black
            , rounded 5
            ]
        , style HealthRed
            [ Color.background Color.red
            , rounded 5
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
                        [ onClick Msg.Reset
                        , padding 10
                        ]
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


viewStartRound model =
    layout stylesheet <|
        (tweetInput model.currentInput)


tweetInput str =
    column
        TweetInput
        [ verticalCenter
        , center
        , spacing 10
        , padding 10
        ]
        [ textArea
            TextInput
            [ placeholder "What's happening?"
            , onInput Msg.UpdateInput
            ]
            str
        , button <|
            el
                Button
                [ alignRight
                , onClick Msg.StartRound
                , disabled <| String.isEmpty <| str
                , padding 10
                ]
                (text "Tweet")
        ]


health model =
    layout stylesheet <|
        (healthBar model.health)


healthBar currHealth =
    let
        redBar =
            el
                HealthRed
                [ width <| percent <| toFloat <| (clamp 0 100 currHealth)
                , height (px 8)
                ]
                empty
    in
        el
            HealthBar
            [ width (px 100)
            , height (px 10)
            ]
            redBar
