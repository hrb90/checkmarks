module View exposing (view)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html exposing (Html)
import Model exposing (..)
import Msg exposing (Msg)
import Style exposing (..)
import Style.Border exposing (rounded, none, all, bottom)
import Style.Color as Color
import Tweet exposing (..)
import User exposing (..)


-- Stylesheet


type Styles
    = None
    | PressedLikeButton
    | Button
    | NotifsButton
    | Box
    | TextInput
    | TweetInput
    | HealthBar
    | HealthRed
    | Badge
    | Page
    | Tweet
    | IntroSplashStyle



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
            , rounded 20
            ]
        , style HealthRed
            [ Color.background Color.red
            , rounded 20
            ]
        , style Badge
            [ Color.background Color.yellow
            , rounded 20
            ]
        , style NotifsButton
            [ Color.background twitterLightGrey
            , Color.text twitterTextBlue
            ]
        , style PressedLikeButton
            [ Color.background Color.red ]
        , style Tweet []
        , style IntroSplashStyle []
        ]



-- Views
-- The router


view : Model -> Html Msg
view model =
    layout stylesheet <|
        case model.currentPage of
            GameOver ->
                viewGameOver model

            PlayingGame ->
                viewInRound model

            BeforeRound ->
                viewStartRound model

            Introduction page ->
                viewIntro page



-- Introduction


viewIntro : IntroductionPage -> Element Styles a Msg
viewIntro page =
    case page of
        IntroSplash ->
            viewIntroSplash


newGameButton : Element Styles a Msg
newGameButton =
    button <|
        el
            Button
            [ onClick Msg.Reset
            , padding 10
            ]
            (text "New Game")


viewIntroSplash : Element Styles a Msg
viewIntroSplash =
    el IntroSplashStyle [] <|
        el Box
            [ verticalCenter
            , center
            , 80 |> percent |> width
            , 40 |> percent |> height
            ]
        <|
            row None
                [ verticalCenter, spacing 50 ]
            <|
                (List.map
                    (paragraph None [ alignLeft ] << List.singleton << text)
                    [ "Rampant Twitter cyberbullying these days..."
                    , "Whitehouse is not the exception..."
                    ]
                )
                    ++ [ newGameButton ]



-- Game over screen


viewGameOver model =
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
                , newGameButton
                ]
    in
        el
            Box
            [ verticalCenter
            , center
            , padding 20
            ]
            col



-- Start round screen


viewStartRound model =
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



-- Game views


viewInRound model =
    column
        None
        []
        [ navBar model
        , viewNotifs model
        ]


viewNotifs model =
    column
        None
        []
        [ loadMoreButton model, viewTweetList model ]


viewTweetList model =
    column
        None
        [ center ]
        (List.map viewTweet model.timeline)


viewTweet tweet =
    column
        Tweet
        [ spacing 10
        , width (px 400)
        ]
        [ viewHeader tweet
        , text tweet.content
        , viewFooter tweet
        ]


viewFooter tweet =
    case tweet.user of
        Player ->
            empty

        NPC data ->
            row
                None
                [ spacing 20 ]
                [ likeButton tweet
                , blockButton data
                ]


likeButton tweet =
    let
        ( style_, msg_ ) =
            if tweet.liked then
                ( PressedLikeButton, Msg.Unlike tweet )
            else
                ( None, Msg.Like tweet )
    in
        el
            style_
            [ onClick msg_ ]
            (text "Like")


blockButton data =
    el None [ onClick (Msg.Block data) ] (text "Block")


makeHeader data =
    row
        None
        [ spacing 20 ]
        [ bold data.nickname
        , text ("@" ++ data.username)
        ]


viewHeader tweet =
    case tweet.user of
        Player ->
            makeHeader trumpData

        NPC data ->
            makeHeader data


loadMoreButton model =
    let
        loadMoreText numTweets =
            el
                None
                [ center ]
                (text <| (toString numTweets ++ " new replies"))
    in
        if List.isEmpty model.unseenTimeline then
            empty
        else
            el
                NotifsButton
                [ onClick Msg.ShowMoreTweets
                , paddingXY 0 10
                ]
                (loadMoreText <| List.length <| model.unseenTimeline)


navBar model =
    let
        badges =
            row
                None
                [ spacing 20 ]
                [ roundBadge model, score model ]
    in
        row
            None
            [ 50 |> px |> height
            , 100 |> percent |> width
            , paddingXY 10 0
            , verticalCenter
            , justify
            ]
            [ badges
            , healthBar model.health
            ]


healthBar currHealth =
    let
        redBar =
            el
                HealthRed
                [ toFloat (clamp 0 100 currHealth) |> percent |> width
                , 18 |> px |> height
                ]
                empty
    in
        el
            HealthBar
            [ 100 |> px |> width
            , 20 |> px |> height
            ]
            redBar


roundBadge model =
    el
        Badge
        [ maxWidth (px 100), padding 10 ]
        (text <| "Round " ++ (toString model.roundNumber))


score model =
    el
        Badge
        [ maxWidth (px 100), padding 10 ]
        (text <| "Score: " ++ (toString model.score))
