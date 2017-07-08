module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import String
import Tweet exposing (..)
import User exposing (..)
import ViewGameOver exposing (..)


-- Helpers


textCoercive =
    text << toString



-- View logic


view : Model -> Html Msg
view model =
    case model.currentPage of
        GameOver ->
            viewGameOver model

        PlayingGame ->
            viewInRound model

        BeforeRound ->
            viewStartRound model


viewStartRound : Model -> Html Msg
viewStartRound model =
    tweetInput model.currentInput


viewInRound : Model -> Html Msg
viewInRound model =
    div
        [ class "root" ]
        [ escapeHatch model
        , loadMoreButton model
        , viewTweetList model
        ]


loadMoreButton : Model -> Html Msg
loadMoreButton model =
    button
        [ class "load-more"
        , onClick Msg.ShowMoreTweets
        ]
        [ textCoercive <| List.length <| model.unseenTimeline ]


viewTweetList : Model -> Html Msg
viewTweetList model =
    ul
        [ class "tweet-list" ]
        (List.map viewTweetLi model.timeline)


makeHeader : UserData -> Html Msg
makeHeader data =
    p
        [ class "tweet-header" ]
        [ text data.nickname
        , text data.username
        ]


viewHeader : Tweet -> Html Msg
viewHeader tweet =
    case tweet.user of
        Player ->
            makeHeader trumpData

        NPC data ->
            makeHeader data


viewFooter : Tweet -> Html Msg
viewFooter tweet =
    case tweet.user of
        Player ->
            div [ class "tweet-footer hidden" ] []

        NPC data ->
            div
                [ class "tweet-footer" ]
                [ likeButton tweet
                , button
                    [ class "block"
                    , onClick (Msg.Block data)
                    ]
                    [ text "Block" ]
                ]


likeButton : Tweet -> Html Msg
likeButton tweet =
    let
        ( class_, msg_, textCoercive_ ) =
            if tweet.liked then
                ( "star liked", Msg.Unlike tweet, "Unlike" )
            else
                ( "star unliked", Msg.Like tweet, "Like" )
    in
        button
            [ class class_
            , onClick msg_
            ]
            [ text textCoercive_ ]


viewTweetLi : Tweet -> Html Msg
viewTweetLi tweet =
    li
        [ class "tweet-list-item" ]
        [ div
            [ class "tweet" ]
            [ viewHeader tweet
            , text tweet.content
            , viewFooter tweet
            ]
        ]


tweetInput : String -> Html Msg
tweetInput str =
    div
        [ class "tweet-input" ]
        [ textarea
            [ class "tweet-box"
            , placeholder "Write a tweet!"
            , value str
            , onInput Msg.UpdateInput
            ]
            []
        , button
            [ onClick Msg.StartRound
            , disabled <| String.isEmpty <| str
            ]
            [ text "Tweet" ]
        ]


score : Model -> Html Msg
score model =
    p
        [ class "score" ]
        [ textCoercive model.score ]


health : Model -> Html Msg
health model =
    p
        [ class "health" ]
        [ textCoercive model.health ]


roundBadge : Model -> Html Msg
roundBadge model =
    p
        [ class "round-number" ]
        [ textCoercive model.roundNumber ]



-- This is for development purposes, it's a place where I can put things
-- that won't exist in the final UI but are helpful in development


endGameButton : Html Msg
endGameButton =
    button
        [ onClick Msg.EndGame ]
        [ text "End Game" ]


escapeHatch : Model -> Html Msg
escapeHatch model =
    div
        [ class "escape-hatch" ]
        [ score model
        , health model
        , roundBadge model
        , endGameButton
        ]
