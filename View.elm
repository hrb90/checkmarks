module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import String
import Tweet exposing (..)
import User exposing (..)
import StylishView exposing (..)


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


viewInRound : Model -> Html Msg
viewInRound model =
    div
        [ class "root" ]
        [ navBar model
        , escapeHatch model
        , loadMoreButton model
        , viewTweetList model
        ]


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
        [ endGameButton
        ]
