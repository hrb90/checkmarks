module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import User exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none }

type alias Tweet =
  { content : String
  , user : User }

type alias Model =
  { currentInput: String
  , timeline: List Tweet }

type Msg =
  NoOp
  | UpdateInput String
  | SendPlayerTweet
  | SendReply User

makeTweet : User -> String -> Tweet
makeTweet user str =
  { content = str
  , user = user }

generateTweet : User -> Tweet
generateTweet user =
  let makeFromString = makeTweet user
  in
  case user of
    Player -> makeFromString "Healthy young child goes to doctor, gets pumped with massive shot of many vaccines, doesn't feel good and changes - AUTISM. Many such cases!"
    NPC data -> makeFromString (generateText data.alignment)

init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [] } ! []

-- Update logic

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
    UpdateInput str -> { model | currentInput = str } ! []
    SendPlayerTweet -> let prependTweet tl =
                    (makeTweet Player model.currentInput) :: tl
                in
                    { model | timeline = prependTweet model.timeline }
                     |> update (UpdateInput "")
    SendReply user -> let prependTweet tl =
                    (generateTweet user) :: tl
                in
                    { model | timeline = prependTweet model.timeline } ! []


-- View logic

view : Model -> Html Msg
view model =
  div
    [ class "root" ]
    [ tweetInput model.currentInput
    , viewTweetList model
    , escapeHatch model ]

viewTweetList : Model -> Html Msg
viewTweetList model =
    ul
        [ class "tweets-list" ]
        (List.map viewTweetLi model.timeline)

makeHeader : UserData -> Html Msg
makeHeader data =
  p
    [ class "tweet-header" ]
    [ text data.nickname
    , text data.username ]

viewHeader : Tweet -> Html Msg
viewHeader tweet =
  case tweet.user of
    Player -> makeHeader trumpData
    NPC data -> makeHeader data

viewTweetLi : Tweet -> Html Msg
viewTweetLi tweet =
    li
        [ class "tweets-list-item" ]
        [ div
          [ class "tweet" ]
          [ viewHeader tweet
          , text tweet.content ]
        ]

tweetInput : String -> Html Msg
tweetInput str =
    div
        [ class "tweet-input" ]
        [ textarea
            [ class "tweet-box"
            , placeholder "Write a tweet!"
            , value str
            , onInput UpdateInput ]
            [],
           button
             [ onClick SendPlayerTweet
             , disabled (String.length str == 0) ]
             [ text "Tweet" ]
        ]

-- This is for development purposes, it's a place where I can put things
-- that won't exist in the final UI but are helpful in development

escapeHatch : Model -> Html Msg
escapeHatch model =
    div
        [ class "escape-hatch" ]
        [ button
          [ onClick (SendReply (getRandomUser ()))]
          [ text "Get reply" ]
        ]
