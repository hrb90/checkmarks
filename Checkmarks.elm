module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (generate)
import String
import User exposing (..)
import Tweet exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none }

type alias Model =
  { uid: Int
  , currentInput: String
  , timeline: List Tweet }

type Msg =
  NoOp
  | GenerateReply User
  | UpdateInput String
  | SendPlayerTweet
  | SendTweet Tweet
  | Like Tweet
  | Unlike Tweet


init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [], uid = 1 } ! []

-- Update logic

-- Model

incrementId : Model -> Model
incrementId model =
  { model | uid = model.uid + 1 }

setCurrentInput : String -> Model -> Model
setCurrentInput newInput model =
  { model | currentInput = newInput }

addTweet: Tweet -> Model -> Model
addTweet tweet model =
  { model | timeline = tweet::model.timeline }
  |> incrementId

mapTimeline: (Tweet -> Tweet) -> Model -> Model
mapTimeline f model =
  { model | timeline = List.map f model.timeline }

-- Cmd Msg

noEffects : Model -> ( Model, Cmd Msg )
noEffects model =
  model ! []

genReply : User -> Model -> ( Model, Cmd Msg )
genReply user model =
  model ! [generate SendTweet (tweetGenerator user model.uid)]

-- The update function itself

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model |> noEffects
    GenerateReply user -> model |> (genReply user)
    UpdateInput str -> model |> (setCurrentInput str) |> noEffects
    SendPlayerTweet -> let sendMsg =
                    SendTweet (makeTweet model.uid Player model.currentInput)
                in
                    model |> (setCurrentInput "") |> (update sendMsg)
    SendTweet tweet -> model |> (addTweet tweet) |> noEffects
    Like tweet -> let like t =
                    if t.id == tweet.id then
                      { t | liked = True }
                    else
                      t
                  in
                    model |> (mapTimeline like) |> noEffects
    Unlike tweet -> let unlike t =
                      if t.id == tweet.id then
                        { t | liked = False }
                      else
                        t
                    in
                      model |> (mapTimeline unlike) |> noEffects


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

viewFooter : Tweet -> Html Msg
viewFooter tweet =
  let buttoninfo = if tweet.liked then
                      { text = "Unlike", class = "star liked", msg = Unlike tweet }
                   else
                      { text = "Like", class = "star unliked", msg = Like tweet }
  in
  case tweet.user of
    Player -> div [ class "tweet-footer hidden" ] []
    NPC _ -> div
              [ class "tweet-footer" ]
              [ button
                [ class buttoninfo.class
                , onClick buttoninfo.msg ]
                [ text buttoninfo.text ]
              ]

viewTweetLi : Tweet -> Html Msg
viewTweetLi tweet =
    li
        [ class "tweets-list-item" ]
        [ div
          [ class "tweet" ]
          [ viewHeader tweet
          , text tweet.content
          , viewFooter tweet ]
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
             , disabled (String.isEmpty str) ]
             [ text "Tweet" ]
        ]

-- This is for development purposes, it's a place where I can put things
-- that won't exist in the final UI but are helpful in development

escapeHatch : Model -> Html Msg
escapeHatch model =
    div
        [ class "escape-hatch" ]
        [ button
          [ onClick (GenerateReply legate)]
          [ text "Get reply" ]
        ]
