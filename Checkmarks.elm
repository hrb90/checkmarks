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
  { id : Int
  , content : String
  , user : User
  , liked: Bool }

type alias Model =
  { uid: Int
  , currentInput: String
  , timeline: List Tweet }

type Msg =
  NoOp
  | UpdateInput String
  | SendPlayerTweet
  | SendReply User
  | Like Tweet
  | Unlike Tweet

makeTweet : Int -> User -> String -> Tweet
makeTweet iden user str =
  { id = iden
  , content = str
  , user = user
  , liked = False }

generateTweet : Int -> User -> Tweet
generateTweet iden user =
  let makeFromString = makeTweet iden user
  in
  case user of
    Player -> makeFromString "Healthy young child goes to doctor, gets pumped with massive shot of many vaccines, doesn't feel good and changes - AUTISM. Many such cases!"
    NPC data -> makeFromString (generateText data.alignment)

init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [], uid = 1 } ! []

-- Update logic

increment : Model -> Model
increment model =
  { model | uid = model.uid + 1 }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
    UpdateInput str -> { model | currentInput = str } ! []
    SendPlayerTweet -> let prependTweet tl =
                    (makeTweet model.uid Player model.currentInput) :: tl
                in
                    { model | timeline = prependTweet model.timeline }
                     |> update (UpdateInput "")
    SendReply user -> let prependTweet tl =
                    (generateTweet model.uid user) :: tl
                in
                    ({ model | timeline = prependTweet model.timeline }
                    |> increment) ! []
    Like tweet -> let like t =
                    if t.id == tweet.id then
                      { t | liked = True }
                    else
                      t
                  in
                  { model | timeline = List.map like model.timeline } ! []
    Unlike tweet -> let unlike t =
                      if t.id == tweet.id then
                        { t | liked = False }
                      else
                        t
                    in
                    { model | timeline = List.map unlike model.timeline } ! []


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
  let buttoninfo = if tweet.liked then { text = "Unlike", msg = Unlike tweet } else { text = "Like", msg = Like tweet }
  in
  case tweet.user of
    Player -> div [ class "tweet-footer hidden" ] []
    NPC _ -> div
              [ class "tweet-footer" ]
              [ button
                [ class "like-button"
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
          [ onClick (SendReply (getRandomUser ()))]
          [ text "Get reply" ]
        ]
