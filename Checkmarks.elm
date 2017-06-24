module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (generate)
import String
import Generators exposing (..)
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
  , timeline: List Tweet
  , users: List UserData }

type Msg =
  NoOp
  | Tick
  | CreateUser UserData
  | GenerateReply UserData
  | UpdateInput String
  | SendPlayerTweet
  | SendTweet Tweet
  | Like Tweet
  | Unlike Tweet
  | Block UserData


init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [], uid = 1, users = [] } ! []

-- Update logic

-- Model

incrementId : Model -> Model
incrementId model =
  { model | uid = model.uid + 1 }

setCurrentInput : String -> Model -> Model
setCurrentInput newInput model =
  { model | currentInput = newInput }

addUser: UserData -> Model -> Model
addUser data model =
  let data_ =
    { data | userId = model.uid }
  in
  { model | users = data_::model.users }
  |> incrementId

addTweet: Tweet -> Model -> Model
addTweet tweet model =
  let tweet_ =
    { tweet | id = model.uid }
  in
  { model | timeline = tweet_::model.timeline }
  |> incrementId

mapTimeline: (Tweet -> Tweet) -> Model -> Model
mapTimeline f model =
  { model | timeline = List.map f model.timeline }

filterTimeline: (Tweet -> Bool) -> Model -> Model
filterTimeline f model =
  { model | timeline = List.filter f model.timeline }

filterUsers : (UserData -> Bool) -> Model -> Model
filterUsers f model =
  { model | users = List.filter f model.users }

-- Cmd Msg

noEffects : Model -> ( Model, Cmd Msg )
noEffects model =
  model ! []

genReply : UserData -> Model -> ( Model, Cmd Msg )
genReply data model =
  model ! [generate SendTweet (tweetGenerator data)]

genUsers: Model -> ( Model, Cmd Msg )
genUsers model =
  model ! List.repeat 20 (generate CreateUser userGenerator)

pickUser : UserData -> List UserData -> Model -> ( Model, Cmd Msg )
pickUser hd tl model =
  model ! [generate GenerateReply (pickFromList hd (hd::tl))]

-- The update function itself

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model |> noEffects
    CreateUser data -> model |> addUser data |> noEffects
    Tick -> case model.users of
              [] -> model |> genUsers
              hd::tl -> model |> pickUser hd tl
    GenerateReply data -> model |> (genReply data)
    UpdateInput str -> model |> (setCurrentInput str) |> noEffects
    SendPlayerTweet -> let sendMsg =
                         SendTweet (makeTweet Player model.currentInput)
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
    Block data -> let display t =
                      case t.user of
                        User.Player -> True
                        User.NPC otherData -> data.userId /= otherData.userId
                    in
                      model |> (filterTimeline display)
                      |> (filterUsers (\u -> u.userId /= data.userId))
                      |> noEffects


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
  case tweet.user of
    Player -> div [ class "tweet-footer hidden" ] []
    NPC data -> div
              [ class "tweet-footer" ]
              [ likeButton tweet
              , button
                [ class "block"
                , onClick (Block data) ]
                [ text "Block" ]
              ]

likeButton : Tweet -> Html Msg
likeButton tweet =
  let buttoninfo =
    if tweet.liked then
      { text = "Unlike", class = "star liked", msg = Unlike tweet }
    else
      { text = "Like", class = "star unliked", msg = Like tweet }
  in
  button
    [ class buttoninfo.class
    , onClick buttoninfo.msg ]
    [ text buttoninfo.text ]

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
            , onInput UpdateInput
            ]
            [],
           button
             [ onClick SendPlayerTweet
             , disabled (String.isEmpty str)
             ]
             [ text "Tweet" ]
        ]

-- This is for development purposes, it's a place where I can put things
-- that won't exist in the final UI but are helpful in development

escapeHatch : Model -> Html Msg
escapeHatch model =
    div
        [ class "escape-hatch" ]
        [ button
          [ onClick Tick ]
          [ text "Tick" ]
        ]
