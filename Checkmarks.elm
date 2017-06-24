module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (generate)
import String
import Generators exposing (..)
import User exposing (..)
import Time exposing (Time, second)
import Tweet exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

type alias Model =
  { uid: Int
  , currentInput: String
  , timeline: List Tweet
  , users: List UserData
  , score: Int
  , health: Int
  , inRound: Bool }

type Msg =
  NoOp
  | Tick
  | StartRound
  | EndRound
  | CreateUser UserData
  | GenerateReply UserData
  | UpdateInput String
  | SendTweet Tweet
  | Like Tweet
  | Unlike Tweet
  | Block UserData


init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [], uid = 1, users = [], score = 0, health = 0, inRound = False } ! []

-- Update logic

-- Model

incrementId : Model -> Model
incrementId model =
  { model | uid = model.uid + 1 }

addToScore : Int -> Model -> Model
addToScore n model =
  { model | score = model.score + n }

addToHealth : Int -> Model -> Model
addToHealth n model =
  { model | health = Basics.min (model.health + n) 100 }

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

startRound : Model -> Model
startRound model =
  { model | inRound = True } |> addToHealth 200

endRound : Model -> Model
endRound model =
  { model | inRound = False }

updateScore : Msg -> Model -> Model
updateScore msg model =
  case msg of
    Like tweet -> case tweet.user of
                    Player -> model
                    NPC data -> case data.alignment of
                                  Maga -> model |> addToScore 1 |> addToHealth 1
                                  Resist -> model |> addToHealth -3
                                  Boring -> model |> addToHealth -1
    Unlike tweet -> case tweet.user of
                    Player -> model
                    NPC data -> case data.alignment of
                                  Maga -> model |> addToHealth -3
                                  Resist -> model |> addToScore 1
                                  Boring -> model |> addToHealth -1
    Block data -> case data.alignment of
                    Maga -> model |> addToHealth -5
                    Resist -> model |> addToScore 5 |> addToHealth 1
                    Boring -> model |> addToHealth -3
    Tick -> model
            |> addToHealth (-1 * (List.length (List.filter resistanceTweet model.timeline) ))
    _ -> model


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
    Tick -> let resisters =
              List.filter resists model.users
            in
            case resisters of
              [] -> model |> (update EndRound)
              hd::tl -> model |> (updateScore msg) |> pickUser hd model.users
    StartRound -> let tweet =
                    makeTweet Player model.currentInput
                  in
                    model
                    |> (setCurrentInput "") |> (addTweet tweet)
                    |> startRound |> genUsers
    EndRound -> model |> endRound |> noEffects
    GenerateReply data -> model |> (genReply data)
    UpdateInput str -> model |> (setCurrentInput str) |> noEffects
    SendTweet tweet -> model |> (addTweet tweet) |> noEffects
    Like tweet -> let like t =
                    if t.id == tweet.id then
                      { t | liked = True }
                    else
                      t
                  in
                    model |> (mapTimeline like) |> (updateScore msg) |> noEffects
    Unlike tweet -> let unlike t =
                      if t.id == tweet.id then
                        { t | liked = False }
                      else
                        t
                    in
                      model |> (mapTimeline unlike) |> (updateScore msg) |> noEffects
    Block data -> let display t =
                      case t.user of
                        User.Player -> True
                        User.NPC otherData -> data.userId /= otherData.userId
                    in
                      model |> (filterTimeline display)
                      |> (filterUsers (\u -> u.userId /= data.userId))
                      |> (updateScore msg)
                      |> noEffects


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> if model.inRound then Tick else NoOp)

-- View logic

view : Model -> Html Msg
view model =
  div
    [ class "root" ]
    [ tweetInput model.currentInput
    , escapeHatch model
    , viewTweetList model
    ]

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
             [ onClick StartRound
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
        [ p
            [ class "score" ]
            [ text (toString model.score) ]
        , p
            [ class "health" ]
            [ text (toString model.health) ]
        ]
