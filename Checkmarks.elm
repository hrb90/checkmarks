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
  , unseenTimeline: List Tweet
  , timeline: List Tweet
  , users: List UserData
  , score: Int
  , health: Int
  , roundNumber: Int
  , inRound: Bool
  , gameOver: Bool }

type Msg =
  NoOp
  | Reset
  | Tick
  | ShowMoreTweets
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
  { currentInput = ""
  , unseenTimeline = []
  , timeline = []
  , uid = 1
  , users = []
  , score = 0
  , health = 0
  , roundNumber = 0
  , inRound = False
  , gameOver = False } ! []

-- Update logic

-- Utils

tweetsPerTick : Model -> Int
tweetsPerTick model =
  1

populationSize : Model -> Int
populationSize model =
  20

-- Model

incrementId : Model -> Model
incrementId model =
  { model | uid = model.uid + 1 }

incrementRound : Model -> Model
incrementRound model =
  { model | roundNumber = model.roundNumber + 1 }

addToScore : Int -> Model -> Model
addToScore n model =
  { model | score = model.score + n }

addToHealth : Int -> Model -> Model
addToHealth n model =
  { model | health = Basics.min (model.health + n) 100 }
  |> checkHealth

checkHealth : Model -> Model
checkHealth model =
  if model.health <= 0 then
    { model | gameOver = True } |> endRound
  else
    model

setCurrentInput : String -> Model -> Model
setCurrentInput newInput model =
  { model | currentInput = newInput }

clearUsers : Model -> Model
clearUsers model =
  { model | users = [] }

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
    { model | unseenTimeline = tweet_::model.unseenTimeline }
    |> incrementId

mapTimeline: (Tweet -> Tweet) -> Model -> Model
mapTimeline f model =
  { model | timeline = List.map f model.timeline }

filterTimeline: (Tweet -> Bool) -> Model -> Model
filterTimeline f model =
  { model | timeline = List.filter f model.timeline }

filterUnseenTimeline: (Tweet -> Bool) -> Model -> Model
filterUnseenTimeline f model =
  { model | unseenTimeline = List.filter f model.unseenTimeline }

filterUsers : (UserData -> Bool) -> Model -> Model
filterUsers f model =
  { model | users = List.filter f model.users }

startRound : Model -> Model
startRound model =
  { model | inRound = True }
  |> addToHealth 200
  |> incrementRound
  |> clearUsers

endRound : Model -> Model
endRound model =
  { model | inRound = False }

showMoreTweets : Model -> Model
showMoreTweets model =
  let clearUnseen model_ =
    { model_ | unseenTimeline = [] }
  in
  { model | timeline = (List.sortBy getRep model.unseenTimeline) ++ model.timeline }
  |> clearUnseen


updateScore : Msg -> Model -> Model
updateScore msg model =
  let countResistanceTweets model =
    (model.unseenTimeline ++ model.timeline)
    |> List.filter resistanceTweet
    |> List.length
  in
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
            |> addToHealth (-1 * countResistanceTweets model)
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
  let n =
    populationSize model
  in
  model ! List.repeat n (generate CreateUser userGenerator)

pickUsers : UserData -> List UserData -> Model -> ( Model, Cmd Msg )
pickUsers hd tl model =
  let n =
    tweetsPerTick model
  in
    model ! List.repeat n (generate GenerateReply (pickFromList hd (hd::tl)))

-- The update function itself

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let updateScore_ =
    updateScore msg
  in
  case msg of
    Reset -> init
    NoOp -> model |> noEffects
    CreateUser data -> model |> addUser data |> noEffects
    Tick -> let resisters =
              List.filter resists model.users
            in
            case resisters of
              [] -> model |> (update EndRound)
              hd::tl -> model |> updateScore_ |> pickUsers hd model.users
    StartRound -> let tweet =
                    makeTweet Player model.currentInput
                  in
                    model
                    |> (setCurrentInput "") |> (addTweet tweet)
                    |> startRound |> genUsers
    EndRound -> model |> showMoreTweets |> endRound |> noEffects
    ShowMoreTweets -> model |> showMoreTweets |> noEffects
    GenerateReply data -> model |> (genReply data)
    UpdateInput str -> model |> (setCurrentInput str) |> noEffects
    SendTweet tweet -> model |> (addTweet tweet) |> noEffects
    Like tweet -> let like t =
                    if t.id == tweet.id then
                      { t | liked = True }
                    else
                      t
                  in
                    model |> (mapTimeline like) |> updateScore_ |> noEffects
    Unlike tweet -> let unlike t =
                      if t.id == tweet.id then
                        { t | liked = False }
                      else
                        t
                    in
                      model |> (mapTimeline unlike) |> updateScore_ |> noEffects
    Block data -> let display t =
                      case t.user of
                        User.Player -> True
                        User.NPC otherData -> data.userId /= otherData.userId
                    in
                      model |> (filterTimeline display)
                      |> (filterUnseenTimeline display)
                      |> (filterUsers (\u -> u.userId /= data.userId))
                      |> updateScore_
                      |> noEffects


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> if model.inRound then Tick else NoOp)

-- View logic

view : Model -> Html Msg
view model =
  case (model.gameOver, model.inRound) of
    (True, _) -> viewGameOver model
    (_, True) -> viewInRound model
    (_, False) -> viewStartRound model

viewGameOver : Model -> Html Msg
viewGameOver model =
  div
    [ class "game-over" ]
    [
      p
        []
        [ text "Game Over!"
        , text ("Score: " ++ (toString model.score))
        ]
      , button
          [ class "restart"
          , onClick Reset
          ]
          [ text "New Game" ]
    ]

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
    , onClick ShowMoreTweets ]
    [ model.unseenTimeline
      |> List.length
      |> toString
      |> text ]

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

score : Model -> Html Msg
score model =
  p
    [ class "score" ]
    [ text (toString model.score) ]


health : Model -> Html Msg
health model =
  p
    [ class "health" ]
    [ text (toString model.health) ]

-- This is for development purposes, it's a place where I can put things
-- that won't exist in the final UI but are helpful in development

escapeHatch : Model -> Html Msg
escapeHatch model =
    div
        [ class "escape-hatch" ]
        [ score model
        , health model
        ]
