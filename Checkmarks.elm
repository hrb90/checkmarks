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
  | AddTweet

makeTweet : User -> String -> Tweet
makeTweet user str =
  { content = str
  , user = user }


init : (Model, Cmd Msg)
init =
  { currentInput = "", timeline = [] } ! []

-- Update logic

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
    UpdateInput str -> { model | currentInput = str } ! []
    AddTweet -> let prependTweet tl =
                    (makeTweet Player model.currentInput) :: tl
                in
                    { model | timeline = prependTweet model.timeline }
                     |> update (UpdateInput "")

-- View logic

view : Model -> Html Msg
view model =
    ul
        [ class "tweets-list" ]
        ((tweetInput model.currentInput) :: (List.map viewTweet model.timeline))

trumpHeader : Html Msg
trumpHeader = p
                [ class "tweet-header" ]
                [ text "Donald J. Trump"
                , text "@realDonaldTrump" ]

viewHeader : Tweet -> Html Msg
viewHeader tweet =
  case tweet.user of
    Player -> trumpHeader
    NPC data -> p
                  [ class "tweet-header" ]
                  [ text data.nickname
                  , text data.username ]

viewTweet : Tweet -> Html Msg
viewTweet tweet =
    li
        [ class "tweets-list-item" ]
        [ div
          [ class "tweet "]
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
             [ onClick AddTweet ]
             [ text "Tweet" ]
        ]
