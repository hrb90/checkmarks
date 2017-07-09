module Model exposing (..)

import Tweet exposing (Tweet)
import User exposing (UserData)


type IntroductionPage
    = IntroSplash


type Page
    = GameOver
    | BeforeRound
    | PlayingGame
    | Introduction IntroductionPage


type alias Model =
    { uid : Int
    , currentInput : String
    , unseenTimeline : List Tweet
    , timeline : List Tweet
    , users : List UserData
    , score : Int
    , health : Int
    , roundNumber : Int
    , currentPage : Page
    }


initialModel : Model
initialModel =
    { currentInput = ""
    , unseenTimeline = []
    , timeline = []
    , uid = 1
    , users = []
    , score = 0
    , health = 0
    , roundNumber = 0
    , currentPage = Introduction IntroSplash
    }


newGameModel : Model
newGameModel =
    { initialModel | currentPage = BeforeRound }
