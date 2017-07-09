module Update exposing (update, init)

import Generators exposing (..)
import List.Nonempty as NE exposing (Nonempty, sample)
import Model exposing (..)
import Msg exposing (..)
import Random exposing (generate)
import Tweet exposing (..)
import User exposing (..)


-- Init


init : ( Model, Cmd Msg )
init =
    initialModel ! []



-- Utils


avgTweetsPerTick : Model -> Float
avgTweetsPerTick model =
    (toFloat model.roundNumber + 2.0) / 5


populationSize : Model -> Int
populationSize model =
    (model.roundNumber - 1)
        % 5
        |> (+) 1
        |> (*) 10



-- Model


incrementId : Model -> Model
incrementId model =
    { model | uid = model.uid + 1 }


setCurrentInput : String -> Model -> Model
setCurrentInput newInput model =
    { model | currentInput = newInput }


clearUsers : Model -> Model
clearUsers model =
    { model | users = [] }


addUser : UserData -> Model -> Model
addUser data model =
    let
        data_ =
            { data | userId = model.uid }
    in
        { model | users = data_ :: model.users }
            |> incrementId


addTweets : List Tweet -> Model -> Model
addTweets tweets model =
    case tweets of
        [] ->
            model

        hd :: tl ->
            addTweet hd (addTweets tl model)


addTweet : Tweet -> Model -> Model
addTweet tweet model =
    let
        tweet_ =
            { tweet | id = model.uid }
    in
        { model | unseenTimeline = tweet_ :: model.unseenTimeline }
            |> incrementId


mapTimeline : (Tweet -> Tweet) -> Model -> Model
mapTimeline f model =
    { model | timeline = List.map f model.timeline }


filterTimeline : (Tweet -> Bool) -> Model -> Model
filterTimeline f model =
    { model | timeline = List.filter f model.timeline }


filterUnseenTimeline : (Tweet -> Bool) -> Model -> Model
filterUnseenTimeline f model =
    { model | unseenTimeline = List.filter f model.unseenTimeline }


filterUsers : (UserData -> Bool) -> Model -> Model
filterUsers f model =
    { model | users = List.filter f model.users }


startRound : Model -> Model
startRound model =
    let
        incrementRound model =
            { model | roundNumber = model.roundNumber + 1 }
    in
        { model | currentPage = PlayingGame }
            |> addToHealth 200
            |> incrementRound
            |> clearUsers


endRound : Model -> Model
endRound model =
    { model | currentPage = BeforeRound }


endGame : Model -> Model
endGame model =
    { model | currentPage = GameOver }


showMoreTweets : Model -> Model
showMoreTweets model =
    let
        clearUnseen model_ =
            { model_ | unseenTimeline = [] }

        sortedUnseen =
            List.sortBy getRep model.unseenTimeline
    in
        { model | timeline = sortedUnseen ++ model.timeline }
            |> clearUnseen


addToScore : Int -> Model -> Model
addToScore n model =
    { model | score = Basics.max 0 (model.score + n) }


addToHealth : Int -> Model -> Model
addToHealth n model =
    let
        checkHealth model =
            if model.health == 0 then
                model |> endGame
            else
                model
    in
        { model | health = clamp 0 100 (model.health + n) }
            |> checkHealth


updateScore : Msg -> Model -> Model
updateScore msg model =
    let
        countResistanceTweets =
            (model.unseenTimeline ++ model.timeline)
                |> List.filter resistanceTweet
                |> List.length

        onLike data =
            case data.alignment of
                Maga ->
                    model |> addToScore 1 |> addToHealth 1

                Resist ->
                    model |> addToHealth -3

                Boring ->
                    model |> addToHealth -1

        onUnlike data =
            case data.alignment of
                Maga ->
                    model |> addToHealth -3

                Resist ->
                    model |> addToScore 1

                Boring ->
                    model |> addToHealth -1

        onBlock data =
            case data.alignment of
                Maga ->
                    model |> addToHealth -5

                Resist ->
                    model |> addToScore 5 |> addToHealth 1

                Boring ->
                    model |> addToHealth -3
    in
        case msg of
            Like tweet ->
                case tweet.user of
                    Player ->
                        model

                    NPC data ->
                        onLike data

            Unlike tweet ->
                case tweet.user of
                    Player ->
                        model

                    NPC data ->
                        onUnlike data

            Block data ->
                onBlock data

            Tick ->
                model
                    |> addToHealth (-1 * countResistanceTweets)

            _ ->
                model



-- Cmd Msg


noEffects : Model -> ( Model, Cmd Msg )
noEffects model =
    model ! []


genReplies : UserData -> List UserData -> Model -> ( Model, Cmd Msg )
genReplies hd tl model =
    let
        lambda =
            avgTweetsPerTick model
    in
        model
            ! [ generate SendTweets (makeTweetListGenerator lambda hd tl) ]


genUsers : Model -> ( Model, Cmd Msg )
genUsers model =
    let
        n =
            populationSize model
    in
        model ! List.repeat n (generate CreateUser userGenerator)



-- The update function itself


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateScore_ =
            updateScore msg

        resisters =
            List.filter resists model.users
    in
        case msg of
            Reset ->
                newGameModel |> noEffects

            NoOp ->
                model |> noEffects

            CreateUser data ->
                model |> addUser data |> noEffects

            Tick ->
                case model.users of
                    [] ->
                        model |> endRound |> noEffects

                    hd :: tl ->
                        if List.isEmpty resisters then
                            model |> endRound |> noEffects
                        else
                            model |> updateScore_ |> genReplies hd tl

            StartRound ->
                let
                    tweet =
                        makeTweet Player model.currentInput
                in
                    model
                        |> (setCurrentInput "")
                        |> (addTweet tweet)
                        |> startRound
                        |> genUsers

            EndRound ->
                model |> showMoreTweets |> endRound |> noEffects

            ShowMoreTweets ->
                model |> showMoreTweets |> noEffects

            UpdateInput str ->
                model |> (setCurrentInput str) |> noEffects

            SendTweets tweets ->
                model |> (addTweets tweets) |> noEffects

            Like tweet ->
                let
                    like t =
                        if t.id == tweet.id then
                            { t | liked = True }
                        else
                            t
                in
                    model |> (mapTimeline like) |> updateScore_ |> noEffects

            Unlike tweet ->
                let
                    unlike t =
                        if t.id == tweet.id then
                            { t | liked = False }
                        else
                            t
                in
                    model |> (mapTimeline unlike) |> updateScore_ |> noEffects

            Block data ->
                let
                    noMatch data_ =
                        data.userId /= data_.userId

                    display t =
                        case t.user of
                            User.Player ->
                                True

                            User.NPC otherData ->
                                noMatch otherData
                in
                    model
                        |> (filterTimeline display)
                        |> (filterUnseenTimeline display)
                        |> (filterUsers noMatch)
                        |> updateScore_
                        |> noEffects
