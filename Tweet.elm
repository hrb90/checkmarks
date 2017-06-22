module Tweet exposing (..)

type alias Tweet =
  { content : String }

-- Constructors

makeTweet : String -> Tweet
makeTweet str =
  { content = str }

-- View logic

viewTweet : Tweet -> Html Msg
viewTweet tweet =
    li
        [ class "tweet" ]
        [ text tweet.content ]
