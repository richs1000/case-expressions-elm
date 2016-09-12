module Question exposing (..)

import RandomStuff exposing (pickOne, pickABunch, compressList)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


type Suit
    = Club
    | Diamond
    | Heart
    | Spade


type Rank
    = Jack
    | Queen
    | King
    | Ace
    | Num Int


type CardType
    = Card ( Suit, Rank )
    | Wildcard


mystery : CardType -> Int
mystery c =
    case c of
        Card ( Spade, _ ) ->
            1

        Card ( _, Ace ) ->
            2

        Card ( Heart, _ ) ->
            3

        Card ( _, Jack ) ->
            4

        Card ( _, Num i ) ->
            i

        Card ( _, _ ) ->
            5

        Wildcard ->
            6


cardToString : CardType -> String
cardToString c =
    case c of
        Wildcard ->
            "Wildcard"

        Card ( s, Num i ) ->
            "Card(" ++ toString (s) ++ ", Num " ++ toString (i) ++ ")"

        Card ( s, r ) ->
            "Card(" ++ toString (s) ++ ", " ++ toString (r) ++ ")"


randomCard : List Int -> CardType
randomCard randomValues =
    let
        suit =
            pickOne randomValues [ Club, Diamond, Heart, Spade ] Club

        rank =
            pickOne
                (List.drop 1 randomValues)
                [ Jack, Queen, King, Ace, Num 2, Num 3, Num 4, Num 5, Num 6 ]
                Queen

        wildCard =
            pickOne (List.drop 2 randomValues) [ 1, 2, 3, 4, 5 ] 1
    in
        if wildCard == 5 then
            Wildcard
        else
            Card ( suit, rank )


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        card =
            randomCard randomValues

        question' =
            [ "What is the value of ans after the following "
            , "ML expressions are evaluated?"
            , ""
            , "datatype suit = Club | Diamond | Heart | Spade"
            , "datatype rank = Jack | Queen | King | Ace | Num of int"
            , "datatype cardType = Card of suit * rank | Wildcard"
            , ""
            , "fun mystery (c : cardType) ="
            , "    case c of"
            , "        Card(Spade, _) => 1"
            , "      | Card(_, Ace) => 2"
            , "      | Card(Heart, _) => 3"
            , "      | Card(_, Jack) => 4"
            , "      | Card(_, Num i) => i"
            , "      | Card(_, _) => 5"
            , "      | Wildcard => 6"
            , ""
            , "val my_card = " ++ (cardToString card)
            , "val ans = mystery(my_card)"
            , ""
            ]

        answer' =
            toString (mystery card)

        distractors =
            [ "1"
            , "2"
            , "3"
            , "4"
            , "5"
            , "6"
            ]

        ( _, distractors' ) =
            List.partition (\d -> d == answer') (compressList distractors)
    in
        { question = question'
        , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors'
        , answer = ( answer', "Correct" )
        , format = MultipleChoice
        }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
