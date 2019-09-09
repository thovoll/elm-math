module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter addition sentence", value model.content, onInput Change ] []
        , mathAlgoHtml model.content
        ]


mathAlgoHtml : String -> Html msg
mathAlgoHtml mathSentence =
    let
        pieces =
            String.split "+" mathSentence

        wrappedPieces =
            List.map (\p -> "[" ++ p ++ "]") pieces

        piecesString =
            String.join " " wrappedPieces
    in
    div []
        [ pre [] [ text (mathAlgoText pieces) ]
        , p [] [ text ("Debug: " ++ piecesString) ]
        ]


mathAlgoText : List String -> String
mathAlgoText pieces =
    case pieces of
        a :: b :: _ ->
            mathAlgoText2 a b

        _ ->
            "I don't understand. Type something like this: 342+465"


mathAlgoText2 : String -> String -> String
mathAlgoText2 a b =
    let
        indentDepth =
            3

        indentString =
            String.repeat indentDepth " "

        maxChars =
            Basics.max (String.length a) (String.length b)

        paddedA =
            String.padLeft maxChars ' ' a

        paddedB =
            String.padLeft maxChars ' ' b
    in
    indentString
        ++ paddedA
        ++ "\n"
        ++ String.pad indentDepth ' ' "+"
        ++ paddedB
        ++ "\n"
        ++ indentString
        ++ String.repeat maxChars "="


mathAlgoText100 : String -> String
mathAlgoText100 mathSentence =
    case String.indices "+" mathSentence of
        [] ->
            "No + found"

        first :: rest ->
            String.fromInt first


mathAlgoText101 : String -> String
mathAlgoText101 mathSentence =
    String.indices "+" mathSentence
        |> List.head
        |> Maybe.withDefault -1
        |> String.fromInt
