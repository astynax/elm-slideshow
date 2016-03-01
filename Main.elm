module Main where

import Html exposing (text, li)

import SlideShow exposing (..)

main = start slides |> .html

slides =
  [ title <| slide ""
      [ text "Elm powered", nl
      , text "SlideShow", nl
      , text "(demo)"
      ]
  , slide "Unordered list"
      [ text "Fruits"
      , ul_
          [ text "Apple"
          , text "Orange"
          , text "Mango"
          , text "Tangerine"
          ]
      ]
  , slide "Source code (lines)"
      [ text "Elm utilities:"
      , ul_ <| List.map code_
          [ "elm-make"
          , "elm-package"
          , "elm-reactor"
          , "elm-repl"
          ]
      ]
  , slide "Source code (block)"
      [ source "elm" "import Html exposing (Html, text)
import Mouse
import Signal exposing (Signal)

main : Signal Html
main =
  Signal.map (text <| toString)
    Mouse.position"
      ]
  , title <| slide "" [ text "The End" ]
  ]
