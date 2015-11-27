module Main where

import Keyboard
import Signal exposing (Address)

import Effects
import Html exposing (text, h1, p, pre, code, div, span, button)
import Html.Attributes exposing (id, style, disabled, class)
import Html.Events exposing (onClick)
import StartApp

main =
  .html
  <| StartApp.start { init   = (init slides, Effects.none)
                    , view   = view
                    , update = update
                    , inputs = [ leftRight ]
                    }

type Action = Forward
            | Backward

type alias Slide = Address (Maybe.Maybe Action) -> Html.Html
type alias Model = (List Slide, List Slide)

slides : List ((Int, Int) -> Slide)
slides = [ slide "Welcome!"
           <:: p []
              [ text "Some code:"
              , source "elm" "-- simpliest Elm-app
import Html exposing (text)
import SimpleApp.Simple as Simple

main = Simple.start
       { model  = model
       , view   = view
       , update = update
       }

model          = \"Hello World!\"

view   _ model = text model

update _ model = model"
              ]

         , slide "Empty" []

         , slide "The End" <:: p [] <:: text "Questions?"
         ]

init slides =
  let count = List.length slides
  in  slides
    |> List.indexedMap (\idx s -> s (idx + 1, count))
    |> (flip (,) [])

view address model =
  case model of
    ((s :: _), _) -> s address
    _             -> text "Oops!"

update action model =
  ( case (action, model) of
      (Just Forward,  ((s :: s' :: rest), prev)) ->
        (s' :: rest, s :: prev)

      (Just Backward, (rest, (s :: prev))) ->
        (s  :: rest,      prev)

      _ -> model
  , Effects.none
  )

-- input

leftRight : Signal.Signal (Maybe.Maybe Action)
leftRight =
  Keyboard.arrows
    |> Signal.map (\ev ->
                     case (ev.x, ev.y) of
                       (-1, 0) -> Just Backward
                       (1,  0) -> Just Forward
                       _       -> Nothing
                  )

-- view helpers

slide : String -> List Html.Html -> (Int, Int) -> Slide
slide header content (idx, count) address =
  let navButton active label action =
        button [ style [("cursor", "pointer")]
               , onClick address action
               , disabled <| not active
               ] [ text label ]
  in  div [ id "slide" ]
        [ h1 [ id "header" ] <:: text header
        , div [ id "content" ] content
        , div [ id "navigation" ]
          [ navButton (idx > 1)     "<<" <| Just Backward
          , text <| toString idx
          , text "/"
          , text <| toString count
          , navButton (idx < count) ">>" <| Just Forward
          ]
        ]

source hl s = pre [ class "src" ] <:: code [ class hl ] <:: text s

infixr 9 <::
(<::) f x = f [ x ]
