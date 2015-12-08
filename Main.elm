module Main where

import Effects         exposing (Effects)
import Keyboard
import Signal          exposing (Address)

import Html            exposing (text, h1, p, pre, code, div, span, button)
import Html.Attributes exposing (id, style, disabled, class)
import Html.Events     exposing (onClick)

import Markdown

import StartApp

main : Signal.Signal Html.Html
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
type alias UnindexedSlide = (Int, Int) -> Slide
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

         , slide "Markdown" <:: md """# Title

some code:
```python
print "Hello World"
```
"""
         , slide "The End" <:: p [] <:: text "Questions?"
         ]

init : List UnindexedSlide -> Model
init slides =
  let count = List.length slides
  in  slides
    |> List.indexedMap (\idx s -> s (idx + 1, count))
    |> (flip (,) [])

view : Address (Maybe Action) -> Model -> Html.Html
view address model =
  case model of
    ((s :: _), _) -> s address
    _             -> text "Oops!"

update : Maybe Action -> Model -> (Model, Effects a)
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

slide : String -> List Html.Html -> UnindexedSlide
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

source : String -> String -> Html.Html
source hl s = pre [ class "src" ] <:: code [ class hl ] <:: text s

md : String -> Html.Html
md = Markdown.toElement >> Html.fromElement

infixr 9 <::
(<::) : (List a -> b) -> a -> b
(<::) f x = f [ x ]
