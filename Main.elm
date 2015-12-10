module Main where

import Effects exposing (Effects)
import Keyboard
import Signal exposing (Address, Signal)

import Html exposing (Html, text, a, p, pre, code, div, span, img, br)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)

import StartApp exposing (App, start)

main : Signal Html
main = app.html

app : App Model
app = StartApp.start { init = (init slides, Effects.none)
                     , view = view
                     , update = update
                     , inputs = [ leftRight ]
                     }

type Action = Forward
            | Backward

type alias Slide = Address (Maybe.Maybe Action) -> Html.Html
type alias UnindexedSlide = (Int, Int) -> Slide
type alias Model = (List Slide, List Slide)

slides : List UnindexedSlide
slides = [ title <| slide ""
           [ text "Разработка", nl
           , text "интерактивных", nl
           , text "Web-приложений", nl
           , text "на языке", nl
           , text "Elm", nl
           , img [ src "elm.png" ] []
           ]
         , slide "Welcome!"
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

         , title <| slide "The End" <:: text "Questions?"
         ]

init : List UnindexedSlide -> Model
init slides =
  let
    count = List.length slides
  in
    ( slides
        |> List.indexedMap (\idx s -> s (idx + 1, count))
    , []
    )

view : Address (Maybe Action) -> Model -> Html.Html
view address model =
  case model of
    ((s :: _), _) -> s address
    _ -> text "Oops!"


update : Maybe Action -> Model -> (Model, Effects a)
update action model =
  ( case (action, model) of
      (Just Forward, ((s :: s' :: rest), prev)) ->
        (s' :: rest, s :: prev)

      (Just Backward, (rest, (s :: prev))) ->
        (s :: rest, prev)

      _ -> model
  , Effects.none
  )

-- input

leftRight : Signal (Maybe Action)
leftRight =
  Keyboard.arrows
    |> Signal.map (\ev ->
                     case (ev.x, ev.y) of
                       (-1, 0) -> Just Backward
                       (1,  0) -> Just Forward
                       _       -> Nothing
                  )

-- view helpers

slide : String -> List Html -> UnindexedSlide
slide header content (idx, count) address =
  let
    navButton active label action =
      a (List.append
               [ href "#" ]
               (if active
                then [ class "nav-btn"
                     , onClick address action ]
                else [ class "nav-btn disabled" ]
               )
        ) [ text label ]
  in
    div [ class "slide"
        ] [ div [ class "header" ] <:: text header
          , div [ class "frame" ] [ div [ class "content" ] content ]
          , div [ class "nav-bar" ]
            [ navButton (idx > 1) "<<" (Just Backward)
            , div [ class "pages" ] [ text <| toString idx
                                    , text "/"
                                    , text <| toString count
                                    ]
            , navButton (idx < count) ">>" (Just Forward)
            ]
          ]

title : UnindexedSlide -> UnindexedSlide
title s i a = div [ class "title" ] [ s i a ]

nl : Html
nl = br [] []

source : String -> String -> Html
source hl s = pre [ class "src" ] <:: code [ class hl ] <:: text s


infixr 9 <::
(<::) : (List a -> b) -> a -> b
(<::) f x = f [ x ]
