module SlideShow
  ( SlideShowApp, start
  , Slide, slide, title
  , img_, ul_, code_, nl, source
  ) where

import Effects exposing (Effects)
import Keyboard
import Signal exposing (Address, Signal)

import Html exposing (Html, text, a, p, pre, code, div, span, img, br, ul, li)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)

import StartApp exposing (App, start)

type Action = Forward | Backward

type alias IndexedSlide = Address (Maybe.Maybe Action) -> Html.Html

type alias Slide = (Int, Int) -> IndexedSlide

type alias Model = (List IndexedSlide, List IndexedSlide)


type alias SlideShowApp = App Model


start : List Slide -> SlideShowApp
start slides =
  StartApp.start
  { init = (init slides, Effects.none)
  , view = view
  , update = update
  , inputs = [ leftRight, pageUpDown ]
  }


init : List Slide -> Model
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
    |> Signal.map
       (\ev ->
          case (ev.x, ev.y) of
            (-1, 0) -> Just Backward
            (1,  0) -> Just Forward
            _ -> Nothing
       )

pageUpDown : Signal (Maybe Action)
pageUpDown =
  Keyboard.presses
    |> Signal.map
       (\key ->
          case key of
            33 -> Just Backward  -- PgUp
            34 -> Just Forward  -- PgDn
            _ -> Nothing
       )

-- view helpers

slide : String -> List Html -> Slide
slide header content (idx, count) address =
  let
    navButton active label action =
      a ( List.append
            [ href "#" ]
            (if active
             then [ class "nav-btn"
                  , onClick address action ]
             else [ class "nav-btn disabled" ]
            )
        ) [ text label ]
  in
    div [ class "slide" ]
    [ div [ class "header" ] <:: text header
    , div [ class "frame" ] [ div [ class "content" ] content ]
    , div [ class "nav-bar" ]
        [ navButton (idx > 1) "<<" (Just Backward)
        , div [ class "pages" ]
            [ text <| toString idx
            , text "/"
            , text <| toString count
            ]
        , navButton (idx < count) ">>" (Just Forward)
        ]
    ]


title : Slide -> Slide
title s i a = div [ class "title" ] [ s i a ]


nl : Html
nl = br [] []


ul_ : List Html -> Html
ul_ = ul [] << List.map ((<::) (li []))


code_ : String -> Html
code_ = (<::) (code []) << text


img_ : String -> Html
img_ s = img [ src s ] []


source : String -> String -> Html
source syntax s =
  pre [ class "src" ]
  <:: code [ class syntax ]
  <:: text s


infixr 9 <::
(<::) : (List a -> b) -> a -> b
(<::) f x = f [ x ]
