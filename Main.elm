import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Collage
import Element
import Color exposing (Color)
import Task exposing (Task)
import Html.App as Html
import List exposing (filter, map, concatMap)
import Date
import Random

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias Model = (List Collage.Form)


init : ( Model, Cmd Msg )
init =
    ( [], Task.perform (always Init) (always Init) (Task.succeed 0) )

-- MESSAGES

type Msg
    = Init
    | RenderPointsSuccess (List Collage.Form)
    | RenderPointsError

-- VIEW

view model =
  div
  [ style ["padding" => "30px 0"]
  ]
  [ div
    [ style
      [ "height" => "680px"
      , "margin" => "auto"
      , "position" => "relative"
      , "width" => "480px"
      ]
    ]
    [ renderWell model
    ]
  ]

circleGenerator: Float -> Float -> Task a Collage.Form
circleGenerator x y = Date.now
  |> Task.map(Date.millisecond)
  |> Task.map(Random.initialSeed)
  |> Task.map(Random.step (Random.int 0 3))
  |> Task.map(\(i,_) -> randomColor(i))
  |> Task.map(\c -> Collage.filled c (Collage.circle 20))
  |> Task.map(\s -> Collage.move (x,y) s)

randomColor: Int -> Color
randomColor i =
  case i of
    3 -> Color.blue
    4 -> Color.red
    5 -> Color.green
    6 -> Color.black
    _ -> Color.charcoal

renderPoint =
  let xs = filter (\x -> x % 50 == 0) [0..300] |> map(\x -> x + 25) |> map(\x -> toFloat(x - 175))
      ys = filter (\x -> x % 50 == 0) [0..400] |> map(\x -> x + 25) |> map(\x -> toFloat(x - 200))
  in Task.sequence(concatMap(\x -> map(\y -> circleGenerator x y) ys) xs)
--7x9
renderWell model = model
  |> Collage.collage (700) (900)
  |> Element.toHtml

renderCmd : Cmd Msg
renderCmd =
    Task.perform (always RenderPointsError) (\x -> RenderPointsSuccess x) renderPoint

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Init -> ( [], renderCmd )

        RenderPointsSuccess forms ->
            ( forms, Cmd.none )

        RenderPointsError ->
            ( [], Cmd.none )

-- MAIN


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
