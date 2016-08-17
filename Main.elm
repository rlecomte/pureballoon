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
import Model exposing(grid,Cell)

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

generateCircle: Cell -> Collage.Form
generateCircle cell = circleGenerator cell.caseType (toFloat ((cell.row * 50) + 25 - 175)) (toFloat ((cell.column * 50) + 25 - 200))

circleGenerator: Int -> Float -> Float -> Collage.Form
circleGenerator i x y = Collage.filled (randomColor i) (Collage.circle 20)
  |> Collage.move (x,y)

randomColor: Int -> Color
randomColor i =
  case i of
    1 -> Color.blue
    2 -> Color.red
    3 -> Color.green
    4 -> Color.black
    _ -> Color.charcoal

renderPoint = Model.grid |> Task.map(List.map(generateCircle))

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
