import Basics exposing (Never)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.App as Html
import Task exposing (Task)
import Collage
import Element
import Color exposing (Color)
import Keyboard exposing (KeyCode)

-- MESSAGES

type Msg
    = Init
    | UserCommand Direction
    | RenderSuccess (List Collage.Form)
    | RenderError

-- VIEW

view model =
  div
  [ style ["padding" => "30px 0"]
  ]
  [ div
    [ style
      [ "height" => "1680px"
      , "margin" => "auto"
      , "position" => "relative"
      , "width" => "1480px"
      ]
    ]
    [ renderWell model
    ]
  ]

renderWell model = model |> Collage.collage (700) (900) |> Element.toHtml

-- MAIN


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- ALIAS

(=>) : a -> b -> (a, b)
(=>) = (,)

type alias Model = (List Collage.Form)

type Direction = LEFT | RIGHT | UP | DOWN | NOOP
toDirection : KeyCode -> Direction
toDirection code =
    case (code) of
    113 -> LEFT
    100 -> RIGHT
    122 -> UP
    115 -> DOWN
    _ -> NOOP

-- Main Agent

init : ( Model, Cmd Msg )
init =
    ( [], Task.perform (always Init) (always Init) (Task.succeed 0) )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Init -> ( [], renderCmd )

        RenderSuccess forms ->
            ( forms, Cmd.none )

        RenderError ->
            ( [], Cmd.none )

        UserCommand d ->
           ( [], moveCmd d model )

renderCmd : Cmd Msg
renderCmd = Task.perform (always RenderError) (\x -> RenderSuccess x) render

moveCmd : Direction -> Model -> Cmd Msg
moveCmd direction model = Task.perform (always RenderError) (\x -> RenderSuccess x) (move direction model)

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses (\code -> UserCommand(toDirection(code)) )

-- FUNCTION

render: Task.Task a Model
render = Task.succeed (generateCircle 1 0 0 :: [])

move: Direction -> Model -> Task.Task a Model
move direction model =
    let
        moveFun =
            case direction of
            UP -> Collage.moveY 10.0
            DOWN -> Collage.moveY -10.0
            RIGHT -> Collage.moveX 10.0
            LEFT -> Collage.moveX -10.0
            _ -> Collage.moveX 0.0
    in List.map moveFun model |> Task.succeed

generateCircle: Int -> Float -> Float -> Collage.Form
generateCircle i x y = Collage.filled (Color.yellow) (Collage.circle 20) |> Collage.move (x,y)
