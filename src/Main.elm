module Main exposing (main)

import AnimationFrame
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Keyboard exposing (KeyCode, downs)
import Math.Matrix4 as Mat4 exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


type alias Position =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Right
    | Left


type alias Model =
    { position : Position
    , direction : Direction
    , time : Time
    , lastMove : Time
    }


type Msg
    = Tick Time
    | KeyDown KeyCode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timediff ->
            { model | time = (timediff / 1000) + model.time } ! [ Cmd.none ]

        KeyDown key ->
            { model | direction = dirForKey key model.direction } ! []


dirForKey : KeyCode -> Direction -> Direction
dirForKey key previous =
    case key of
        38 ->
            Up

        40 ->
            Down

        37 ->
            Left

        39 ->
            Right

        _ ->
            previous


init : ( Model, Cmd Msg )
init =
    ( Model ( 4, 4 ) Right 0 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AnimationFrame.diffs Tick, Keyboard.downs KeyDown ]


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 400
        , height 400
        , style [ ( "display", "block" ) ]
        ]
        [ triangle ( -0.5, 0, 0 ) ( 0.25, 0.25, 0.25 ) model.time
        , square ( 0.5, 0, 0 ) ( 0.25, 0.25, 0.25 ) (model.time / 2)
        ]


type alias Translation =
    ( Float, Float, Float )


type alias Scale =
    ( Float, Float, Float )


type alias Rotation =
    Float


square : Translation -> Scale -> Rotation -> WebGL.Entity
square =
    element squareMesh


triangle : Translation -> Scale -> Rotation -> WebGL.Entity
triangle =
    element triangleMesh


element : Mesh Vertex -> Translation -> Scale -> Rotation -> WebGL.Entity
element mesh translation scale time =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = perspective (time / 1000), transform = transform translation scale time }


transform : Translation -> Scale -> Rotation -> Mat4
transform ( tx, ty, tz ) ( sx, sy, sz ) r =
    makeScale (vec3 sx sy sz)
        |> mul (makeRotate r (vec3 0 1 0))
        |> mul (makeTranslate (vec3 tx ty tz))


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


triangleMesh : Mesh Vertex
triangleMesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0), Vertex (vec3 1 -1 0), Vertex (vec3 0 1 0) )
        ]


squareMesh : Mesh Vertex
squareMesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0), Vertex (vec3 1 -1 0), Vertex (vec3 1 1 0) )
        , ( Vertex (vec3 1 1 0), Vertex (vec3 -1 1 0), Vertex (vec3 -1 -1 0) )
        ]


type alias Uniforms =
    { perspective : Mat4, transform : Mat4 }


type alias Vertex =
    { position : Vec3 }


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 perspective;
        uniform mat4 transform;
        void main () {
            gl_Position = transform * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        void main () {
            gl_FragColor = vec4(vec3( 1, 0, 0), 1.0);
        }
    |]
