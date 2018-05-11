module Main exposing (main)

import AnimationFrame
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Keyboard exposing (KeyCode, downs)
import Math.Matrix4 as Mat4 exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random exposing (..)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


type Msg
    = Tick Time
    | KeyDown KeyCode
    | NewFood (List Position) Int


type alias Position =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Right
    | Left


type alias Model =
    { head : Position
    , tail : List Position
    , food : Maybe Position
    , direction : Direction
    , time : Time
    , lastMove : Time
    , lastMoveDirection : Direction
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getNextPosition initialModel.head initialModel.tail )


initialModel : Model
initialModel =
    Model ( 4, 4 ) [] Nothing Right 0 0 Right


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
        ((head model.head
            :: List.map
                (\c -> tail c)
                model.tail
         )
            ++ [ food model.food ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timediff ->
            let
                time =
                    (timediff / 1000) + model.time

                ( head, tail, food, lastMove, lastMoveDirection ) =
                    if time - model.lastMove >= 1 then
                        let
                            ( head, lastMoveDirection ) =
                                nextPosition model

                            ( tail, food ) =
                                newTail head model
                        in
                        ( head, tail, food, time, lastMoveDirection )
                    else
                        ( model.head, model.tail, model.food, model.lastMove, model.lastMoveDirection )

                ( x, y ) =
                    head

                hitBorder =
                    x < 0 || x > 9 || y < 0 || y > 9

                hitTail =
                    (List.filter (\( tx, ty ) -> tx == x && ty == y) tail |> List.length) > 0

                newModel =
                    if hitBorder || hitTail then
                        initialModel
                    else
                        { model
                            | time = time
                            , head = head
                            , tail = tail
                            , food = food
                            , lastMove = lastMove
                            , lastMoveDirection = lastMoveDirection
                        }
            in
            newModel
                ! [ case food of
                        Nothing ->
                            getNextPosition head tail

                        Just _ ->
                            Cmd.none
                  ]

        KeyDown key ->
            { model | direction = dirForKey key model.direction model.lastMoveDirection } ! []

        NewFood positions selection ->
            let
                food =
                    List.drop (selection - 1) positions
                        |> List.head
            in
            { model | food = food } ! []


nextPosition : Model -> ( Position, Direction )
nextPosition model =
    let
        ( x, y ) =
            model.head
    in
    case model.direction of
        Up ->
            ( ( x, y + 1 ), Up )

        Down ->
            ( ( x, y - 1 ), Down )

        Left ->
            ( ( x - 1, y ), Left )

        Right ->
            ( ( x + 1, y ), Right )


newTail : Position -> Model -> ( List Position, Maybe Position )
newTail head model =
    let
        ( x, y ) =
            head

        gotFood =
            case model.food of
                Nothing ->
                    False

                Just ( fx, fy ) ->
                    x == fx && y == fy

        newTail : List Position
        newTail =
            model.head :: model.tail
    in
    if gotFood then
        ( newTail, Nothing )
    else
        ( newTail
            |> List.reverse
            |> List.tail
            |> Maybe.withDefault []
            |> List.reverse
        , model.food
        )


dirForKey : KeyCode -> Direction -> Direction -> Direction
dirForKey key previous lastMove =
    case key of
        38 ->
            if lastMove == Down then
                Down
            else
                Up

        40 ->
            if lastMove == Up then
                Up
            else
                Down

        37 ->
            if lastMove == Right then
                Right
            else
                Left

        39 ->
            if lastMove == Left then
                Left
            else
                Right

        _ ->
            previous


getNextPosition : Position -> List Position -> Cmd Msg
getNextPosition head tail =
    let
        snake =
            head :: tail

        available =
            List.filter
                (\i -> not (List.member i snake))
                allFields
    in
    Random.generate (NewFood available) (Random.int 0 (List.length available))


allFields : List Position
allFields =
    let
        valueRange =
            List.range 0 9
    in
    List.map (\x -> ( x, 0 )) valueRange
        |> List.concatMap (\( x, _ ) -> List.map (\y -> ( x, y )) valueRange)


food : Maybe Position -> WebGL.Entity
food position =
    case position of
        Just p ->
            draw (vec3 0 1 0) p

        Nothing ->
            draw (vec3 0 0 0) ( -10, -10 )


head : Position -> WebGL.Entity
head =
    draw (vec3 1 0 0)


tail : Position -> WebGL.Entity
tail =
    draw (vec3 1 0 0)


draw : Vec3 -> Position -> WebGL.Entity
draw color ( x, y ) =
    let
        xpos =
            toFloat x * 0.2 - 1 + 0.1

        ypos =
            toFloat y * 0.2 - 1 + 0.1
    in
    square color ( xpos, ypos, 0 ) ( 0.1, 0.1, 0.1 ) 0


type alias Translation =
    ( Float, Float, Float )


type alias Scale =
    ( Float, Float, Float )


type alias Rotation =
    Float


square : Vec3 -> Translation -> Scale -> Rotation -> WebGL.Entity
square color =
    element (squareMesh color)


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


squareMesh : Vec3 -> Mesh Vertex
squareMesh color =
    WebGL.triangles
        [ ( Vertex (vec3 -1 -1 0) color, Vertex (vec3 1 -1 0) color, Vertex (vec3 1 1 0) color )
        , ( Vertex (vec3 1 1 0) color, Vertex (vec3 -1 1 0) color, Vertex (vec3 -1 -1 0) color )
        ]


type alias Uniforms =
    { perspective : Mat4, transform : Mat4 }


type alias Vertex =
    { position : Vec3, color : Vec3 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vcolor;
        void main () {
            gl_Position = transform * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
