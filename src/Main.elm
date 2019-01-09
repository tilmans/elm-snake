module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (Direction(..))
import Math.Matrix4 as Mat4 exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random exposing (..)
import WebGL exposing (Mesh, Shader)


type Msg
    = Tick Float
    | KeyMsg Keyboard.Msg
    | NewFood (List Position) Int


type alias Position =
    ( Int, Int )


type MovementDirection
    = Up
    | Down
    | Right
    | Left


type alias Model =
    { head : Position
    , tail : List Position
    , food : Maybe Position
    , direction : List Key
    , time : Float
    , lastMove : Float
    , lastMoveDirection : MovementDirection
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getNextPosition initialModel.head initialModel.tail )


initialModel : Model
initialModel =
    Model ( 4, 4 ) [] Nothing [] 0 0 Right


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
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
                    (timediff / 300) + model.time

                gameState =
                    if time - model.lastMove >= 1 then
                        let
                            ( head_, lastMoveDirection ) =
                                nextPosition model

                            ( tail_, food_ ) =
                                newTail head_ model
                        in
                        { head = head_, tail = tail_, food = food_, lastMove = time, lastMoveDirection = lastMoveDirection }

                    else
                        { head = model.head, tail = model.tail, food = model.food, lastMove = model.lastMove, lastMoveDirection = model.lastMoveDirection }

                ( x, y ) =
                    gameState.head

                hitBorder =
                    x < 0 || x > 9 || y < 0 || y > 9

                hitTail =
                    (List.filter (\( tx, ty ) -> tx == x && ty == y) gameState.tail |> List.length) > 0

                newModel =
                    if hitBorder || hitTail then
                        initialModel

                    else
                        { model
                            | time = time
                            , head = gameState.head
                            , tail = gameState.tail
                            , food = gameState.food
                            , lastMove = gameState.lastMove
                            , lastMoveDirection = gameState.lastMoveDirection
                        }
            in
            ( newModel
            , case gameState.food of
                Nothing ->
                    getNextPosition gameState.head gameState.tail

                Just _ ->
                    Cmd.none
            )

        KeyMsg key ->
            let
                arrows =
                    Keyboard.update key model.direction

                newDirection =
                    Keyboard.Arrows.arrowsDirection arrows

                newMovement =
                    case newDirection of
                        North ->
                            Up

                        South ->
                            Down

                        West ->
                            Left

                        East ->
                            Right

                        _ ->
                            model.lastMoveDirection
            in
            ( { model | lastMoveDirection = newMovement, direction = arrows }, Cmd.none )

        NewFood positions selection ->
            let
                foodItems =
                    List.drop (selection - 1) positions
                        |> List.head
            in
            ( { model | food = foodItems }, Cmd.none )


nextPosition : Model -> ( Position, MovementDirection )
nextPosition model =
    let
        ( x, y ) =
            model.head
    in
    case model.lastMoveDirection of
        Up ->
            ( ( x, y + 1 ), Up )

        Down ->
            ( ( x, y - 1 ), Down )

        Left ->
            ( ( x - 1, y ), Left )

        Right ->
            ( ( x + 1, y ), Right )


newTail : Position -> Model -> ( List Position, Maybe Position )
newTail head_ model =
    let
        ( x, y ) =
            head_

        gotFood =
            case model.food of
                Nothing ->
                    False

                Just ( fx, fy ) ->
                    x == fx && y == fy

        newTail_ : List Position
        newTail_ =
            model.head :: model.tail
    in
    if gotFood then
        ( newTail_, Nothing )

    else
        ( newTail_
            |> List.reverse
            |> List.tail
            |> Maybe.withDefault []
            |> List.reverse
        , model.food
        )


getNextPosition : Position -> List Position -> Cmd Msg
getNextPosition head_ tail_ =
    let
        snake =
            head_ :: tail_

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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
