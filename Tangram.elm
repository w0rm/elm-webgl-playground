module Tangram exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 exposing (makeRotate, makeScale, makeTranslate, mul, Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Tangram.Mesh as Mesh
import Tangram.Shader as Shader exposing (Uniform, Varying, Attribute)
import Tangram.Shape as Shape exposing (Shape, Position)
import WebGL exposing (Entity, Mesh)
import Window
import Time exposing (Time)
import Task


type Message
    = Resize Window.Size
    | Tick Time


type alias Model =
    { size : Window.Size
    , time : Time
    }


shapes : List Shape
shapes =
    List.intersperse Shape.elm
        [ Shape.elmBerlin
        , Shape.elmCommunity
        , Shape.elmWeekly
        , Shape.elmBridge
        , Shape.elmLive
        ]
        |> (::) Shape.elm


main : Program Never Model Message
main =
    Html.program
        { init = ( Model (Window.Size 0 0) 0, Task.perform Resize Window.size )
        , view = \{ time, size } -> view size (time * 0.001)
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Tick time ->
            { model | time = model.time + time } ! []

        Resize size ->
            { model | size = size } ! []


view : Window.Size -> Time -> Html Message
view size t =
    let
        shape =
            interpolate shapes t

        ratio =
            toFloat size.width / toFloat size.height
    in
        WebGL.toHtml
            [ width (size.width * 2)
            , height (size.height * 2)
            , style
                [ ( "transform", "scale(0.5)" )
                , ( "transform-origin", "0 0" )
                , ( "display", "block" )
                ]
            ]
            [ render ratio Mesh.parallelepiped colors.green (scaleMat 1) shape.parallelepiped t
            , render ratio Mesh.tetrahedron colors.orange (scaleMat 2) shape.orangeTetrahedron1 t
            , render ratio Mesh.cube colors.green (scaleMat 2) shape.cube t
            , render ratio Mesh.tetrahedron colors.orange (scaleMat 2) shape.orangeTetrahedron2 t
            , render ratio Mesh.tetrahedron colors.gray (scaleMat 4) shape.grayTetrahedron t
            , render ratio Mesh.tetrahedron colors.blue (scaleMat 4) shape.blueTetrahedron2 t
            , render ratio Mesh.tetrahedron colors.blue (scaleMat (sqrt 2 * 2)) shape.blueTetrahedron1 t
            ]


render : Float -> Mesh Attribute -> Vec3 -> Mat4 -> Position -> Float -> Entity
render ratio shape color scaleMat { rotateZ, rotateY, x, y, z } t =
    WebGL.entity
        Shader.vertex
        Shader.fragment
        shape
        (Uniform color (rotateMat rotateZ rotateY t) scaleMat (translateMat x y z t) (camera ratio))


colors :
    { gray : Vec3
    , green : Vec3
    , orange : Vec3
    , blue : Vec3
    }
colors =
    { gray = vec3 0.353 0.388 0.47
    , green = vec3 0.514 0.784 0.2
    , orange = vec3 0.937 0.647 0
    , blue = vec3 0.372 0.706 0.792
    }


camera : Float -> Mat4
camera ratio =
    let
        eye =
            vec3 0 0 30

        center =
            vec3 0 0 0
    in
        mul (Math.Matrix4.makePerspective 45 ratio 0.01 100)
            (Math.Matrix4.makeLookAt eye center Math.Vector3.j)


f : Float -> Float
f t =
    abs (cos t)


interpolate : List Shape -> Float -> Shape
interpolate shapes t =
    let
        current =
            truncate (t / pi)

        shape1 =
            shapes
                |> List.drop (current % List.length shapes)
                |> List.head
                |> Maybe.withDefault Shape.default

        shape2 =
            shapes
                |> List.drop ((current + 1) % List.length shapes)
                |> List.head
                |> Maybe.withDefault Shape.default

        d =
            sin ((t - toFloat current * pi) / 2)
    in
        Shape.morph d shape1 shape2


rotateMat : Float -> Float -> Float -> Mat4
rotateMat rotateZ rotateY t =
    makeRotate (rotateZ * pi / 180 * f t) (vec3 0 0 1)
        |> mul (makeRotate (rotateY * pi / 180 + 2 * (1 - f t)) (vec3 0 1 0))


scaleMat : Float -> Mat4
scaleMat s =
    makeScale (vec3 s s s)


translateMat : Float -> Float -> Float -> Float -> Mat4
translateMat x y z t =
    makeTranslate (vec3 (x / (f t * f t)) (y / (f t * f t)) (z / (f t * f t)))
