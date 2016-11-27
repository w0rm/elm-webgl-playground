module Tangram exposing (main)

import Tangram.Shader as Shader exposing (Uniform, Varying, Attribute)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (makeRotate, makeScale, makeTranslate, mul, Mat4)
import Html exposing (Html)
import Html.Attributes exposing (width, height)
import WebGL
import AnimationFrame
import Tangram.Mesh as Mesh
import Tangram.Shape as Shape exposing (Shape, Position)


shapes : List Shape
shapes =
    List.intersperse Shape.elm
        [ Shape.elmCommunity
        , Shape.elmWeekly
        , Shape.elmBridge
        , Shape.elmLive
        ]
        |> (::) Shape.elm


main : Program Never Float Float
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = \t -> view (t * 0.001)
        , update = \dt t -> ( t + dt, Cmd.none )
        , subscriptions = always (AnimationFrame.diffs identity)
        }


view : Float -> Html Float
view t =
    let
        shape =
            interpolate shapes t
    in
        WebGL.toHtml [ width 400, height 400 ]
            [ render Mesh.parallelepiped colors.green (scaleMat 1) shape.parallelepiped t
            , render Mesh.tetrahedron colors.orange (scaleMat 2) shape.orangeTetrahedron1 t
            , render Mesh.cube colors.green (scaleMat 2) shape.cube t
            , render Mesh.tetrahedron colors.orange (scaleMat 2) shape.orangeTetrahedron2 t
            , render Mesh.tetrahedron colors.gray (scaleMat 4) shape.grayTetrahedron t
            , render Mesh.tetrahedron colors.blue (scaleMat 4) shape.blueTetrahedron2 t
            , render Mesh.tetrahedron colors.blue (scaleMat (sqrt 2 * 2)) shape.blueTetrahedron1 t
            ]


render : WebGL.Drawable Attribute -> Vec3 -> Mat4 -> Position -> Float -> WebGL.Renderable
render shape color scaleMat { rotateZ, rotateY, x, y, z } t =
    WebGL.render
        Shader.vertex
        Shader.fragment
        shape
        (Uniform color (rotateMat rotateZ rotateY t) scaleMat (translateMat x y z t) camera)


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


camera : Mat4
camera =
    let
        eye =
            vec3 0 0 30

        center =
            vec3 0 0 0
    in
        mul (Math.Matrix4.makePerspective 45 1 0.01 100)
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
