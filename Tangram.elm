module Tangram exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Math.Matrix4 exposing (Mat4, makeRotate, makeScale, makeTranslate, mul)
import Math.Vector3 exposing (Vec3, vec3)
import Tangram.Mesh as Mesh
import Tangram.Shader as Shader exposing (Attribute, Uniform)
import Tangram.Shape as Shape exposing (Position, Shape)
import Task
import WebGL exposing (Entity, Mesh)


type Message
    = Resize Float Float
    | Tick Float


type alias Model =
    { width : Float
    , height : Float
    , time : Float
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


main : Program () Model Message
main =
    Browser.element
        { init =
            always
                ( Model 0 0 0
                , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
                )
        , view = \{ time, width, height } -> view width height (time * 0.0005)
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize (\w h -> Resize (toFloat w) (toFloat h))
        ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Tick time ->
            ( { model | time = model.time + time }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )


view : Float -> Float -> Float -> Html Message
view width height t =
    let
        shape =
            interpolate shapes t

        ratio =
            width / height
    in
    WebGL.toHtml
        [ Attributes.width (round (width * 2))
        , Attributes.height (round (height * 2))
        , style "transform" "scale(0.5)"
        , style "transform-origin" "0 0"
        , style "display" "block"
        ]
        [ render ratio Mesh.parallelepiped colors.green (scaleMatrix 1) shape.parallelepiped t
        , render ratio Mesh.tetrahedron colors.orange (scaleMatrix 2) shape.orangeTetrahedron1 t
        , render ratio Mesh.cube colors.green (scaleMatrix 2) shape.cube t
        , render ratio Mesh.tetrahedron colors.orange (scaleMatrix 2) shape.orangeTetrahedron2 t
        , render ratio Mesh.tetrahedron colors.gray (scaleMatrix 4) shape.grayTetrahedron t
        , render ratio Mesh.tetrahedron colors.blue (scaleMatrix 4) shape.blueTetrahedron2 t
        , render ratio Mesh.tetrahedron colors.blue (scaleMatrix (sqrt 2 * 2)) shape.blueTetrahedron1 t
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
    clamp_ (cos t)


clamp_ : Float -> Float
clamp_ x =
    (clamp 0.2 0.8 (x ^ 2) - 0.2) / 0.6


interpolate : List Shape -> Float -> Shape
interpolate shapesList t =
    let
        current =
            truncate (t / pi)

        shape1 =
            shapesList
                |> List.drop (modBy (List.length shapes) current)
                |> List.head
                |> Maybe.withDefault Shape.default

        shape2 =
            shapesList
                |> List.drop (modBy (List.length shapes) (current + 1))
                |> List.head
                |> Maybe.withDefault Shape.default

        d =
            clamp_ (sin ((t - toFloat current * pi) / 2))
    in
    Shape.morph d shape1 shape2


rotateMat : Float -> Float -> Float -> Mat4
rotateMat rotateZ rotateY t =
    makeRotate (rotateZ * pi / 180 * f t) (vec3 0 0 1)
        |> mul (makeRotate (rotateY * pi / 180 + 2 * (1 - f t)) (vec3 0 1 0))


scaleMatrix : Float -> Mat4
scaleMatrix s =
    makeScale (vec3 s s s)


translateMat : Float -> Float -> Float -> Float -> Mat4
translateMat x y z t =
    makeTranslate (vec3 (x / (f t * f t)) (y / (f t * f t)) (z / (f t * f t)))
