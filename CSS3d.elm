module CSS3d exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onMouseMove, onResize)
import Html exposing (Html, div, text)
import Html.Attributes as Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest


type alias Model =
    { width : Float
    , height : Float
    , left : Float
    , top : Float
    }


type Action
    = Resize Float Float
    | MouseMove Float Float


main : Program () Model Action
main =
    Browser.element
        { init = always init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Action )
init =
    ( { width = 0
      , height = 0
      , left = 0
      , top = 0
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
    )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ onMouseMove mousePosition
        , onResize (\w h -> Resize (toFloat w) (toFloat h))
        ]


mousePosition : Decoder Action
mousePosition =
    Decode.map2 MouseMove
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        MouseMove left top ->
            ( { model | left = left, top = top }, Cmd.none )



-- Meshes


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


faceMesh : Mesh Vertex
faceMesh =
    WebGL.triangles (square 100 (vec4 0 0 0 0))


sidesMesh : Mesh Vertex
sidesMesh =
    [ ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, 270 ) ]
        |> List.concatMap (rotatedSquare (vec4 0 0 1 1))
        |> WebGL.triangles


rotatedSquare : Vec4 -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedSquare color ( angleXZ, angleYZ ) =
    let
        transformMat =
            Mat4.mul
                (Mat4.makeRotate (degrees angleXZ) Vec3.j)
                (Mat4.makeRotate (degrees angleYZ) Vec3.i)

        transform vertex =
            { vertex
                | position =
                    Mat4.transform transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
    List.map transformTriangle (square 100 color)


square : Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
square scale color =
    let
        topLeft =
            Vertex (vec3 -scale scale scale) color

        topRight =
            Vertex (vec3 scale scale scale) color

        bottomLeft =
            Vertex (vec3 -scale -scale scale) color

        bottomRight =
            Vertex (vec3 scale -scale scale) color
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]



-- VIEW


view : Model -> Html Action
view { width, height, left, top } =
    let
        eye =
            vec3 (1 - 2 * left / width) -(1 - 2 * top / height) 1
                |> Vec3.normalize
                |> Vec3.scale 600

        lookAt =
            Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j

        perspective =
            Mat4.makePerspective 45 (width / height) 1 10000

        webglMat =
            Mat4.mul perspective lookAt

        css3dMat =
            lookAt

        fov =
            height / 2 / tan (degrees (45 * 0.5))
    in
    div
        [ style "position" "absolute"
        , Attributes.style "left" "0"
        , Attributes.style "top" "0"
        , style "transform-style" "preserve-3d"
        , style "perspective" (String.fromFloat fov ++ "px")
        , style "overflow" "hidden"
        , style "width" (String.fromFloat width ++ "px")
        , style "height" (String.fromFloat height ++ "px")
        ]
        [ camera
            fov
            width
            height
            css3dMat
            [ box 200 200 (Mat4.makeTranslate3 0 0 100) ]
        , WebGL.toHtml
            [ Attributes.width (round width)
            , Attributes.height (round height)
            , Attributes.style "position" "absolute"
            , Attributes.style "left" "0"
            , Attributes.style "top" "0"
            ]
            [ WebGL.entityWith [ DepthTest.default, Settings.colorMask False False False False ] vertexShader fragmentShader faceMesh { perspective = webglMat }
            , WebGL.entity vertexShader fragmentShader sidesMesh { perspective = webglMat }
            ]
        ]


cameraMatrix3d : { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float } -> String
cameraMatrix3d { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    [ m11, -m21, m31, m41, m12, -m22, m32, m42, m13, -m23, m33, m43, m14, -m24, m34, m44 ]
        |> List.map String.fromFloat
        |> List.intersperse ","
        |> List.foldr (++) ""
        |> (\s -> "matrix3d(" ++ s ++ ")")


objectMatrix3d : { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float } -> String
objectMatrix3d { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    [ m11, m21, m31, m41, -m12, -m22, -m32, -m42, m13, m23, m33, m43, m14, m24, m34, m44 ]
        |> List.map String.fromFloat
        |> List.intersperse ","
        |> List.foldr (++) ""
        |> (\s -> "matrix3d(" ++ s ++ ")")


box : Float -> Float -> Mat4 -> Html Action
box width height matrix =
    div
        [ style "position" "absolute"
        , style "background-color" "red"
        , style "transform-style" "preserve-3d"
        , style "width" (String.fromFloat width ++ "px")
        , style "height" (String.fromFloat height ++ "px")
        , style "transform" ("translate3d(-50%, -50%, 0) " ++ objectMatrix3d (Mat4.toRecord matrix))
        ]
        [ text "OH WOW, I'm a DIV in the WebGL space!" ]


camera : Float -> Float -> Float -> Mat4 -> List (Html Action) -> Html Action
camera fov width height matrix =
    div
        [ style "position" "absolute"
        , style "transform-style" "preserve-3d"
        , style "width" (String.fromFloat width ++ "px")
        , style "height" (String.fromFloat height ++ "px")
        , style "transform"
            (""
                ++ "translate3d(0,0,"
                ++ String.fromFloat fov
                ++ "px)"
                ++ cameraMatrix3d (Mat4.toRecord matrix)
                ++ "translate3d("
                ++ String.fromFloat (width / 2)
                ++ "px,"
                ++ String.fromFloat (height / 2)
                ++ "px,"
                ++ "0)"
            )
        ]



-- SHADERS


vertexShader : Shader Vertex { perspective : Mat4 } { vcolor : Vec4 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec4 color;
        varying vec4 vcolor;
        uniform mat4 perspective;

        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcolor = color;
        }

    |]


fragmentShader : Shader {} { perspective : Mat4 } { vcolor : Vec4 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec4 vcolor;

        void main () {
          gl_FragColor = vcolor;
        }

    |]
