module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4, translate3)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Mouse
import Native.Mat4
import Task exposing (Task)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest
import Window


type alias Model =
    { size : Window.Size
    , position : Mouse.Position
    }


type Action
    = Resize Window.Size
    | MouseMove Mouse.Position


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Action )
init =
    ( { size = Window.Size 0 0
      , position = Mouse.Position 0 0
      }
    , Task.perform Resize Window.size
    )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.moves MouseMove
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Resize size ->
            ( { model | size = size }, Cmd.none )

        MouseMove position ->
            ( { model | position = position }, Cmd.none )



-- Meshes


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


faceMesh : Mesh Vertex
faceMesh =
    WebGL.triangles (square (vec4 0 0 0 0))


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
        List.map transformTriangle (square color)


square : Vec4 -> List ( Vertex, Vertex, Vertex )
square color =
    let
        topLeft =
            Vertex (vec3 -1 1 1) color

        topRight =
            Vertex (vec3 1 1 1) color

        bottomLeft =
            Vertex (vec3 -1 -1 1) color

        bottomRight =
            Vertex (vec3 1 -1 1) color
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]


type Tuple16
    = Tuple16 Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float


mat4toTuple : Mat4 -> Tuple16
mat4toTuple =
    Native.Mat4.toTuple



-- VIEW


view : Model -> Html Action
view { size, position } =
    let
        eye =
            vec3 (0.5 - toFloat position.x / toFloat size.width) -(0.5 - toFloat position.y / toFloat size.height) 1
                |> Vec3.normalize
                |> Vec3.scale 6

        lookAt =
            Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j

        perspective =
            Mat4.makePerspective 100 (toFloat size.width / toFloat size.height) 0.01 100

        css3dMat =
            lookAt

        webglMat =
            Mat4.mul perspective lookAt
    in
        div
            [ style
                [ ( "position", "relative" )
                , ( "transform-style", "preserve-3d" )
                , ( "overflow", "hidden" )
                , ( "width", toString size.width ++ "px" )
                , ( "height", toString size.height ++ "px" )
                ]
            ]
            [ camera
                100
                size
                css3dMat
                [ box 500 500 Mat4.identity ]
            , WebGL.toHtml
                [ width size.width
                , height size.height
                , style
                    [ ( "position", "absolute" )
                    ]
                ]
                [ WebGL.entityWith [ DepthTest.default, Settings.colorMask False False False False ] vertexShader fragmentShader faceMesh { perspective = webglMat }
                , WebGL.entity vertexShader fragmentShader sidesMesh { perspective = webglMat }
                ]
            ]


box : Float -> Float -> Mat4 -> Html Action
box width height matrix =
    let
        matrix3d (Tuple16 a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4) =
            [ a1, b1, c1, d1, -a2, -b2, -c2, -d2, a3, b3, c3, d3, a4, b4, c4, d4 ]
                |> List.map toString
                |> List.intersperse ","
                |> List.foldl (++) ""
                |> (\s -> "matrix3d(" ++ s ++ ")")
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "background-color", "red" )
                , ( "width", toString width ++ "px" )
                , ( "height", toString height ++ "px" )
                , ( "transform"
                  , ""
                        ++ "translate3d(-50%, -50%, 0)"
                        ++ matrix3d (mat4toTuple matrix)
                  )
                ]
            ]
            []


camera : Int -> Window.Size -> Mat4 -> List (Html Action) -> Html Action
camera fov { width, height } matrix =
    let
        matrix3d (Tuple16 a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4) =
            [ a1, -b1, c1, d1, a2, -b2, c2, d2, a3, -b3, c3, d3, a4, -b4, c4, d4 ]
                |> List.map toString
                |> List.intersperse ","
                |> List.foldl (++) ""
                |> (\s -> "matrix3d(" ++ s ++ ")")
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "transform-style", "preserve-3d" )
                , ( "perspective", toString fov ++ "px" )
                , ( "width", toString width ++ "px" )
                , ( "height", toString height ++ "px" )
                , ( "transform"
                  , ""
                        ++ "translate3d(0,0,"
                        ++ toString fov
                        ++ "px)"
                        ++ matrix3d (mat4toTuple matrix)
                        ++ "translate3d("
                        ++ toString (width // 2)
                        ++ "px,"
                        ++ toString (height // 2)
                        ++ "px,"
                        ++ "0)"
                  )
                ]
            ]



-- SHADERS


type alias Uniforms =
    { perspective : Mat4
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec4 }
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


fragmentShader : Shader {} Uniforms { vcolor : Vec4 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec4 vcolor;

        void main () {
          gl_FragColor = vcolor;
        }

    |]
