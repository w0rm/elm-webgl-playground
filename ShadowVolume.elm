module ShadowVolume exposing (main)

{-
   Rendering shadows using the Shadow Volume technique:
   https://en.wikipedia.org/wiki/Shadow_volume
-}

import Array exposing (Array)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest


type alias Model =
    { width : Float
    , height : Float
    , elapsedTime : Float
    }


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias Face =
    { vertexIndices : List Int
    , normal : Vec3
    }


type alias Edge =
    { start : Vec3
    , end : Vec3
    , leftNormal : Vec3
    , rightNormal : Vec3
    }


type alias CollectedNormals =
    { leftNormals : Dict ( Int, Int ) Vec3
    , rightNormals : Dict ( Int, Int ) Vec3
    }


collectEdgeNormals : Face -> CollectedNormals -> CollectedNormals
collectEdgeNormals face collectedNormals =
    case face.vertexIndices of
        first :: _ ->
            collectEdgeNormalsHelp face.normal first face.vertexIndices collectedNormals

        [] ->
            collectedNormals


collectEdgeNormalsHelp : Vec3 -> Int -> List Int -> CollectedNormals -> CollectedNormals
collectEdgeNormalsHelp normal firstIndex remainingVertexIndices collectedNormals =
    case remainingVertexIndices of
        index1 :: index2 :: rest ->
            collectEdgeNormalsHelp normal firstIndex (index2 :: rest) (addNormal normal index1 index2 collectedNormals)

        [ lastIndex ] ->
            addNormal normal lastIndex firstIndex collectedNormals

        [] ->
            -- Should never get here
            collectedNormals


addNormal : Vec3 -> Int -> Int -> CollectedNormals -> CollectedNormals
addNormal normal startIndex endIndex collectedNormals =
    if startIndex < endIndex then
        { collectedNormals
            | leftNormals = Dict.insert ( startIndex, endIndex ) normal collectedNormals.leftNormals
        }

    else
        { collectedNormals
            | rightNormals = Dict.insert ( endIndex, startIndex ) normal collectedNormals.rightNormals
        }


meshEdges : Array Vec3 -> List Face -> List Edge
meshEdges vertices faces =
    let
        { leftNormals, rightNormals } =
            List.foldl
                collectEdgeNormals
                { leftNormals = Dict.empty, rightNormals = Dict.empty }
                faces
    in
    Dict.merge
        (\_ _ -> identity)
        (\( startIndex, endIndex ) leftNormal rightNormal currentEdges ->
            case Array.get startIndex vertices of
                Just start ->
                    case Array.get endIndex vertices of
                        Just end ->
                            { start = start
                            , end = end
                            , leftNormal = leftNormal
                            , rightNormal = rightNormal
                            }
                                :: currentEdges

                        Nothing ->
                            currentEdges

                Nothing ->
                    currentEdges
        )
        (\_ _ -> identity)
        leftNormals
        rightNormals
        []


boxVertices : { x : Float, y : Float, z : Float } -> Array Vec3
boxVertices dimensions =
    let
        x =
            dimensions.x / 2

        y =
            dimensions.y / 2

        z =
            dimensions.z / 2
    in
    Array.fromList
        [ vec3 -x -y -z
        , vec3 x -y -z
        , vec3 x y -z
        , vec3 -x y -z
        , vec3 -x -y z
        , vec3 x -y z
        , vec3 x y z
        , vec3 -x y z
        ]


boxFaces : List Face
boxFaces =
    [ { vertexIndices = [ 3, 2, 1, 0 ], normal = vec3 0 0 -1 }
    , { vertexIndices = [ 4, 5, 6, 7 ], normal = vec3 0 0 1 }
    , { vertexIndices = [ 5, 4, 0, 1 ], normal = vec3 0 -1 0 }
    , { vertexIndices = [ 2, 3, 7, 6 ], normal = vec3 0 1 0 }
    , { vertexIndices = [ 0, 4, 7, 3 ], normal = vec3 -1 0 0 }
    , { vertexIndices = [ 1, 2, 6, 5 ], normal = vec3 1 0 0 }
    ]


planeMesh : Mesh Vertex
planeMesh =
    let
        normal =
            vec3 0 0 1

        size =
            10
    in
    WebGL.triangles
        [ ( { position = vec3 -size -size 0, normal = normal }
          , { position = vec3 size -size 0, normal = normal }
          , { position = vec3 size size 0, normal = normal }
          )
        , ( { position = vec3 size size 0, normal = normal }
          , { position = vec3 -size size 0, normal = normal }
          , { position = vec3 -size -size 0, normal = normal }
          )
        ]


boxMeshes : { x : Float, y : Float, z : Float } -> { mesh : Mesh Vertex, shadowVolume : Mesh Vertex }
boxMeshes dimensions =
    let
        vertices =
            boxVertices dimensions

        meshTriangles =
            List.foldl
                (\{ vertexIndices, normal } triangles ->
                    case List.filterMap (\i -> Array.get i vertices) vertexIndices of
                        [ p0, p1, p2, p3 ] ->
                            ( { position = p0, normal = normal }, { position = p1, normal = normal }, { position = p2, normal = normal } )
                                :: ( { position = p2, normal = normal }, { position = p3, normal = normal }, { position = p0, normal = normal } )
                                :: triangles

                        _ ->
                            triangles
                )
                []
                boxFaces

        shadowVolumeTriangles =
            List.foldl
                (\{ start, end, leftNormal, rightNormal } triangles ->
                    ( { position = start, normal = rightNormal }, { position = end, normal = rightNormal }, { position = end, normal = leftNormal } )
                        :: ( { position = end, normal = leftNormal }, { position = start, normal = leftNormal }, { position = start, normal = rightNormal } )
                        :: triangles
                )
                []
                (meshEdges vertices boxFaces)
    in
    { mesh = WebGL.triangles meshTriangles
    , shadowVolume = WebGL.triangles shadowVolumeTriangles
    }


type Msg
    = Resize Float Float
    | Tick Float


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { width = 0, height = 0, elapsedTime = 0 }
                , Task.perform
                    (\{ viewport } ->
                        Resize viewport.width viewport.height
                    )
                    getViewport
                )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        Tick milliseconds ->
            ( { model | elapsedTime = model.elapsedTime + milliseconds / 1000 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onAnimationFrameDelta Tick
        ]



-- VIEW


cubeMeshes : { mesh : Mesh Vertex, shadowVolume : Mesh Vertex }
cubeMeshes =
    boxMeshes { x = 2, y = 2, z = 2 }


dotMesh : Mesh Vertex
dotMesh =
    (boxMeshes { x = 0.2, y = 0.2, z = 0.2 }).mesh


view : Model -> Html Msg
view { width, height, elapsedTime } =
    let
        lightPosition =
            vec3 -2 -2 4

        uniforms transform =
            { perspective = Mat4.makePerspective 45 (width / height) 1 1000
            , camera = Mat4.makeLookAt (vec3 0 -8 8) (vec3 0 0 0) Vec3.k
            , lightPosition = lightPosition
            , transform = transform
            }

        firstBoxPosition =
            Mat4.makeRotate (degrees 30 * elapsedTime) (vec3 1 1 1)

        planePosition =
            Mat4.makeTranslate3 0 0 -5
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.stencil 0
        ]
        [ Attributes.width (round width)
        , Attributes.height (round height)
        , Attributes.style "position" "absolute"
        , Attributes.style "left" "0"
        , Attributes.style "top" "0"
        ]
        [ -- Ambient:
          WebGL.entityWith
            [ DepthTest.default, Settings.cullFace Settings.back ]
            vertexShader
            ambientFragmentShader
            cubeMeshes.mesh
            (uniforms firstBoxPosition)
        , WebGL.entityWith
            [ DepthTest.default, Settings.cullFace Settings.back ]
            vertexShader
            ambientFragmentShader
            planeMesh
            (uniforms planePosition)

        -- Shadow volume:
        , WebGL.entityWith
            [ DepthTest.less { write = False, near = 0, far = 1 }
            , Settings.colorMask False False False False
            , StencilTest.testSeparate
                { ref = 1
                , mask = 0xFF
                , writeMask = 0xFF
                }
                { test = StencilTest.always
                , fail = StencilTest.keep
                , zfail = StencilTest.keep
                , zpass = StencilTest.incrementWrap
                }
                { test = StencilTest.always
                , fail = StencilTest.keep
                , zfail = StencilTest.keep
                , zpass = StencilTest.decrementWrap
                }
            ]
            shadowVolumeVertexShader
            shadowVolumeFragmentShader
            cubeMeshes.shadowVolume
            (uniforms firstBoxPosition)

        -- Diffuse:
        , WebGL.entityWith
            [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
            , StencilTest.test
                { ref = 0
                , mask = 0xFF
                , test = StencilTest.equal
                , fail = StencilTest.keep
                , zfail = StencilTest.keep
                , zpass = StencilTest.keep
                , writeMask = 0x00
                }
            , Settings.cullFace Settings.back
            , Blend.add Blend.one Blend.one
            ]
            vertexShader
            diffuseFragmentShader
            cubeMeshes.mesh
            (uniforms firstBoxPosition)
        , WebGL.entityWith
            [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
            , StencilTest.test
                { ref = 0
                , mask = 0xFF
                , test = StencilTest.equal
                , fail = StencilTest.keep
                , zfail = StencilTest.keep
                , zpass = StencilTest.keep
                , writeMask = 0x00
                }
            , Settings.cullFace Settings.back
            , Blend.add Blend.one Blend.one
            ]
            vertexShader
            diffuseFragmentShader
            planeMesh
            (uniforms planePosition)
        , -- Light
          WebGL.entityWith
            []
            vertexShader
            ambientFragmentShader
            dotMesh
            (uniforms (Mat4.makeTranslate lightPosition))
        ]



-- SHADERS


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , transform : Mat4
    , lightPosition : Vec3
    }


type alias Varyings =
    { vposition : Vec3
    , vnormal : Vec3
    }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 normal;
        varying vec3 vposition;
        varying vec3 vnormal;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 transform;
        
        void main () {
          vec4 transformedPosition = transform * vec4(position, 1.0);
          vec4 transformedNormal = transform * vec4(normal, 0.0);
          gl_Position = perspective * camera * transformedPosition;
          vposition = transformedPosition.xyz;
          vnormal = transformedNormal.xyz;
        }

    |]


ambientFragmentShader : Shader {} Uniforms Varyings
ambientFragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vposition;
        varying vec3 vnormal;
        uniform vec3 lightPosition;

        void main () {    
          float ambientLight = 0.4;
          float intensity = ambientLight;
          gl_FragColor = vec4(intensity, 0.0, 0.0, 1.0);
        }

    |]


diffuseFragmentShader : Shader {} Uniforms Varyings
diffuseFragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vposition;
        varying vec3 vnormal;
        uniform vec3 lightPosition;

        void main () {    
          float directionalLight = 0.6;
          float directional = max(dot(normalize(lightPosition - vposition), vnormal), 0.0);
          float intensity = directional * directionalLight;
          gl_FragColor = vec4(intensity, 0.0, 0.0, 1.0);
        }

    |]


shadowVolumeVertexShader : Shader Vertex Uniforms Varyings
shadowVolumeVertexShader =
    [glsl|

        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        varying vec3 vposition;
        varying vec3 vnormal;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform vec3 lightPosition;
        uniform mat4 transform;
        
        void main () {
          vec3 transformedPosition = (transform * vec4(position, 1.0)).xyz;
          vec3 transformedNormal = (transform * vec4(normal, 0.0)).xyz;

          vec3 directionToLight = normalize(lightPosition - transformedPosition);
          vec3 offset = vec3(0.0, 0.0, 0.0);
          if (dot(directionToLight, transformedNormal) > 0.0) {
            offset = vec3(0.0, 0.0, 0.0);
          } else {
            offset = -100.0 * directionToLight;
          }
          vec3 offsetPosition = transformedPosition + offset;
          gl_Position = perspective * camera * vec4(offsetPosition, 1.0);
          vposition = offsetPosition;
          vnormal = transformedNormal;
        }

    |]


shadowVolumeFragmentShader : Shader {} Uniforms Varyings
shadowVolumeFragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vposition;
        varying vec3 vnormal;
        uniform vec3 lightPosition;

        void main () {    
          float ambientLight = 0.4;
          float directionalLight = 0.6;
          float directional = max(dot(normalize(lightPosition - vposition), vnormal), 0.0);
          float intensity = ambientLight + directional * directionalLight;

          gl_FragColor = vec4(intensity, intensity, intensity, 1.0);
        }

    |]
