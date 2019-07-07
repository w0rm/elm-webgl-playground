module ShadowVolume exposing (main, meshEdges, boxVertices, boxFaces)

{-
   Rendering shadows using the Shadow Volume technique:
   https://en.wikipedia.org/wiki/Shadow_volume
-}

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import Dict exposing (Dict)
import WebGL exposing (Mesh, Shader)
import Array exposing (Array)
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest


type alias Model =
    { width : Float
    , height : Float 
    }


type alias Vertex =
    { position : Vec3
    , normal: Vec3
    }


type alias Face =
    { vertexIndices : List Int
    , normal : Vec3
    }

type alias Edge =
    { start : Vec3
    , end : Vec3
    , leftNormal : Vec3
    , rightNormal: Vec3
    }

type alias CollectedNormals =
    { leftNormals : Dict ( Int, Int ) Vec3
    , rightNormals : Dict ( Int, Int ) Vec3
    }


collectEdgeNormals : Face -> CollectedNormals -> CollectedNormals
collectEdgeNormals  face collectedNormals =
    case face.vertexIndices of
        first :: _ ->
            collectEdgeNormalsHelp  face.normal first face.vertexIndices collectedNormals

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
        { collectedNormals |
            leftNormals = Dict.insert ( startIndex, endIndex ) normal collectedNormals.leftNormals
        }

    else
        { collectedNormals |
            rightNormals = Dict.insert ( endIndex, startIndex ) normal collectedNormals.rightNormals
        }

meshEdges : Array Vec3 -> List Face -> List Edge
meshEdges vertices faces =
    let
        { leftNormals, rightNormals } = List.foldl 
            collectEdgeNormals 
            {leftNormals = Dict.empty, rightNormals = Dict.empty}
            faces
         
    in
    Dict.merge
        (\_ _ -> identity)
        (\(startIndex, endIndex) leftNormal rightNormal currentEdges ->  
            case Array.get startIndex vertices of
                Just start ->
                    case Array.get endIndex vertices of
                        Just end ->
                            { start = start
                            , end = end 
                            , leftNormal = leftNormal
                            , rightNormal = rightNormal 
                            } :: currentEdges
                        Nothing ->
                            currentEdges
                Nothing ->
                    currentEdges
        )
        (\_ _ -> identity)
        leftNormals
        rightNormals
        []


boxVertices : { x: Float, y: Float, z: Float } -> Array Vec3
boxVertices dimensions= 
    let 
        x = dimensions.x / 2
        y = dimensions.y / 2
        z = dimensions.z / 2
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


boxMeshes : { x: Float, y: Float, z: Float } -> { mesh : Mesh Vertex, shadowVolume : Mesh Vertex }
boxMeshes dimensions =
    let
        vertices = boxVertices dimensions

        meshTriangles = 
            List.foldl 
                (\{vertexIndices, normal} triangles ->
                    case List.filterMap (\i -> Array.get i vertices) vertexIndices of 
                        [p0, p1, p2, p3] ->
                            ({position = p0, normal = normal}, {position = p1, normal = normal}, {position = p2, normal = normal}) ::
                            ({position = p2, normal = normal}, {position = p3, normal = normal}, {position = p0, normal = normal}) ::
                                triangles

                        _ -> 
                            triangles
                
                )
                []
                boxFaces

        shadowVolumeTriangles =
            List.foldl 
                (\{ start, end, leftNormal, rightNormal } triangles -> 
                    ({ position = start, normal = rightNormal }, { position = end, normal = rightNormal }, { position = end, normal = leftNormal }) ::
                    ({ position = end, normal = leftNormal }, { position = start, normal = leftNormal }, { position = start, normal = rightNormal }) ::
                        triangles
                )
                []
                (meshEdges vertices boxFaces)

    in
        { mesh = WebGL.triangles meshTriangles
        , shadowVolume = WebGL.triangles shadowVolumeTriangles
        }


type Msg
    = Resize Float Float


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { width = 0, height = 0 }
                , Task.perform
                    (\{ viewport } ->
                        Resize viewport.width viewport.height
                    )
                    getViewport
                )
        , subscriptions = \_ -> onResize (\w h -> Resize (toFloat w) (toFloat h))
        , update =
            \(Resize width height) model ->
                ( { model | width = width, height = height }
                , Cmd.none
                )
        , view = view
        }


-- VIEW

cubeMeshes : { mesh : Mesh Vertex, shadowVolume : Mesh Vertex }
cubeMeshes =
    boxMeshes { x = 2, y = 2, z = 2 }


view : Model -> Html Msg
view { width, height } =
    WebGL.toHtml
        [ Attributes.width (round width)
        , Attributes.height (round height)
        , Attributes.style "position" "absolute"
        , Attributes.style "left" "0"
        , Attributes.style "top" "0"
        ]
        [ WebGL.entityWith 
            [ DepthTest.default, Settings.cullFace Settings.back ]
            vertexShader
            fragmentShader
            cubeMeshes.mesh
            { perspective = Mat4.makePerspective 45 (width / height) 1 1000
            , camera = Mat4.makeLookAt (vec3 0 -4 4) (vec3 0 0 0) Vec3.k
            , lightPosition = vec3 -4 -4 8
            , transform = Mat4.identity
            }
        , WebGL.entityWith 
            [ DepthTest.default, Settings.cullFace Settings.back ]
            shadowVolumeVertexShader
            shadowVolumeFragmentShader
            cubeMeshes.shadowVolume
            { perspective = Mat4.makePerspective 45 (width / height) 1 1000
            , camera = Mat4.makeLookAt (vec3 0 -4 4) (vec3 0 0 0) Vec3.k
            , lightPosition = vec3 -4 -4 8
            , transform = Mat4.identity
            }
        , WebGL.entityWith 
            [ DepthTest.default, Settings.cullFace Settings.back ]
            vertexShader
            fragmentShader
            cubeMeshes.mesh
            { perspective = Mat4.makePerspective 45 (width / height) 1 1000
            , camera = Mat4.makeLookAt (vec3 0 -4 4) (vec3 0 0 0) Vec3.k
            , lightPosition = vec3 -4 -4 8
            , transform = Mat4.makeTranslate3 0.5 0.5 -0.5
            }
        ]



-- SHADERS


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , transform : Mat4
    , lightPosition: Vec3
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


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
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
          if (dot(directionToLight, normal) > 0.0) {
            offset = vec3(0.0, 0.0, 0.0);
          } else {
            offset = -1.0 * directionToLight;
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


