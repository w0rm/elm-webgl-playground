module Planet3D exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (Html)
import Html.Attributes as Attributes
import Math.Matrix4 exposing (Mat4, inverseOrthonormal, makeLookAt, makePerspective, makeRotate, transpose)
import Math.Vector3 exposing (Vec3, add, dot, length, normalize, scale, vec3)
import Random exposing (Generator)
import Task
import WebGL exposing (Mesh, Shader)



{- Types -}


type Action
    = Resize Float Float
    | Animate Float


type alias Model =
    { width : Float
    , height : Float
    , angle : Float
    }



{- Program -}


main : Program () Model Action
main =
    Browser.element
        { init = always init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Action )
init =
    ( { width = 0
      , height = 0
      , angle = 0
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
    )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onResize (\w h -> Resize (toFloat w) (toFloat h))
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Resize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )

        Animate elapsed ->
            ( { model | angle = model.angle + elapsed / 1000 }
            , Cmd.none
            )



-- View


view : Model -> Html Action
view { width, height, angle } =
    WebGL.toHtml
        [ Attributes.width (round width)
        , Attributes.height (round height)
        , Attributes.style "display" "block"
        , Attributes.style "position" "absolute"
        , Attributes.style "left" "0"
        , Attributes.style "top" "0"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            planet
            { rotation = makeRotate angle (vec3 0 1 0)
            , perspective = makePerspective 45 (width / height) 0.01 100
            , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
            , normal = transpose (inverseOrthonormal (makeRotate angle (vec3 0 1 0)))
            }
        ]



-- Mesh and shaders


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type alias Uniform =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , normal : Mat4
    }


type alias Varying =
    { vcolor : Vec3
    , vlighting : Vec3
    }



{- Planet mesh -}


planet : Mesh Vertex
planet =
    let
        scaleV v =
            if length v < 1 then
                normalize v |> scale 20

            else
                scale 20 v

        vec3ToVertex ( a, b, c ) =
            ( Vertex (scaleV a) (color a)
            , Vertex (scaleV b) (color b)
            , Vertex (scaleV c) (color c)
            )

        triangles =
            divideSphere 5 octahedron

        randomTriangles =
            Random.step (surface 80 triangles) (Random.initialSeed 1) |> Tuple.first
    in
    WebGL.triangles (List.map vec3ToVertex randomTriangles)



{- Randomize the surface of the sphere -}


surface : Int -> List ( Vec3, Vec3, Vec3 ) -> Generator (List ( Vec3, Vec3, Vec3 ))
surface step triangles =
    let
        scaleBy n m v =
            if dot n v > 0 then
                scale (1 + m / 50) v

            else
                scale (1 - m / 50) v
    in
    Random.map2
        (\n m -> \( a, b, c ) -> ( scaleBy n m a, scaleBy n m b, scaleBy n m c ))
        randomVec3
        (Random.float -1 1)
        |> Random.andThen
            (\f ->
                if step <= 0 then
                    succeed triangles

                else
                    surface (step - 1) (List.map f triangles)
            )



{- Generate a random vertex -}


randomVec3 : Generator Vec3
randomVec3 =
    Random.map3 vec3 (Random.float -1 1) (Random.float -1 1) (Random.float -1 1)
        |> Random.andThen
            (\v ->
                if length v < 1 then
                    succeed v

                else
                    randomVec3
            )



{- Generator that always succeeds with the same value -}


succeed : a -> Generator a
succeed v =
    Random.map (always v) (Random.int 0 0)



{- Color the vertex based on its length -}


color : Vec3 -> Vec3
color v =
    let
        len =
            length v

        c =
            len * 0.9
    in
    if len <= 1 then
        vec3 0 0 c

    else if len < 1.1 then
        vec3 0 (c / 1.1) 0

    else if len < 1.17 then
        vec3 (c / 2) (c / 5) (c / 10)

    else
        vec3 c c c



{- Recursively divide an octahedron to turn it into a sphere -}


divideSphere : Int -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divideSphere step triangles =
    if step <= 0 then
        triangles

    else
        divideSphere (step - 1) (List.concatMap divide triangles)



{- 1
       / \
    b /___\ c
     /\   /\
    /__\ /__\
   0    a    2
-}


divide : ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divide ( v0, v1, v2 ) =
    let
        a =
            add v0 v2 |> normalize

        b =
            add v0 v1 |> normalize

        c =
            add v1 v2 |> normalize
    in
    [ ( v0, b, a ), ( b, v1, c ), ( a, b, c ), ( a, c, v2 ) ]



{- Octahedron mesh -}


octahedron : List ( Vec3, Vec3, Vec3 )
octahedron =
    [ ( vec3 1 0 0, vec3 0 0 1, vec3 0 1 0 )
    , ( vec3 0 1 0, vec3 0 0 1, vec3 -1 0 0 )
    , ( vec3 -1 0 0, vec3 0 0 1, vec3 0 -1 0 )
    , ( vec3 0 -1 0, vec3 0 0 1, vec3 1 0 0 )
    , ( vec3 1 0 0, vec3 0 1 0, vec3 0 0 -1 )
    , ( vec3 0 1 0, vec3 -1 0 0, vec3 0 0 -1 )
    , ( vec3 -1 0 0, vec3 0 -1 0, vec3 0 0 -1 )
    , ( vec3 0 -1 0, vec3 1 0 0, vec3 0 0 -1 )
    ]


vertexShader : Shader Vertex Uniform Varying
vertexShader =
    [glsl|
  attribute vec3 position;
  attribute vec3 color;
  uniform mat4 perspective;
  uniform mat4 camera;
  uniform mat4 rotation;
  uniform mat4 normal;
  varying vec3 vcolor;
  varying vec3 vlighting;
  void main () {
    gl_Position = perspective * camera * rotation * vec4(0.05 * position, 1.0);
    vcolor = color;
    highp vec3 ambientLight = vec3(0.6, 0.6, 0.6);
    highp vec3 directionalLightColor = vec3(0.5, 0.5, 0.75);
    highp vec3 directionalVector = vec3(0.85, 0.8, 0.75);
    highp vec4 transformedNormal = normal * vec4(0.05 * position, 1.0);
    highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
    vlighting = ambientLight + (directionalLightColor * directional);
  }
|]


fragmentShader : Shader {} Uniform Varying
fragmentShader =
    [glsl|
  precision mediump float;
  varying vec3 vcolor;
  varying vec3 vlighting;
  void main () {
    gl_FragColor = vec4(vcolor.rgb * vlighting, 1.0);
  }
|]
