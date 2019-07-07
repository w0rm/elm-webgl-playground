module OsloElmDay.Shader exposing
    ( Attributes
    , Uniforms
    , fragment
    , vertex
    )

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    , color : Vec3
    , lightDirection : Vec3
    }


vertex : Shader Attributes Uniforms { vlighting : Float }
vertex =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        uniform vec3 lightDirection;
        varying float vlighting;
        void main () {
          // set both to 0.5 to enable flat lighting
          float ambientLight = 1.0;
          float directionalLight = 0.0;
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          vec4 transformedNormal = normalize(transform * vec4(normal, 0.0));
          float directional = max(dot(transformedNormal.xyz, lightDirection), 0.0);
          vlighting = ambientLight + directional * directionalLight;
        }
    |]


fragment : Shader {} Uniforms { vlighting : Float }
fragment =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying float vlighting;
        void main () {
          gl_FragColor = vec4(vlighting * color, 1.0);
        }
    |]
