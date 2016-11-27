module Tangram.Shader exposing (Attribute, Varying, Uniform, vertex, fragment)

import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL


type alias Attribute =
    { position : Vec3
    , normal : Vec3
    }


type alias Varying =
    { vlighting : Float
    }


type alias Uniform =
    { color : Vec3
    , rotate : Mat4
    , scale : Mat4
    , translate : Mat4
    , camera : Mat4
    }


vertex : WebGL.Shader Attribute Uniform Varying
vertex =
    [glsl|
    attribute vec3 position;
    attribute vec3 normal;
    varying highp float vlighting;
    uniform mat4 rotate;
    uniform mat4 scale;
    uniform mat4 translate;
    uniform mat4 camera;
    highp float ambientLight = 0.4;
    highp float directionalLight = 0.6;
    highp vec3 directionalVector = vec3(0, 0, 1);
    void main () {
        gl_Position = camera * translate * rotate * scale * vec4(position, 1.0);
        highp vec4 transformedNormal = normalize(rotate * vec4(normal, 1.0));
        highp float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
        vlighting = ambientLight + directional * directionalLight;
    }
|]


fragment : WebGL.Shader {} Uniform Varying
fragment =
    [glsl|
    precision mediump float;
    varying highp float vlighting;
    uniform vec3 color;
    void main () {
        gl_FragColor = vec4(color.rgb * vlighting, 1.0);
    }
|]
