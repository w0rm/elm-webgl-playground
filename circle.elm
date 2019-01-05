module Circle exposing (main)

{-
   Draw a circle in the fragment shader.

   This example is based on:
   http://www.geeks3d.com/20130705/shader-library-circle-disc-fake-sphere-in-glsl-opengl-glslhacker/
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


main : Program () Float Float
main =
    Browser.element
        { init = always ( 0, Cmd.none )
        , view = view
        , subscriptions = \model -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { transform =
                Mat4.makeRotate (t / 1000) (vec3 0 0 1)
            }
        ]



-- Mesh


type alias Vertex =
    { position : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 -1 1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 -1 -1 0)
          )
        , ( Vertex (vec3 -1 -1 0)
          , Vertex (vec3 1 1 0)
          , Vertex (vec3 1 -1 0)
          )
        ]



-- Shaders


type alias Uniforms =
    { transform : Mat4 }


type alias Varyings =
    { vposition : Vec2
    }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

        attribute vec3 position;
        uniform mat4 transform;
        varying vec2 vposition;

        void main () {
            gl_Position = transform * vec4(position, 1.0);
            vposition = position.xy;
        }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec2 vposition;

        // Taken from https://www.shadertoy.com/view/MsS3Wc
        vec3 hsv2rgb(in vec3 hsv) {
            vec3 rgb = clamp(
                abs(mod(hsv.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0,
                0.0,
                1.0
            );
            return hsv.z * mix(vec3(1.0), rgb, hsv.y);
        }

        void main () {
            float border = 0.01;
            float dist = sqrt(dot(vposition, vposition));
            float t = smoothstep(1.0, 1.0 - border, dist);
            vec4 transparentColor = vec4(0.0, 0.0, 0.0, 0.0);
            float a = atan(vposition.x, vposition.y) / (2.0 * 3.1415926);
            vec4 discColor = vec4(hsv2rgb(vec3(a, 1.0, 1.0)), 1.0);
            if (dist <= 1.0) {
                gl_FragColor = mix(transparentColor, discColor, t);
            } else {
                discard;
            }
        }
    |]
