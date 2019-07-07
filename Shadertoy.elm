{- The shader code is taken from https://www.shadertoy.com/view/Ms2SD1 -}


module Shadertoy exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onMouseMove, onMouseUp, onResize)
import Html exposing (Html, a, div, text)
import Html.Attributes as Attributes exposing (href, style)
import Json.Decode as Decode exposing (Decoder)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Task
import WebGL exposing (Mesh, Shader)


type Msg
    = Diff Float
    | MouseMove Float Float
    | Press Bool
    | Resize Float Float


type alias Model =
    { width : Float
    , height : Float
    , left : Float
    , top : Float
    , pressed : Bool
    , time : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( Model 0 0 0 0 False 0
                , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
                )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff time ->
            ( { model | time = model.time + time }
            , Cmd.none
            )

        MouseMove left top ->
            ( { model | left = left, top = top }
            , Cmd.none
            )

        Press pressed ->
            ( { model | pressed = pressed }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions { pressed } =
    Sub.batch
        [ onAnimationFrameDelta Diff
        , onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onMouseDown (Decode.succeed (Press True))
        , onMouseUp (Decode.succeed (Press False))
        , if pressed then
            onMouseMove mousePosition

          else
            Sub.none
        ]


mousePosition : Decoder Msg
mousePosition =
    Decode.map2 MouseMove
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


view : Model -> Html msg
view { width, height, left, top, time } =
    div []
        [ WebGL.toHtml
            [ Attributes.width (round width)
            , Attributes.height (round height)
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "display" "block"
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { iResolution = vec3 width height 0
                , iGlobalTime = time / 1000
                , iMouse = vec4 left top 0 0
                }
            ]
        , a
            [ href "https://www.shadertoy.com/view/Ms2SD1"
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "color" "#fff"
            , style "font" "300 20px sans-serif"
            , style "background-color" "#222"
            , style "text-decoration" "none"
            ]
            [ text "The shader code is taken from: https://www.shadertoy.com/view/Ms2SD1" ]
        ]



-- Mesh


mesh : Mesh { position : Vec3 }
mesh =
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]



-- Shaders


type alias Uniforms =
    { iResolution : Vec3
    , iGlobalTime : Float
    , iMouse : Vec4
    }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|

        precision mediump float;

        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;

        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }

    |]


fragmentShader : Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;

        varying vec2      vFragCoord;
        uniform vec3      iResolution;           // viewport resolution (in pixels)
        uniform float     iGlobalTime;           // shader playback time (in seconds)
        uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click

        /*
         * "Seascape" by Alexander Alekseev aka TDM - 2014
         * License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
         * Contact: tdmaav@gmail.com
         */

        const int NUM_STEPS = 8;
        const float PI	 	= 3.1415;
        const float EPSILON	= 1e-3;

        // sea
        const int ITER_GEOMETRY = 3;
        const int ITER_FRAGMENT = 5;
        const float SEA_HEIGHT = 0.6;
        const float SEA_CHOPPY = 4.0;
        const float SEA_SPEED = 0.8;
        const float SEA_FREQ = 0.16;
        const vec3 SEA_BASE = vec3(0.1,0.19,0.22);
        const vec3 SEA_WATER_COLOR = vec3(0.8,0.9,0.6);
        const mat2 octave_m = mat2(1.6,1.2,-1.2,1.6);

        // math
        mat3 fromEuler(vec3 ang) {
        	  vec2 a1 = vec2(sin(ang.x),cos(ang.x));
            vec2 a2 = vec2(sin(ang.y),cos(ang.y));
            vec2 a3 = vec2(sin(ang.z),cos(ang.z));
            mat3 m;
            m[0] = vec3(a1.y*a3.y+a1.x*a2.x*a3.x,a1.y*a2.x*a3.x+a3.y*a1.x,-a2.y*a3.x);
        	  m[1] = vec3(-a2.y*a1.x,a1.y*a2.y,a2.x);
        	  m[2] = vec3(a3.y*a1.x*a2.x+a1.y*a3.x,a1.x*a3.x-a1.y*a3.y*a2.x,a2.y*a3.y);
        	  return m;
        }
        float hash( vec2 p ) {
          	float h = dot(p,vec2(127.1,311.7));
            return fract(sin(h)*43758.5453123);
        }
        float noise( in vec2 p ) {
            vec2 i = floor( p );
            vec2 f = fract( p );
          	vec2 u = f*f*(3.0-2.0*f);
            return -1.0 + 2.0 * mix(
              mix(
                hash(i + vec2(0.0,0.0)),
                hash(i + vec2(1.0,0.0)),
                u.x
              ),
              mix(
                hash(i + vec2(0.0,1.0)),
                hash(i + vec2(1.0,1.0)),
                u.x
              ),
              u.y
            );
        }

        // lighting
        float diffuse(vec3 n,vec3 l,float p) {
            return pow(dot(n,l) * 0.4 + 0.6,p);
        }
        float specular(vec3 n,vec3 l,vec3 e,float s) {
            float nrm = (s + 8.0) / (3.1415 * 8.0);
            return pow(max(dot(reflect(e,n),l),0.0),s) * nrm;
        }

        // sky
        vec3 getSkyColor(vec3 e) {
            e.y = max(e.y,0.0);
            return vec3(pow(1.0-e.y,2.0), 1.0-e.y, 0.6+(1.0-e.y)*0.4);
        }

        // sea
        float sea_octave(vec2 uv, float choppy) {
            uv += noise(uv);
            vec2 wv = 1.0-abs(sin(uv));
            vec2 swv = abs(cos(uv));
            wv = mix(wv,swv,wv);
            return pow(1.0-pow(wv.x * wv.y,0.65),choppy);
        }

        float map(vec3 p) {
            float SEA_TIME = 1.0 + iGlobalTime * SEA_SPEED;
            float freq = SEA_FREQ;
            float amp = SEA_HEIGHT;
            float choppy = SEA_CHOPPY;
            vec2 uv = p.xz; uv.x *= 0.75;

            float d, h = 0.0;
            for(int i = 0; i < ITER_GEOMETRY; i++) {
              	d = sea_octave((uv+SEA_TIME)*freq,choppy);
              	d += sea_octave((uv-SEA_TIME)*freq,choppy);
                h += d * amp;
              	uv *= octave_m; freq *= 1.9; amp *= 0.22;
                choppy = mix(choppy,1.0,0.2);
            }
            return p.y - h;
        }

        float map_detailed(vec3 p) {
            float SEA_TIME = 1.0 + iGlobalTime * SEA_SPEED;
            float freq = SEA_FREQ;
            float amp = SEA_HEIGHT;
            float choppy = SEA_CHOPPY;
            vec2 uv = p.xz; uv.x *= 0.75;

            float d, h = 0.0;
            for(int i = 0; i < ITER_FRAGMENT; i++) {
                d = sea_octave((uv+SEA_TIME)*freq,choppy);
            	  d += sea_octave((uv-SEA_TIME)*freq,choppy);
                h += d * amp;
            	  uv *= octave_m; freq *= 1.9; amp *= 0.22;
                choppy = mix(choppy,1.0,0.2);
            }
            return p.y - h;
        }

        vec3 getSeaColor(vec3 p, vec3 n, vec3 l, vec3 eye, vec3 dist) {
            float fresnel = clamp(1.0 - dot(n,-eye), 0.0, 1.0);
            fresnel = pow(fresnel,3.0) * 0.65;

            vec3 reflected = getSkyColor(reflect(eye,n));
            vec3 refracted = SEA_BASE + diffuse(n,l,80.0) * SEA_WATER_COLOR * 0.12;

            vec3 color = mix(refracted,reflected,fresnel);

            float atten = max(1.0 - dot(dist,dist) * 0.001, 0.0);
            color += SEA_WATER_COLOR * (p.y - SEA_HEIGHT) * 0.18 * atten;

            color += vec3(specular(n,l,eye,60.0));

            return color;
        }

        // tracing
        vec3 getNormal(vec3 p, float eps) {
            vec3 n;
            n.y = map_detailed(p);
            n.x = map_detailed(vec3(p.x+eps,p.y,p.z)) - n.y;
            n.z = map_detailed(vec3(p.x,p.y,p.z+eps)) - n.y;
            n.y = eps;
            return normalize(n);
        }

        float heightMapTracing(vec3 ori, vec3 dir, out vec3 p) {
            float tm = 0.0;
            float tx = 1000.0;
            float hx = map(ori + dir * tx);
            if(hx > 0.0) return tx;
            float hm = map(ori + dir * tm);
            float tmid = 0.0;
            for(int i = 0; i < NUM_STEPS; i++) {
                tmid = mix(tm,tx, hm/(hm-hx));
                p = ori + dir * tmid;
            	  float hmid = map(p);
            		if (hmid < 0.0) {
                    tx = tmid;
                    hx = hmid;
                } else {
                    tm = tmid;
                    hm = hmid;
                }
            }
            return tmid;
        }

        // main
        void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
        	  vec2 uv = fragCoord.xy / iResolution.xy;
            uv = uv * 2.0 - 1.0;
            uv.x *= iResolution.x / iResolution.y;
            float time = iGlobalTime * 0.3 + iMouse.x*0.01;

            // ray
            vec3 ang = vec3(sin(time*3.0)*0.1,sin(time)*0.2+0.3,time);
            vec3 ori = vec3(0.0,3.5,time*5.0);
            vec3 dir = normalize(vec3(uv.xy,-2.0)); dir.z += length(uv) * 0.15;
            dir = normalize(dir) * fromEuler(ang);

            // tracing
            vec3 p;
            heightMapTracing(ori,dir,p);
            vec3 dist = p - ori;
            float EPSILON_NRM = 0.1 / iResolution.x;
            vec3 n = getNormal(p, dot(dist,dist) * EPSILON_NRM);
            vec3 light = normalize(vec3(0.0,1.0,0.8));

            // color
            vec3 color = mix(
                getSkyColor(dir),
                getSeaColor(p,n,light,dir,dist),
            	  pow(smoothstep(0.0,-0.05,dir.y),0.3)
            );

            // post
        	  fragColor = vec4(pow(color,vec3(0.75)), 1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

    |]
