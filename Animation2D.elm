module Animation2D exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseMove, onResize)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Vector2 exposing (Vec2, vec2)
import Task exposing (Task)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Error, Texture)


{-| Types
-}
type Action
    = Resize Float Float
    | MouseMove Float Float
    | Animate Float
    | TextureLoad Texture
    | TextureError Error


type alias Model =
    { width : Float
    , height : Float
    , left : Float
    , top : Float
    , maybeTexture : Maybe Texture
    , elapsed : Float
    , frame : Int
    }


{-| Program
-}
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
      , left = 0
      , top = 0
      , maybeTexture = Nothing
      , elapsed = 0
      , frame = 0
      }
    , Cmd.batch
        [ Texture.load "animation2d.png"
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            TextureLoad val
                )
        , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
        ]
    )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onMouseMove mousePosition
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
            ( { model | width = width, height = height }
            , Cmd.none
            )

        MouseMove left top ->
            ( { model | left = left, top = top }
            , Cmd.none
            )

        Animate elapsed ->
            ( animate elapsed model
            , Cmd.none
            )

        TextureLoad texture ->
            ( { model | maybeTexture = Just texture }
            , Cmd.none
            )

        TextureError _ ->
            ( model
            , Cmd.none
            )


animate : Float -> Model -> Model
animate elapsed model =
    let
        timeout =
            150

        newElapsed =
            elapsed + model.elapsed
    in
    if newElapsed > timeout then
        { model
            | frame = modBy 24 (model.frame + 1)
            , elapsed = newElapsed - timeout
        }

    else
        { model
            | elapsed = newElapsed
        }


view : Model -> Html Action
view { width, height, left, top, maybeTexture, frame } =
    WebGL.toHtml
        [ Attributes.width (round width)
        , Attributes.height (round height)
        , Attributes.style "display" "block"
        , Attributes.style "position" "absolute"
        , Attributes.style "left" "0"
        , Attributes.style "top" "0"
        ]
        (case maybeTexture of
            Nothing ->
                []

            Just texture ->
                [ WebGL.entityWith
                    [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
                    vertexShader
                    fragmentShader
                    mesh
                    { screenSize = vec2 width height
                    , offset = vec2 left top
                    , texture = texture
                    , frame = frame
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                    , frameSize = vec2 128 256
                    }
                ]
        )



{- Mesh and shaders -}


type alias Vertex =
    { position : Vec2 }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 64 128)
          , Vertex (vec2 64 0)
          )
        , ( Vertex (vec2 0 0)
          , Vertex (vec2 0 128)
          , Vertex (vec2 64 128)
          )
        ]


vertexShader : WebGL.Shader { attr | position : Vec2 } { unif | screenSize : Vec2, offset : Vec2 } { texturePos : Vec2 }
vertexShader =
    [glsl|

  attribute vec2 position;
  uniform vec2 offset;
  uniform vec2 screenSize;
  varying vec2 texturePos;

  void main () {
    vec2 clipSpace = (position + offset) / screenSize * 2.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
    texturePos = position;
  }

|]


fragmentShader : WebGL.Shader {} { u | texture : Texture, textureSize : Vec2, frameSize : Vec2, frame : Int } { texturePos : Vec2 }
fragmentShader =
    [glsl|

  precision mediump float;
  uniform sampler2D texture;
  uniform vec2 textureSize;
  uniform vec2 frameSize;
  uniform int frame;
  varying vec2 texturePos;

  void main () {
    vec2 size = frameSize / textureSize;
    int frames = int(1.0 / size.x);
    vec2 frameOffset = size * vec2(float(frame - frame / frames * frames), -float(frame / frames));
    vec2 textureClipSpace = texturePos / textureSize * 2.0 - 1.0;
    vec4 temp = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
    float a = temp.a;
    gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
  }

|]
