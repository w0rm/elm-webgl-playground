import Mouse
import WebGL as GL
import Math.Vector2 exposing (Vec2, vec2)
import Graphics.Element exposing (Element)
import Task exposing (Task)
import Time exposing (Time)


currentFrame : Signal Int
currentFrame =
  Signal.foldp (\_ b -> (b + 1) % 24) 0 (Time.fps 15)


main : Signal Element
main =
  Signal.map3 (view (500, 500)) Mouse.position currentFrame texture.signal


type alias Vertex = { position : Vec2 }


texture : Signal.Mailbox (Maybe GL.Texture)
texture =
  Signal.mailbox Nothing


port textureFetcher : Task GL.Error ()
port textureFetcher =
  GL.loadTexture "/texture/delivery-person.png"
    `Task.andThen` \tex -> Signal.send texture.address (Just tex)


mesh : GL.Drawable Vertex
mesh =
  GL.Triangle
    [ ( Vertex (vec2 0 0)
      , Vertex (vec2 64 128)
      , Vertex (vec2 64 0)
      )
    , ( Vertex (vec2 0 0)
      , Vertex (vec2 0 128)
      , Vertex (vec2 64 128)
      )
    ]


view : (Int, Int) -> (Int, Int) -> Int -> Maybe GL.Texture -> Element
view dimensions position frame maybeTexture =
  GL.webglWithConfig
    [ GL.Enable GL.Blend
    , GL.Disable GL.DepthTest
    , GL.BlendFunc (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    ]
    dimensions
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render dimensions position frame texture
    )


render : (Int, Int) -> (Int, Int) -> Int -> GL.Texture -> List GL.Renderable
render (w, h) (x, y) frame texture =
  [ GL.render
      vertexShader
      fragmentShader
      mesh
      { screenSize = vec2 (toFloat w) (toFloat h)
      , offset = vec2 (toFloat x) (toFloat y)
      , sprite = texture
      , frame = frame
      , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
      , frameSize = vec2 128 256
      }
  ]


-- Shaders

vertexShader : GL.Shader {attr | position : Vec2} {unif | screenSize : Vec2, offset : Vec2} {texturePos : Vec2}
vertexShader = [glsl|

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


fragmentShader : GL.Shader {} {u | sprite : GL.Texture, textureSize : Vec2, frameSize : Vec2, frame : Int } {texturePos : Vec2}
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D sprite;
uniform vec2 textureSize;
uniform vec2 frameSize;
uniform int frame;
varying vec2 texturePos;

void main () {
  vec2 size = frameSize / textureSize;
  int frames = int(1.0 / size.x);
  vec2 frameOffset = size * vec2(float(frame - frame / frames * frames), -float(frame / frames));
  vec2 textureClipSpace = texturePos / textureSize * 2.0 - 1.0;
  gl_FragColor = texture2D(sprite, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
}

|]
