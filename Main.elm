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
  Signal.map3 view Mouse.position currentFrame texture.signal


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


view : (Int, Int) -> Int -> Maybe GL.Texture -> Element
view (x, y) frame maybeTexture =
  GL.webgl
    (1024, 1024)
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render (1024, 1024) (x, y) frame texture
    )


render : (Int, Int) -> (Int, Int) -> Int -> GL.Texture -> List GL.Renderable
render (w, h) (x, y) frame texture =
  [ GL.render
      vertexShader
      fragmentShader
      mesh
      { resolution = vec2 (toFloat w) (toFloat h)
      , offset = vec2 (toFloat x) (toFloat y)
      , sprite = texture
      , frame = frame
      , size = vec2 (128 / toFloat (fst (GL.textureSize texture))) (256 / toFloat (snd (GL.textureSize texture)))
      }
  ]


-- Shaders

vertexShader : GL.Shader {attr | position : Vec2} {unif | resolution : Vec2, offset : Vec2} {texturePos : Vec2}
vertexShader = [glsl|

attribute vec2 position;
uniform vec2 offset;
uniform vec2 resolution;
varying vec2 texturePos;

void main () {
  vec2 clipSpace = (position + offset) / resolution * 2.0 - 1.0;
  gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
  vec2 textureClipSpace = position / resolution * 2.0 - 1.0;
  texturePos = vec2(textureClipSpace.x, -textureClipSpace.y);
}

|]


fragmentShader : GL.Shader {} {u | sprite : GL.Texture, size : Vec2, frame : Int } {texturePos : Vec2}
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D sprite;
uniform vec2 size;
uniform int frame;
varying vec2 texturePos;

void main () {
  int frames = int(1.0 / size.x);
  vec2 frameOffset = size * vec2(float(frame - frame / frames * frames), -float(frame / frames));
  gl_FragColor = texture2D(sprite, texturePos + frameOffset);
}

|]
