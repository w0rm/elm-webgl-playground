import Mouse
import WebGL as GL
import Math.Vector2 exposing (Vec2, vec2)
import Math.Matrix4 as Mat4
import Graphics.Element exposing (Element)
import Task exposing (Task)


main : Signal Element
main =
  Signal.map2 view Mouse.position texture.signal


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


ortho2D : Float -> Float -> Mat4.Mat4
ortho2D w h =
  Mat4.makeOrtho2D 0 w h 0


view : (Int, Int) -> Maybe GL.Texture -> Element
view (w, h) maybeTexture =
  GL.webgl
    (1024, 1024)
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render (1024, 1024) texture
    )


render : (Int, Int) -> GL.Texture -> List GL.Renderable
render (w, h) texture =
  [GL.render vertexShader fragmentShader mesh {resolution = vec2 (toFloat w) (toFloat h), sprite = texture}]


-- Shaders

vertexShader : GL.Shader {attr | position : Vec2} {unif | resolution : Vec2} {texturePos : Vec2}
vertexShader = [glsl|

attribute vec2 position;
uniform vec2 resolution;
varying vec2 texturePos;

void main () {
  vec2 clipSpace = position / resolution * 2.0 - 1.0;
  gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
  texturePos = vec2(clipSpace.x, -clipSpace.y);
}

|]


fragmentShader : GL.Shader {} {u | sprite : GL.Texture } {texturePos : Vec2}
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D sprite;
varying vec2 texturePos;

void main () {
  gl_FragColor = texture2D(sprite, texturePos);
}

|]
