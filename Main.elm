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
      , Vertex (vec2 1 1)
      , Vertex (vec2 1 0)
      )
    , ( Vertex (vec2 0 0)
      , Vertex (vec2 0 1)
      , Vertex (vec2 1 1)
      )
    ]


ortho2D : Float -> Float -> Mat4.Mat4
ortho2D w h =
  Mat4.makeOrtho2D 0 w h 0


view : (Int, Int) -> Maybe GL.Texture -> Element
view (w, h) maybeTexture =
  GL.webgl
    (500, 500)
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render (w, h) texture
    )


render : (Int, Int) -> GL.Texture -> List GL.Renderable
render (w, h) texture =
  let
    matrix = Mat4.makeOrtho2D 0 1 1 0 -- left right bottom top
  in
    [GL.render vertexShader fragmentShader mesh {mat = matrix, sprite = texture}]


-- Shaders

vertexShader : GL.Shader {attr | position : Vec2} {unif | mat : Mat4.Mat4} {pos : Vec2}
vertexShader = [glsl|

attribute vec2 position;
uniform mat4 mat;
varying vec2 pos;

void main () {
    gl_Position = mat * vec4(position, 0.0, 1.0);
    pos = position;
}

|]


fragmentShader : GL.Shader {} {u | sprite : GL.Texture } {pos : Vec2}
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D sprite;
varying vec2 pos;

void main () {
  gl_FragColor = texture2D(sprite, vec2(pos.x, -pos.y));
}

|]
