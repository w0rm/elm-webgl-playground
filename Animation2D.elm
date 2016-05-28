import Mouse
import WebGL
import Math.Vector2 exposing (Vec2, vec2)
import Task exposing (Task)
import AnimationFrame
import Window
import Html.Attributes exposing (width, height, style)
import Html exposing (Html)
import Html.App as Html
import Time exposing (Time)


{- Types -}

type Action
  = Resize Window.Size
  | MouseMove Mouse.Position
  | Animate Time
  | TextureLoad WebGL.Texture
  | TextureError WebGL.Error


type alias Model =
  { size : Window.Size
  , position : Mouse.Position
  , maybeTexture : Maybe WebGL.Texture
  , elapsed : Time
  , frame : Int
  }


{- Program -}

main : Program Never
main =
  Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


init : (Model, Cmd Action)
init =
  { size = Window.Size 0 0
  , position = Mouse.Position 0 0
  , maybeTexture = Nothing
  , elapsed = 0
  , frame = 0
  } !
  [ WebGL.loadTexture "/texture/delivery-person.png" |> Task.perform TextureError TextureLoad
  , Window.size |> Task.perform Resize Resize
  ]


subscriptions : Model -> Sub Action
subscriptions _ =
  Sub.batch
   [ AnimationFrame.diffs Animate
   , Mouse.moves MouseMove
   , Window.resizes Resize
   ]

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Resize size ->
      { model | size = size } ! []

    MouseMove position ->
      { model | position = position } ! []

    Animate elapsed ->
      animate elapsed model ! []

    TextureLoad texture ->
      { model | maybeTexture = Just texture } ! []

    TextureError _ ->
      Debug.crash "Error loading texture"


animate : Time -> Model -> Model
animate elapsed model =
  let
    timeout = 150
    newElapsed = elapsed + model.elapsed
  in
    if newElapsed > timeout then
      { model
      | frame = (model.frame + 1) % 24
      , elapsed = newElapsed - timeout
      }
    else
      { model
      | elapsed = newElapsed
      }


type alias Vertex = { position : Vec2 }


mesh : WebGL.Drawable Vertex
mesh =
  WebGL.Triangle
    [ ( Vertex (vec2 0 0)
      , Vertex (vec2 64 128)
      , Vertex (vec2 64 0)
      )
    , ( Vertex (vec2 0 0)
      , Vertex (vec2 0 128)
      , Vertex (vec2 64 128)
      )
    ]


view : Model -> Html Action
view {size, maybeTexture, position, frame} =
  WebGL.toHtmlWith
    [ WebGL.Enable WebGL.Blend
    , WebGL.BlendFunc (WebGL.One, WebGL.OneMinusSrcAlpha)
    ]
    [ width size.width
    , height size.height
    , style [("display", "block")]
    ]
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          [ WebGL.render
              vertexShader
              fragmentShader
              mesh
              { screenSize = vec2 (toFloat size.width) (toFloat size.height)
              , offset = vec2 (toFloat position.x) (toFloat position.y)
              , texture = texture
              , frame = frame
              , textureSize = vec2 (toFloat (fst (WebGL.textureSize texture))) (toFloat (snd (WebGL.textureSize texture)))
              , frameSize = vec2 128 256
              }
          ]
    )


{- Shaders -}

vertexShader : WebGL.Shader {attr | position : Vec2} {unif | screenSize : Vec2, offset : Vec2} {texturePos : Vec2}
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


fragmentShader : WebGL.Shader {} {u | texture : WebGL.Texture, textureSize : Vec2, frameSize : Vec2, frame : Int } {texturePos : Vec2}
fragmentShader = [glsl|

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
