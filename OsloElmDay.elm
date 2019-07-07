module OsloElmDay exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import OsloElmDay.Mesh as Mesh
import OsloElmDay.Shader as Shader exposing (Attributes)
import Random exposing (Generator)
import Task
import WebGL exposing (Entity, Mesh)


speed : Float
speed =
    0.7


amount : Int
amount =
    40


magicClippingNumbers : ( Float, Float )
magicClippingNumbers =
    ( -1400, 2800 )


type alias Box =
    { i : Int
    , rotationXSpeed : Float
    , rotationYSpeed : Float
    , zPos : Float
    , mesh : Mesh Attributes
    , color : Vec3
    , randomizeYPos : Float
    , randomizeXSpeed : Float
    , rotationX : Float
    , rotationY : Float
    }


andMap : Generator a -> Generator (a -> b) -> Generator b
andMap =
    Random.map2 (|>)


randomBoxes : Int -> Generator (List Box)
randomBoxes =
    boxesHelp (Random.constant [])


boxesHelp : Generator (List Box) -> Int -> Generator (List Box)
boxesHelp generated n =
    if n == 0 then
        generated

    else
        boxesHelp
            (Random.map2 (::)
                (randomBox n)
                generated
            )
            (n - 1)


randomBox : Int -> Generator Box
randomBox i =
    Random.constant (Box i)
        |> andMap (Random.float 0.001 0.005)
        |> andMap (Random.float 0.001 0.005)
        |> andMap (Random.float -800 -1600)
        |> andMap randomMesh
        |> andMap randomColor
        |> andMap (Random.float 20 150)
        |> andMap (Random.float 1.01 1.03)
        |> andMap (Random.float 20 60)
        |> andMap (Random.float 20 60)


randomColor : Generator Vec3
randomColor =
    Random.map
        (\i ->
            List.head
                (List.drop i colors)
                |> Maybe.withDefault (vec3 0 0 0)
        )
        (Random.int 0 (List.length colors - 1))


randomMesh : Generator (Mesh Attributes)
randomMesh =
    Random.map
        Mesh.cube
        (Random.float 40 80)


colors : List Vec3
colors =
    List.map (hexToRgb >> Vec3.fromRecord)
        [ 0x00190E60
        , 0x00190E60
        , 0x00190E60
        , 0x00190E60
        , 0x00190E60
        , 0x00190E60
        , 0x00FFFFFF
        , 0x00252EDE
        , 0x00252EDE
        , 0x00FFCA6A
        ]


type alias Model =
    { width : Float
    , height : Float
    , delta : Float
    , boxes : List Box
    }


type Msg
    = Resize Float Float
    | Animate Float
    | Boxes (List Box)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( Model 0 0 0 []
                , Cmd.batch
                    [ Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
                    , Random.generate Boxes (randomBoxes amount)
                    ]
                )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Animate
                    , onResize (\w h -> Resize (toFloat w) (toFloat h))
                    ]
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate _ ->
            ( { model | delta = model.delta + speed }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height / 1.5 }
            , Cmd.none
            )

        Boxes boxes ->
            ( { model | boxes = boxes }, Cmd.none )


view : Model -> Html msg
view { width, height, delta, boxes } =
    let
        color =
            hexToRgb 0x000A0338

        perspective =
            Mat4.makePerspective 35 (width / height) 0.1 3000
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.clearColor color.x color.y color.z 1
        ]
        [ Attributes.width (round width)
        , Attributes.height (round height)
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "display" "block"
        ]
        (List.map (renderBox perspective delta) boxes)


remainder : Float -> Float -> Float
remainder a b =
    a - toFloat (floor (a / b)) * b


renderBox : Mat4 -> Float -> Box -> Entity
renderBox perspective delta box =
    let
        x =
            Tuple.first magicClippingNumbers + remainder (toFloat box.i * 80 + delta * box.randomizeXSpeed) (Tuple.second magicClippingNumbers)

        y =
            sin (toFloat box.i / 2 * 0.2 + delta * 0.001) * box.randomizeYPos

        z =
            box.zPos
    in
    WebGL.entity
        Shader.vertex
        Shader.fragment
        box.mesh
        { camera = Mat4.makeLookAt (vec3 0 0 700) (vec3 0 0 0) (vec3 0 1 0)
        , perspective = perspective
        , transform =
            Mat4.makeRotate (box.rotationX + delta * box.rotationXSpeed / speed) Vec3.i
                |> Mat4.mul (Mat4.makeRotate (box.rotationY + delta * box.rotationYSpeed / speed) Vec3.j)
                |> Mat4.mul (Mat4.makeTranslate3 x y z)
        , color = box.color
        , lightDirection = vec3 0 0 -1
        }


hexToRgb : Int -> { x : Float, y : Float, z : Float }
hexToRgb color =
    { x = toFloat (color // 65536) / 256
    , y = toFloat (modBy 256 (color // 256)) / 256
    , z = toFloat (modBy 256 color) / 256
    }
