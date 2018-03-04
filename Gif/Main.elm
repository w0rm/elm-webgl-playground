port module Main exposing (main)

import AnimationFrame
import Mouse
import Html exposing (Html, div, text, img)
import Html.Attributes exposing (width, height, style, id, src)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Dict exposing (Dict)


port gifOut : Value -> Cmd msg


port gifIn : (Value -> msg) -> Sub msg


type GifOutMsg
    = StartRecording Int
    | AddFrame Int String Time
    | StopRecording Int


receiveFromPort : Value -> Msg
receiveFromPort =
    Decode.decodeValue
        (Decode.andThen
            (\msgType ->
                case msgType of
                    "Progress" ->
                        Decode.map2 Progress
                            (Decode.field "id" Decode.int)
                            (Decode.field "progress" Decode.float)

                    "Finished" ->
                        Decode.map2 Finished
                            (Decode.field "id" Decode.int)
                            (Decode.field "url" Decode.string)

                    "Aborted" ->
                        Decode.map Aborted
                            (Decode.field "id" Decode.int)

                    _ ->
                        Decode.fail "Undefined Type"
            )
            (Decode.field "type" Decode.string)
        )
        >> Result.withDefault NoOp


sendToPort : GifOutMsg -> Cmd msg
sendToPort msg =
    (case msg of
        StartRecording id ->
            Encode.object
                [ ( "type", Encode.string "StartRecording" )
                , ( "id", Encode.int id )
                ]

        AddFrame id canvasId delta ->
            Encode.object
                [ ( "type", Encode.string "AddFrame" )
                , ( "canvasId", Encode.string canvasId )
                , ( "id", Encode.int id )
                , ( "delay", Encode.float delta )
                ]

        StopRecording id ->
            Encode.object
                [ ( "type", Encode.string "StopRecording" )
                , ( "id", Encode.int id )
                ]
    )
        |> gifOut


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model Nothing Dict.empty 0, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type Msg
    = Animate Time
    | MouseUp Mouse.Position
    | MouseDown Mouse.Position
    | Progress Int Float
    | Finished Int String
    | Aborted Int
    | NoOp


type alias Recording =
    { progress : Float
    , url : Maybe String
    }


type alias Model =
    { recording : Maybe Int
    , recordings : Dict Int Recording
    , elapsed : Time
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , gifIn receiveFromPort
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown _ ->
            startRecording model

        Animate elapsed ->
            addFrame elapsed model

        MouseUp _ ->
            stopRecording model

        Progress id progress ->
            let
                recording =
                    Dict.get id model.recordings
                        |> Maybe.map (\r -> { r | progress = progress })
                        |> Maybe.withDefault { progress = progress, url = Nothing }
            in
                ( { model | recordings = Dict.insert id recording model.recordings }, Cmd.none )

        Finished id url ->
            ( { model | recordings = Dict.insert id { progress = 100, url = Just url } model.recordings }, Cmd.none )

        Aborted id ->
            ( { model | recordings = Dict.remove id model.recordings }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


startRecording : Model -> ( Model, Cmd Msg )
startRecording model =
    let
        id =
            model.recordings
                |> Dict.keys
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0

        recording =
            { progress = 0
            , url = Nothing
            }
    in
        ( { model
            | recording = Just id
            , recordings = Dict.insert id recording model.recordings
          }
        , sendToPort (StartRecording id)
        )


addFrame : Time -> Model -> ( Model, Cmd Msg )
addFrame delta model =
    let
        newModel =
            { model | elapsed = model.elapsed + delta }
    in
        case model.recording of
            Just id ->
                ( newModel, sendToPort (AddFrame id "canvas" delta) )

            Nothing ->
                ( newModel, Cmd.none )


stopRecording : Model -> ( Model, Cmd Msg )
stopRecording model =
    case model.recording of
        Just id ->
            ( { model | recording = Nothing }
            , sendToPort (StopRecording id)
            )

        Nothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            ]
        ]
        (WebGL.toHtml
            [ width 200
            , height 200
            , id "canvas"
            , style itemStyles
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { perspective = perspective (model.elapsed / 1000) }
            ]
            :: (List.map recording (Dict.toList model.recordings))
        )


recording : ( Int, Recording ) -> Html Msg
recording ( _, { progress, url } ) =
    div
        [ style itemStyles ]
        [ case url of
            Just imageUrl ->
                img [ src imageUrl ] []

            Nothing ->
                if progress == 0 then
                    div [] [ text "Recording" ]
                else
                    div [] [ text ("Encoding: " ++ toString (round (progress * 100)) ++ "%") ]
        ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


itemStyles : List ( String, String )
itemStyles =
    [ ( "width", "200px" )
    , ( "height", "200px" )
    , ( "text-align", "center" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    , ( "display", "flex" )
    , ( "background", "#ddd" )
    , ( "margin", "5px" )
    ]



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
