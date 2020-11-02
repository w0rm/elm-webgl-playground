module Elm3dGameJam exposing (main)

import Angle
import Axis3d exposing (Axis3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Http
import Length exposing (Meters, centimeters, millimeters)
import Obj.Decode exposing (Decoder)
import Path
import Pixels exposing (Pixels, pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Scene3d.Mesh exposing (Uniform)
import Segment
import SketchPlane3d
import SubPath
import Task
import TriangularMesh
import Vector3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , maybeMesh : Maybe (Uniform WorldCoordinates)
    }


type Msg
    = Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | LoadedMesh (Result Http.Error (Uniform WorldCoordinates))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize width height ->
            { model | width = width, height = height }

        LoadedMesh result ->
            { model | maybeMesh = Result.toMaybe result }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize
            (\width height ->
                Resize (pixels (toFloat width)) (pixels (toFloat height))
            )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = pixels 0
      , height = pixels 0
      , maybeMesh = Nothing
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize (pixels viewport.width) (pixels viewport.height)
            )
            Browser.Dom.getViewport
        , Http.get
            { url = "3d.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Obj.Decode.expectObj LoadedMesh (\a -> Length.centimeters (a * 20)) mesh
            }
        ]
    )


view : Model -> Html Msg
view { width, height, maybeMesh } =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 0 -0.8 0
                        , focalPoint = Point3d.meters 0 0 0
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 24
                }
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 75) (Angle.degrees -15)
            , shadows = True
            , camera = camera
            , dimensions =
                ( Pixels.int (round (Pixels.toFloat width))
                , Pixels.int (round (Pixels.toFloat height))
                )
            , background = Scene3d.backgroundColor (Color.rgb255 0x99 0x73 0xF1)
            , clipDepth = Length.meters 0.1
            , entities =
                [ case maybeMesh of
                    Just threeD ->
                        Scene3d.mesh
                            (Material.nonmetal
                                { baseColor = Color.rgb255 0xFA 0x8E 0x49
                                , roughness = 0.4
                                }
                            )
                            threeD

                    Nothing ->
                        Scene3d.nothing
                , Scene3d.mesh
                    (Material.nonmetal
                        { baseColor = Color.rgb255 0xE0 0x00 0x00
                        , roughness = 0.6
                        }
                    )
                    tubes
                    |> Scene3d.placeIn (Frame3d.atPoint (Point3d.centimeters 0 -20 -2.5))
                ]
            }
        ]


{-| Decode 3D mesh from OBJ file
-}
mesh : Decoder (Uniform WorldCoordinates)
mesh =
    Obj.Decode.map
        (\texturedFaces ->
            texturedFaces
                |> Scene3d.Mesh.indexedFaces
                |> Scene3d.Mesh.cullBackFaces
        )
        (Obj.Decode.facesIn Frame3d.atOrigin)


{-| Generate tubes mesh from SVG path
-}
tubes : Uniform WorldCoordinates
tubes =
    let
        centerOffset =
            Vector3d.millimeters (-800 / 2) 0 (800 / 2)
                |> Vector3d.scaleBy scale

        scale =
            0.12

        radius =
            millimeters 1.6

        segment =
            5

        slope =
            50

        resolution =
            8

        pathToEntity path result =
            path
                |> SubPath.arcLengthParameterized 0.01
                |> (\parametrization ->
                        let
                            length =
                                SubPath.arcLength parametrization

                            subdivisions =
                                round (length / segment)

                            pointOnPath u =
                                let
                                    ( px, py ) =
                                        SubPath.pointAlong parametrization (length * u)
                                            |> Maybe.withDefault ( 0, 0 )

                                    ( tx, ty ) =
                                        SubPath.tangentAlong parametrization (length * u)
                                            |> Maybe.withDefault ( 0, 0 )
                                in
                                { position =
                                    Point3d.millimeters (px * scale) (u * slope * scale) -(py * scale)
                                , tangent = Direction3d.unsafe { x = tx, y = 0, z = -ty }
                                , normal = Direction3d.y
                                }

                            endCap p =
                                let
                                    { tangent, position, normal } =
                                        pointOnPath p
                                in
                                TriangularMesh.ball resolution
                                    resolution
                                    (\u v ->
                                        let
                                            sketchPlane =
                                                SketchPlane3d.through position tangent

                                            theta =
                                                Angle.radians (2 * pi * u)

                                            phi =
                                                Angle.radians (-pi / 2 + pi * v)

                                            direction =
                                                Direction3d.fromAzimuthInAndElevationFrom sketchPlane theta phi
                                        in
                                        { position =
                                            position
                                                |> Point3d.translateIn direction radius
                                                |> Point3d.translateBy centerOffset
                                        , normal = Direction3d.toVector direction
                                        }
                                    )

                            tube =
                                TriangularMesh.tube subdivisions
                                    resolution
                                    (\u v ->
                                        let
                                            { tangent, position, normal } =
                                                pointOnPath u

                                            frame =
                                                Frame3d.atOrigin
                                                    |> Frame3d.translateIn normal radius
                                                    |> Frame3d.rotateAround (Axis3d.withDirection tangent position) (Angle.turns -v)
                                                    |> Frame3d.translateBy centerOffset
                                        in
                                        { position = Point3d.placeIn frame position
                                        , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                        }
                                    )
                        in
                        tube
                            :: endCap 0
                            :: endCap 1
                            :: result
                   )
    in
    "M738.905,301.344c-48.612,0-87.693,32.409-87.693,77.208c0,25.736,19.064,47.66,42.894,47.66c24.783,0,40.988-9.532,40.988-21.924c0-8.579-5.72-12.392-19.064-12.392c-28.596,0-50.519,24.783-50.519,57.191c0,28.596,21.923,50.52,55.285,50.52c38.127,0,64.815-15.251,78.161-34.314M396.709,593.021c-9.532,81.021-66.724,168.715-97.226,168.715c-10.485,0-20.97-20.97-20.97-53.378c0-74.35,36.222-144.886,58.145-144.886c22.876,0,63.864,101.039,63.864,186.826c0,2.859,0,9.531-0.953,13.345M345.237,328.034c-16.204,64.817-104.852,170.622-137.26,170.622c-7.625,0-11.438-6.673-11.438-24.782c0-83.881,41.94-174.436,67.677-174.436c40.034,0,62.911,138.214,60.051,194.452M479.637,743.627c-2.859-12.392-4.766-28.597-4.766-44.801c0-81.975,26.689-134.4,50.519-134.4c18.111,0,36.222,30.502,36.222,77.208c0,23.83-6.672,38.128-15.251,38.128c-6.673,0-12.392-8.578-12.392-30.502c0-35.268,23.83-66.724,46.707-66.724c23.829,0,44.8,38.129,44.8,112.478c0,29.549-5.719,55.285-13.345,67.677M112.657,715.03c0,27.643,18.111,46.706,40.988,46.706c29.548,0,47.66-30.501,47.66-88.646c0-37.175-2.86-73.396-8.579-108.664M421.492,497.703c-5.719-16.205-9.532-41.941-9.532-75.304c0-63.863,24.784-107.71,49.566-107.71c17.157,0,37.175,24.783,37.175,59.098c0,20.017-7.626,33.361-18.11,33.361c-9.532,0-16.205-10.485-16.205-32.408c0-43.847,25.736-75.303,51.473-75.303c31.456,0,61.957,49.567,61.957,129.635c0,21.923-3.812,52.426-8.578,68.631M134.581,311.83c-9.532-7.626-23.83-12.392-42.894-12.392c-48.613,0-89.6,40.987-89.6,106.759c0,60.05,27.643,92.459,64.817,92.459c47.659,0,81.021-28.596,81.021-62.91c0-20.971-9.532-29.549-42.893-29.549c-14.298,0-34.315,3.812-49.566,9.531M444.369,188.868c-0.954-3.813-1.906-11.438-1.906-18.111c0-57.192,34.314-134.4,58.145-134.4c17.157,0,39.08,40.987,39.08,90.553c0,24.783-5.719,35.268-13.344,35.268c-6.673,0-10.485-4.766-10.485-16.204c0-47.66,40.034-109.617,65.771-109.617c22.876,0,45.754,40.987,45.754,122.009c0,20.017-2.86,47.66-6.673,68.63M340.471,36.356c-9.532,51.473-17.157,116.29-17.157,155.371c0,31.455,8.579,42.894,28.596,42.894c26.689,0,75.303-9.532,91.506-16.205M227.994,36.356c-48.613,0-87.694,32.409-87.694,77.208c0,25.736,19.064,47.66,42.893,47.66c24.783,0,40.988-9.532,40.988-21.924c0-8.579-5.719-12.392-19.064-12.392c-28.596,0-50.519,24.783-50.519,57.192c0,28.596,21.923,50.52,55.285,50.52c38.127,0,64.817-15.251,78.162-34.315"
        |> Path.parse
        |> Result.withDefault []
        |> List.foldl pathToEntity []
        |> TriangularMesh.combine
        |> Scene3d.Mesh.indexedFaces
        |> Scene3d.Mesh.cullBackFaces
