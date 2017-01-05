module Tangram.Mesh exposing (tetrahedron, cube, parallelepiped)

import Math.Vector3 exposing (Vec3, vec3, sub, cross)
import Tangram.Shader exposing (Attribute)
import WebGL exposing (Mesh)


tetrahedron : Mesh Attribute
tetrahedron =
    WebGL.triangles
        [ -- front face
          attribute (vec3 0 1 0) (vec3 -1 0 0) (vec3 0 -1 0)
          -- rest faces
        , attribute (vec3 -0.5 0 -1) (vec3 -1 0 0) (vec3 0 1 0)
        , attribute (vec3 -0.5 0 -1) (vec3 0 -1 0) (vec3 -1 0 0)
        , attribute (vec3 -0.5 0 -1) (vec3 0 1 0) (vec3 0 -1 0)
        ]


cube : Mesh Attribute
cube =
    WebGL.triangles
        [ -- front face
          attribute (vec3 -1 0 0) (vec3 1 0 0) (vec3 0 1 0)
        , attribute (vec3 -1 0 0) (vec3 0 -1 0) (vec3 1 0 0)
          -- back face
        , attribute (vec3 -1 0 -sqrt2) (vec3 0 1 -sqrt2) (vec3 1 0 -sqrt2)
        , attribute (vec3 -1 0 -sqrt2) (vec3 1 0 -sqrt2) (vec3 0 -1 -sqrt2)
          -- left top
        , attribute (vec3 -1 0 0) (vec3 0 1 0) (vec3 0 1 -sqrt2)
        , attribute (vec3 -1 0 -sqrt2) (vec3 -1 0 0) (vec3 0 1 -sqrt2)
          -- right top
        , attribute (vec3 0 1 0) (vec3 1 0 0) (vec3 1 0 -sqrt2)
        , attribute (vec3 0 1 -sqrt2) (vec3 0 1 0) (vec3 1 0 -sqrt2)
          -- left bottom
        , attribute (vec3 -1 0 0) (vec3 0 -1 -sqrt2) (vec3 0 -1 0)
        , attribute (vec3 -1 0 -sqrt2) (vec3 0 -1 -sqrt2) (vec3 -1 0 0)
          -- right bottom
        , attribute (vec3 1 0 0) (vec3 0 -1 0) (vec3 0 -1 -sqrt2)
        , attribute (vec3 1 0 -sqrt2) (vec3 1 0 0) (vec3 0 -1 -sqrt2)
        ]


parallelepiped : Mesh Attribute
parallelepiped =
    WebGL.triangles
        [ -- front face
          attribute (vec3 -3 1 0) (vec3 3 -1 0) (vec3 1 1 0)
        , attribute (vec3 -3 1 0) (vec3 -1 -1 0) (vec3 3 -1 0)
          -- back face
        , attribute (vec3 -3 1 -2) (vec3 1 1 -2) (vec3 3 -1 -2)
        , attribute (vec3 -3 1 -2) (vec3 3 -1 -2) (vec3 -1 -1 -2)
          -- top face
        , attribute (vec3 -3 1 0) (vec3 1 1 0) (vec3 -3 1 -2)
        , attribute (vec3 -3 1 -2) (vec3 1 1 0) (vec3 1 1 -2)
          -- right face
        , attribute (vec3 1 1 0) (vec3 3 -1 0) (vec3 3 -1 -2)
        , attribute (vec3 1 1 -2) (vec3 1 1 0) (vec3 3 -1 -2)
          -- bottom face
        , attribute (vec3 3 -1 0) (vec3 -1 -1 0) (vec3 3 -1 -2)
        , attribute (vec3 3 -1 -2) (vec3 -1 -1 0) (vec3 -1 -1 -2)
          -- left face
        , attribute (vec3 -3 1 0) (vec3 -1 -1 -2) (vec3 -1 -1 0)
        , attribute (vec3 -3 1 -2) (vec3 -1 -1 -2) (vec3 -3 1 0)
        ]


{-| Adds a normal to each vertex
-}
attribute : Vec3 -> Vec3 -> Vec3 -> ( Attribute, Attribute, Attribute )
attribute v1 v2 v3 =
    let
        normal =
            cross (sub v1 v2) (sub v1 v3)
    in
        ( Attribute v1 normal
        , Attribute v2 normal
        , Attribute v3 normal
        )


sqrt2 : Float
sqrt2 =
    sqrt 2
