port module Main exposing (main)

import Json.Encode
import Test.Runner.Node
import Tests


main =
    Test.Runner.Node.run emit Tests.suite


port emit : ( String, Json.Encode.Value ) -> Cmd msg
