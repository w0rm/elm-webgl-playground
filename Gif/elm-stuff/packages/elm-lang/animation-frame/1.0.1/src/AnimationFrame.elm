effect module AnimationFrame where { subscription = MySub } exposing
  ( times, diffs
  )

{-| Browsers have their own render loop, repainting things as fast as possible.
If you want smooth animations in your application, it is helpful to sync up
with the browsers natural refresh rate. The subscriptions in this library fire
in step with the browser so you can make sure you are not doing extra animation
frames.

**Note:** The subscriptions in this library hook into JavaScript's
`requestAnimationFrame` function.

# Animation Subscriptions
@docs times, diffs

-}

import Native.AnimationFrame
import Process
import Task exposing (Task)
import Time exposing (Time)



{-| Subscribe to the current time, given in lockstep with the browser's natural
rerender speed.
-}
times : (Time -> msg) -> Sub msg
times tagger =
  subscription (Time tagger)


{-| Subscribe to the time diffs between animation frames, given in lockstep
with the browser's natural rerender speed.
-}
diffs : (Time -> msg) -> Sub msg
diffs tagger =
  subscription (Diff tagger)



-- SUBSCRIPTIONS


type MySub msg
  = Time (Time -> msg)
  | Diff (Time -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Time tagger ->
      Time (func << tagger)

    Diff tagger ->
      Diff (func << tagger)



-- EFFECT MANAGER


type alias State msg =
  { subs : List (MySub msg)
  , request : Maybe Process.Id
  , oldTime : Time
  }


init : Task Never (State msg)
init =
  Task.succeed (State [] Nothing 0)


onEffects : Platform.Router msg Time -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router subs {request, oldTime} =
  case (request, subs) of
    ( Nothing, [] ) ->
      Task.succeed (State [] Nothing oldTime)

    ( Just pid, [] ) ->
      Process.kill pid
        |> Task.andThen (\_ -> Task.succeed (State [] Nothing oldTime))

    ( Nothing, _ ) ->
      Process.spawn (Task.andThen (Platform.sendToSelf router) rAF)
        |> Task.andThen (\pid -> Time.now
        |> Task.andThen (\time -> Task.succeed (State subs (Just pid) time)))

    ( Just _, _ ) ->
      Task.succeed (State subs request oldTime)


onSelfMsg : Platform.Router msg Time -> Time -> State msg -> Task Never (State msg)
onSelfMsg router newTime {subs, oldTime} =
  let
    diff =
      newTime - oldTime

    send sub =
      case sub of
        Time tagger ->
          Platform.sendToApp router (tagger newTime)

        Diff tagger ->
          Platform.sendToApp router (tagger diff)
  in
    Process.spawn (Task.andThen (Platform.sendToSelf router) rAF)
      |> Task.andThen (\pid -> Task.sequence (List.map send subs)
      |> Task.andThen (\_ -> Task.succeed (State subs (Just pid) newTime)))


rAF : Task x Time
rAF =
  Native.AnimationFrame.create ()
