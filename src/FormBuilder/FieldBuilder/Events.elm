module FormBuilder.FieldBuilder.Events
    exposing
        ( Event
        , onInput
        , onFocus
        , onBlur
        , onChange
        )

{-| Handle events which may happen on fields. Each modifier can be combined with others with function composition.

# Type
@docs Event

# Events
@docs onInput
@docs onFocus
@docs onBlur
@docs onChange
-}

import FormBuilder.FieldBuilder.Attributes exposing (..)


modifyDefaultAttributes : FieldAttributes a msg -> CommonAttributes msg -> FieldAttributes a msg
modifyDefaultAttributes fieldAttributes newAttributes =
    { fieldAttributes | common = newAttributes }


{-| Event for triggering Cmd on action on a field.
-}
type alias Event msg =
    String -> msg


{-| Set the event to trigger on input on the field.
-}
onInput : Event msg -> FieldAttributes a msg -> FieldAttributes a msg
onInput event ({ common } as fieldAttributes) =
    { common | onInput = Just event }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the event to trigger on focus on the field.
-}
onFocus : msg -> FieldAttributes a msg -> FieldAttributes a msg
onFocus event ({ common } as fieldAttributes) =
    { common | onFocus = Just event }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the event to trigger on blur on the field.
-}
onBlur : msg -> FieldAttributes a msg -> FieldAttributes a msg
onBlur event ({ common } as fieldAttributes) =
    { common | onBlur = Just event }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the event to trigger on change on the field.
-}
onChange : msg -> FieldAttributes a msg -> FieldAttributes a msg
onChange msg ({ common } as fieldAttributes) =
    { common | onChange = Just msg }
        |> modifyDefaultAttributes fieldAttributes
