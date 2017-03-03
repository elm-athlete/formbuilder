module FormBuilder.FieldBuilder.Attributes
    exposing
        ( InputType(..)
        , AttributesModifier
        , Event
        , FieldAttributes
        , defaultAttributes
        , commonAttributes
        , label
        , noBottomPadding
        , value
        , id
        , type_
        , mandatory
        , placeholder
        , hidden
        , options
        , onInput
        , onFocus
        , onBlur
        , onChange
        )

{-|
# Types
@docs InputType
@docs AttributesModifier
@docs Event
@docs FieldAttributes

# Default Fields
@docs defaultAttributes
@docs commonAttributes

# Attributes Modifiers
@docs label
@docs noBottomPadding
@docs value
@docs id
@docs type_
@docs mandatory
@docs placeholder
@docs hidden
@docs options
@docs onInput
@docs onFocus
@docs onBlur
@docs onChange
-}


{-| Represent the desired type for the input. If it should be Hidden, TextArea, Text or File.
-}
type InputType
    = Hidden
    | TextArea
    | Text
    | File


{-| Event for triggering Cmd on action on a field.
-}
type alias Event msg =
    String -> msg


{-| Attributes for a field. Could be extended by subfields.
-}
type alias FieldAttributes sub msg =
    { sub | common : CommonAttributes msg }


type alias CommonAttributes msg =
    { value : Maybe String
    , objectName : Maybe String
    , fieldName : Maybe String
    , id : Maybe String
    , type_ : Maybe InputType
    , label : Maybe String
    , placeholder : Maybe String
    , mandatory : Bool
    , hidden : Bool
    , options : Maybe (List ( String, Int ))
    , event : Maybe (Event msg)
    , noBottomPadding : Bool
    , onInput : Maybe (Event msg)
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    , onChange : Maybe msg
    }


{-| Represent a modifier for field attributes. Take attributes as input, and output modified attributes.
-}
type alias AttributesModifier a msg =
    FieldAttributes a msg -> FieldAttributes a msg


{-| Instantiate default attributes for any fields.
-}
defaultAttributes : FieldAttributes {} msg
defaultAttributes =
    { common = commonAttributes }


{-| Instantiate default attributes for any fields.
-}
commonAttributes : CommonAttributes msg
commonAttributes =
    { value = Nothing
    , id = Nothing
    , objectName = Nothing
    , fieldName = Nothing
    , type_ = Nothing
    , label = Nothing
    , placeholder = Nothing
    , mandatory = False
    , hidden = False
    , options = Nothing
    , event = Nothing
    , noBottomPadding = False
    , onInput = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onChange = Nothing
    }


modifyDefaultAttributes : FieldAttributes a msg -> CommonAttributes msg -> FieldAttributes a msg
modifyDefaultAttributes fieldAttributes newAttributes =
    { fieldAttributes | common = newAttributes }


{-| Set the value of the label.
-}
label : String -> FieldAttributes a msg -> FieldAttributes a msg
label lbl ({ common } as fieldAttributes) =
    { common | label = Just lbl }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the objectName of the field.
-}
objectName : String -> FieldAttributes a msg -> FieldAttributes a msg
objectName name ({ common } as fieldAttributes) =
    { common | objectName = Just name }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the fieldName of the field.
-}
fieldName : String -> FieldAttributes a msg -> FieldAttributes a msg
fieldName name ({ common } as fieldAttributes) =
    { common | fieldName = Just name }
        |> modifyDefaultAttributes fieldAttributes


{-| Disable bottom padding.
-}
noBottomPadding : FieldAttributes a msg -> FieldAttributes a msg
noBottomPadding ({ common } as fieldAttributes) =
    { common | noBottomPadding = True }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the value of the field.
-}
value : String -> FieldAttributes a msg -> FieldAttributes a msg
value val ({ common } as fieldAttributes) =
    { common | value = Just val }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the id of the field.
-}
id : String -> FieldAttributes a msg -> FieldAttributes a msg
id id ({ common } as fieldAttributes) =
    { common | id = Just id }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the type of the field.
-}
type_ : InputType -> FieldAttributes a msg -> FieldAttributes a msg
type_ value ({ common } as fieldAttributes) =
    { common | type_ = Just value }
        |> modifyDefaultAttributes fieldAttributes


{-| Makes the field mandatory.
-}
mandatory : FieldAttributes a msg -> FieldAttributes a msg
mandatory ({ common } as fieldAttributes) =
    { common | mandatory = True }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the placeholder of the field.
-}
placeholder : String -> FieldAttributes a msg -> FieldAttributes a msg
placeholder value ({ common } as fieldAttributes) =
    { common | placeholder = Just value }
        |> modifyDefaultAttributes fieldAttributes


{-| Hide the field.
-}
hidden : FieldAttributes a msg -> FieldAttributes a msg
hidden ({ common } as fieldAttributes) =
    { common | hidden = True }
        |> modifyDefaultAttributes fieldAttributes


{-| Set the options of the field.
-}
options : List ( String, Int ) -> FieldAttributes a msg -> FieldAttributes a msg
options options ({ common } as fieldAttributes) =
    { common | options = Just options }
        |> modifyDefaultAttributes fieldAttributes


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
