module FormBuilder.FieldBuilder.Attributes
    exposing
        ( InputType(..)
        , AttributesModifier
        , FieldAttributes
        , CommonAttributes
        , defaultAttributes
        , commonAttributes
        , label
        , objectName
        , fieldName
        , noBottomPadding
        , value
        , id
        , type_
        , mandatory
        , placeholder
        , hidden
        , options
        )

{-| Handle fields attributes. Create default attributes, and provide setters to modifiy them easily. Each modifier can be combined with others with function composition.

# Types
@docs InputType
@docs AttributesModifier
@docs FieldAttributes
@docs CommonAttributes

# Default Fields
@docs defaultAttributes
@docs commonAttributes

# Attributes Modifiers
@docs label
@docs objectName
@docs fieldName
@docs noBottomPadding
@docs value
@docs id
@docs type_
@docs mandatory
@docs placeholder
@docs hidden
@docs options
-}


{-| Represent the desired type for the input. If it should be Hidden, TextArea, Text or File.
-}
type InputType
    = Hidden
    | TextArea
    | Text
    | File


{-| Attributes for a field. Could be extended by subfields.
-}
type alias FieldAttributes sub msg =
    { sub | common : CommonAttributes msg }


{-| Common attributes shared by all form fields.
-}
type alias CommonAttributes msg =
    { value : String
    , objectName : Maybe String
    , fieldName : Maybe String
    , id : Maybe String
    , type_ : Maybe InputType
    , label : Maybe String
    , placeholder : Maybe String
    , mandatory : Bool
    , hidden : Bool
    , options : Maybe (List ( String, Int ))
    , noBottomPadding : Bool
    , onInput : Maybe (String -> msg)
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
    { value = ""
    , id = Nothing
    , objectName = Nothing
    , fieldName = Nothing
    , type_ = Nothing
    , label = Nothing
    , placeholder = Nothing
    , mandatory = False
    , hidden = False
    , options = Nothing
    , noBottomPadding = False
    , onInput = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onChange = Nothing
    }


updateDefaultAttributes : CommonAttributes msg -> FieldAttributes a msg -> FieldAttributes a msg
updateDefaultAttributes newAttributes fieldAttributes =
    { fieldAttributes | common = newAttributes }


{-| Set the value of the label.
-}
label : String -> FieldAttributes a msg -> FieldAttributes a msg
label lbl ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | label = Just lbl }


{-| Set the objectName of the field.
-}
objectName : String -> FieldAttributes a msg -> FieldAttributes a msg
objectName name ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | objectName = Just name }


{-| Set the fieldName of the field.
-}
fieldName : String -> FieldAttributes a msg -> FieldAttributes a msg
fieldName name ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | fieldName = Just name }


{-| Disable bottom padding.
-}
noBottomPadding : FieldAttributes a msg -> FieldAttributes a msg
noBottomPadding ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | noBottomPadding = True }


{-| Set the value of the field.
-}
value : String -> FieldAttributes a msg -> FieldAttributes a msg
value val ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | value = val }


{-| Set the id of the field.
-}
id : String -> FieldAttributes a msg -> FieldAttributes a msg
id id ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | id = Just id }


{-| Set the type of the field.
-}
type_ : InputType -> FieldAttributes a msg -> FieldAttributes a msg
type_ value ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | type_ = Just value }


{-| Makes the field mandatory.
-}
mandatory : FieldAttributes a msg -> FieldAttributes a msg
mandatory ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | mandatory = True }


{-| Set the placeholder of the field.
-}
placeholder : String -> FieldAttributes a msg -> FieldAttributes a msg
placeholder value ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | placeholder = Just value }


{-| Hide the field.
-}
hidden : FieldAttributes a msg -> FieldAttributes a msg
hidden ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | hidden = True }


{-| Set the options of the field.
-}
options : List ( String, Int ) -> FieldAttributes a msg -> FieldAttributes a msg
options options ({ common } as fieldAttributes) =
    fieldAttributes
        |> updateDefaultAttributes
            { common | options = Just options }
