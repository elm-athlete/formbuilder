module FormBuilder.FieldBuilder
    exposing
        ( FieldView
        , object
        , default
        , defaultHidden
        )

{-| Creates generic fields for FormBuilder. It can be used as a base to creates new customs fields.


# Types

@docs FieldView


# Fields

@docs object
@docs default
@docs defaultHidden

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe exposing (andThen, withDefault)
import Json.Decode as Json exposing (Value, Decoder)
import FormBuilder.FieldBuilder.Attributes as Attributes
    exposing
        ( FieldAttributes
        , InputType(..)
        , AttributesModifier
        )


{-| Represent a view for a field. Takes attributes in parameters, and returns an HTML msg.
-}
type alias FieldView a msg =
    FieldAttributes a msg -> Html msg


option : String -> ( String, Int ) -> Html msg
option selected ( optionText, valueInsideList ) =
    let
        valueSelected =
            String.toInt selected |> Result.withDefault 0

        isSameValue =
            valueInsideList == valueSelected
    in
        Html.option
            [ Html.Attributes.value (toString valueInsideList)
            , Html.Attributes.selected isSameValue
            ]
            [ Html.text optionText ]


select : List (Html.Attribute msg) -> String -> List ( String, Int ) -> Html msg
select name selected options =
    Html.select name (options |> List.map (option selected))


label : Maybe String -> Bool -> Bool -> Html msg
label text isHidden mandatory =
    let
        label_ =
            if isHidden then
                Nothing
            else
                text
    in
        case label_ of
            Just labelName ->
                Html.label [] [ Html.text (labelText labelName mandatory) ]

            Nothing ->
                Html.text ""


input : InputType -> List (Html.Attribute msg) -> String -> Html msg
input inputType attributes val =
    Html.input
        (Html.Attributes.type_ (inputTypeToString inputType)
            :: (if inputType == File then
                    Html.Attributes.multiple True
                else
                    Html.Attributes.value val
               )
            :: attributes
        )
        []


addAttributeIfExist : Maybe a -> (a -> b) -> List b
addAttributeIfExist maybeAttribute htmlAttribute =
    case maybeAttribute of
        Nothing ->
            []

        Just attribute ->
            [ htmlAttribute attribute ]


decodeNode : (Value -> msg) -> Decoder msg
decodeNode toMsg =
    Json.map toMsg (Json.field "target" Json.value)


genericInput : FieldAttributes a msg -> Maybe (FieldView a msg) -> Html msg
genericInput attributes view =
    let
        isHidden =
            if attributes.common.mandatory && attributes.common.value == Nothing then
                False
            else
                attributes.common.hidden

        inputType =
            if isHidden then
                Hidden
            else
                attributes.common.type_ |> withDefault Text

        value =
            attributes.common.value |> Maybe.withDefault ""

        name =
            if attributes.common.removeNameIfEmpty && value == "" then
                []
            else
                case inputName attributes.common of
                    Nothing ->
                        []

                    Just inputName_ ->
                        [ Html.Attributes.name inputName_ ]

        attributes_ =
            List.concat
                [ [ Html.Attributes.required attributes.common.mandatory ]
                , if value == "" then
                    addAttributeIfExist attributes.common.placeholder Html.Attributes.placeholder
                  else
                    []
                , addAttributeIfExist (inputName attributes.common) Html.Attributes.name
                , addAttributeIfExist attributes.common.autocomplete Html.Attributes.autocomplete
                , addAttributeIfExist attributes.common.onInput Html.Events.onInput
                , addAttributeIfExist attributes.common.onBlur Html.Events.onBlur
                , addAttributeIfExist attributes.common.onFocus Html.Events.onFocus
                , addAttributeIfExist attributes.common.onChange (Html.Events.on "change" << decodeNode)
                ]
    in
        case view of
            Nothing ->
                case attributes.common.options of
                    Nothing ->
                        case inputType of
                            TextArea ->
                                Html.textarea (Html.Attributes.value value :: attributes_) [ Html.text value ]

                            _ ->
                                input inputType attributes_ value

                    Just options ->
                        select name value options

            Just view_ ->
                view_ attributes


{-| Generates a generic object, ready to be renderd by FormBuilder. Accepts attributes, view and modifiers.
-}
object : FieldAttributes a msg -> Maybe (FieldView a msg) -> List (AttributesModifier a msg) -> Html msg
object defaultAttributes view customModifiers =
    let
        attributes =
            defaultAttributes |> compose customModifiers

        isMandatory =
            attributes.common.mandatory

        isHidden =
            if isMandatory && attributes.common.value == Nothing then
                False
            else
                attributes.common.hidden

        noBottomPadding =
            attributes.common.noBottomPadding
    in
        Html.div
            (if isHidden || noBottomPadding then
                []
             else
                [ Html.Attributes.style [ ( "padding-bottom", "12px" ) ] ]
            )
            [ label attributes.common.label isHidden isMandatory
            , genericInput attributes view
            ]


{-| Generates a default input field. Generates default attributes, use the default view, and render the field with their attributes.
-}
default : List (AttributesModifier {} msg) -> Html msg
default =
    object Attributes.defaultAttributes Nothing


{-| Generates a default hidden input field. Generates default attributes, and force the field to be hidden.
-}
defaultHidden : List (AttributesModifier {} msg) -> Html msg
defaultHidden =
    object (Attributes.hidden Attributes.defaultAttributes) Nothing


hook : String -> String
hook str =
    "[" ++ str ++ "]"


inputName : Attributes.CommonAttributes msg -> Maybe String
inputName { objectName, fieldName, nestedName, nestedIndice } =
    case fieldName of
        Nothing ->
            Nothing

        Just fieldName_ ->
            case objectName of
                Nothing ->
                    Just fieldName_

                Just objectName_ ->
                    let
                        nested =
                            case nestedName of
                                Nothing ->
                                    ""

                                Just nestedName_ ->
                                    (hook (nestedName_ ++ "_attributes") ++ hook (nestedIndice |> withDefault 0 |> toString))
                    in
                        Just (objectName_ ++ nested ++ hook fieldName_)


inputTypeToString : InputType -> String
inputTypeToString inputType =
    case inputType of
        Hidden ->
            "hidden"

        File ->
            "file"

        _ ->
            "text"


mandatoryText : Bool -> String
mandatoryText mandatory =
    if mandatory then
        " * "
    else
        ""


labelText : String -> Bool -> String
labelText label mandatory =
    label
        ++ mandatoryText mandatory
        ++ " :"


compose : List (a -> a) -> (a -> a)
compose modifiers =
    modifiers
        |> List.foldr (<<) identity
