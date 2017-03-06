module FormBuilder.FieldBuilder
    exposing
        ( FieldView
        , object
        , default
        , defaultHidden
        )

{-|
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
import Json.Decode as Json
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
input inputType attrs val =
    Html.input
        (Html.Attributes.type_ (inputTypeToString inputType)
            :: (if inputType == File then
                    attrs
                else
                    (Html.Attributes.value val) :: attrs
               )
        )
        []


genericInput : FieldAttributes a msg -> Maybe (FieldView a msg) -> Html msg
genericInput attributes view =
    let
        isHidden =
            if attributes.common.mandatory && attributes.common.value == "" then
                False
            else
                attributes.common.hidden

        inputType =
            if isHidden then
                Hidden
            else
                attributes.common.type_ |> withDefault Text

        value =
            attributes.common.value

        name =
            case inputName attributes.common.objectName attributes.common.fieldName of
                Nothing ->
                    []

                Just inputName_ ->
                    [ Html.Attributes.name inputName_ ]

        addAttributeIfExist maybeAttribute htmlAttribute =
            case maybeAttribute of
                Nothing ->
                    []

                Just maybeAttribute ->
                    [ htmlAttribute maybeAttribute ]

        attributes_ =
            List.concat
                [ [ Html.Attributes.required attributes.common.mandatory ]
                , addAttributeIfExist attributes.common.placeholder Html.Attributes.placeholder
                , addAttributeIfExist (inputName attributes.common.objectName attributes.common.fieldName) Html.Attributes.name
                , addAttributeIfExist attributes.common.id Html.Attributes.id
                , addAttributeIfExist attributes.common.onInput Html.Events.onInput
                , addAttributeIfExist attributes.common.onBlur Html.Events.onBlur
                , addAttributeIfExist attributes.common.onFocus Html.Events.onFocus
                , addAttributeIfExist attributes.common.onChange <| Html.Events.on "change" << Json.succeed
                ]
    in
        case view of
            Nothing ->
                case attributes.common.options of
                    Nothing ->
                        case inputType of
                            TextArea ->
                                Html.textarea attributes_ [ Html.text value ]

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
            if isMandatory && attributes.common.value == "" then
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
                [ Html.Attributes.class "pb" ]
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


inputName : Maybe String -> Maybe String -> Maybe String
inputName objectName attributeName =
    case attributeName of
        Nothing ->
            Nothing

        Just attributeName_ ->
            case objectName of
                Nothing ->
                    Just attributeName_

                Just objectName_ ->
                    Just (objectName_ ++ "[" ++ attributeName_ ++ "]")


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
