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


{-| Represent a view for a field. Takes attributes in parameters, and returns an HTML msg. -}
type alias FieldView a msg =
    FieldAttributes a msg -> List (Html.Attribute msg) -> List (Html.Attribute msg) -> String -> Html msg


option : String -> ( String, Int ) -> Html msg
option selected current =
    let
        valueInsideList : Int
        valueInsideList =
            Tuple.second current

        valueSelected : Int
        valueSelected =
            String.toInt selected |> Result.withDefault 0

        isSameValue : Bool
        isSameValue =
            valueInsideList == valueSelected
    in
        Html.option
            [ Html.Attributes.value (toString valueInsideList)
            , Html.Attributes.selected isSameValue
            ]
            [ Html.text (Tuple.first current) ]


select : List (Html.Attribute msg) -> String -> List ( String, Int ) -> Html msg
select name selected options =
    Html.select name (options |> List.map (option selected))


label : Maybe String -> Bool -> Bool -> Html msg
label text isHidden mandatory =
    let
        lbl =
            if isHidden then
                Nothing
            else
                text
    in
        case lbl of
            Just l ->
                Html.label [] [ Html.text (labelText l mandatory) ]

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
            if attributes.common.mandatory && attributes.common.value /= Nothing then
                False
            else
                attributes.common.hidden

        inputType =
            if isHidden then
                Hidden
            else
                attributes.common.type_ |> withDefault Text

        val =
            attributes.common.value |> withDefault ""

        id =
            Html.Attributes.id (attributes.common.id |> withDefault "")

        name =
            case inputName attributes.common.objectName attributes.common.fieldName of
                Nothing ->
                    []

                Just inputName_ ->
                    [ Html.Attributes.name inputName_ ]

        attrs =
            List.concat
                [ [ Html.Attributes.required attributes.common.mandatory
                  , id
                  , Html.Attributes.placeholder
                        (if val == "" then
                            (attributes.common.placeholder |> withDefault "")
                         else
                            ""
                        )
                  ]
                , name
                ]

        finalAttrs =
            List.concat
                [ case attributes.common.onInput of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onInput event ]
                , case attributes.common.onBlur of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onBlur event ]
                , case attributes.common.onFocus of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onFocus event ]
                , case attributes.common.onChange of
                    Nothing ->
                        []

                    Just msg ->
                        [ Html.Events.on "change" (Json.succeed msg) ]
                , attrs
                ]
    in
        case view of
            Nothing ->
                case attributes.common.options of
                    Nothing ->
                        case inputType of
                            TextArea ->
                                Html.textarea finalAttrs [ Html.text val ]

                            _ ->
                                input inputType finalAttrs val

                    Just options ->
                        select name val options

            Just v ->
                v attributes finalAttrs name val


{-| Generates a generic object, ready to be renderd by FormBuilder. Accepts attributes, view and modifiers. -}
object : FieldAttributes a msg -> Maybe (FieldView a msg) -> List (AttributesModifier a msg) -> Html msg
object defaultAttributes view customModifiers =
    let
        attributes =
            defaultAttributes |> compose customModifiers

        isMandatory =
            attributes.common.mandatory

        isHidden =
            if isMandatory && attributes.common.value /= Nothing then
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

{-| Generates a default input field. Generates default attributes, use the default view, and render the field with their attributes. -}
default : List (AttributesModifier {} msg) -> Html msg
default =
    object Attributes.defaultAttributes Nothing

{-| Generates a default hidden input field. Generates default attributes, and force the field to be hidden. -}
defaultHidden : List (AttributesModifier {} msg) -> Html msg
defaultHidden =
    object (Attributes.hidden Attributes.defaultAttributes) Nothing



-- Not exposed


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
