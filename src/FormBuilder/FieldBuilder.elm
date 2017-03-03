module FormBuilder.FieldBuilder exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe exposing (andThen, withDefault)
import Json.Decode as Json


type InputType
    = Hidden
    | TextArea
    | Text
    | File


type alias Event msg =
    String -> msg


type alias FieldView a msg =
    FormAttributes a msg -> List (Html.Attribute msg) -> Html.Attribute msg -> String -> Html msg


type alias AttributesModifier a msg =
    FormAttributes a msg -> FormAttributes a msg


type alias FormAttributes sub msg =
    { sub
        | value : Maybe String
        , id : Maybe String
        , type_ : Maybe InputType
        , label : Maybe String
        , placeholder : Maybe String
        , mandatory : Maybe Bool
        , hidden : Maybe Bool
        , options : Maybe (List ( String, Int ))
        , event : Maybe (Event msg)
        , noBottomPadding : Maybe Bool
        , onFocus : Maybe (Event msg)
        , onBlur : Maybe (Event msg)
        , onChange : Maybe msg
    }


defaultFieldAttributes : FormAttributes {} msg
defaultFieldAttributes =
    { value = Nothing
    , id = Nothing
    , type_ = Nothing
    , label = Nothing
    , placeholder = Nothing
    , mandatory = Nothing
    , hidden = Nothing
    , options = Nothing
    , event = Nothing
    , noBottomPadding = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onChange = Nothing
    }


selectOption : { parent | name : String, id : Int } -> Html msg
selectOption { name, id } =
    Html.option [ Html.Attributes.value (toString id) ] [ Html.text name ]


inputName : String -> String -> String
inputName objectName attributeName =
    objectName ++ "[" ++ attributeName ++ "]"


formField : String -> String -> Html msg
formField attributeName val =
    Html.input
        [ Html.Attributes.type_ "hidden"
        , Html.Attributes.name attributeName
        , Html.Attributes.value val
        ]
        []


hiddenObjectField : String -> String -> FormAttributes a msg -> Html msg
hiddenObjectField objectName attributeName attributes =
    Html.input
        [ Html.Attributes.type_ "hidden"
        , Html.Attributes.name (inputName objectName attributeName)
        , Html.Attributes.value (attributes.value |> withDefault "")
        ]
        []


blank : String -> Bool
blank str =
    str == ""


notBlank : String -> Bool
notBlank str =
    not (blank str)


label : String -> FormAttributes a msg -> FormAttributes a msg
label l formAttributes =
    { formAttributes | label = Just l }


noBottomPadding : FormAttributes a msg -> FormAttributes a msg
noBottomPadding formAttributes =
    { formAttributes | noBottomPadding = Just True }


value : String -> FormAttributes a msg -> FormAttributes a msg
value v formAttributes =
    { formAttributes | value = Just v }


id : String -> FormAttributes a msg -> FormAttributes a msg
id id formAttributes =
    { formAttributes | id = Just id }


type_ : InputType -> FormAttributes a msg -> FormAttributes a msg
type_ value formAttributes =
    { formAttributes | type_ = Just value }


mandatory : FormAttributes a msg -> FormAttributes a msg
mandatory formAttributes =
    { formAttributes | mandatory = Just True }


placeholder : String -> FormAttributes a msg -> FormAttributes a msg
placeholder value formAttributes =
    { formAttributes | placeholder = Just value }


hidden : FormAttributes a msg -> FormAttributes a msg
hidden formAttributes =
    { formAttributes | hidden = Just True }


options : List ( String, Int ) -> FormAttributes a msg -> FormAttributes a msg
options options formAttributes =
    { formAttributes | options = Just options }


onInput : Event msg -> FormAttributes a msg -> FormAttributes a msg
onInput event formAttributes =
    { formAttributes | event = Just event }


onFocus : Event msg -> FormAttributes a msg -> FormAttributes a msg
onFocus event formAttributes =
    { formAttributes | onFocus = Just event }


onBlur : Event msg -> FormAttributes a msg -> FormAttributes a msg
onBlur event formAttributes =
    { formAttributes | onBlur = Just event }


onChange : msg -> FormAttributes a msg -> FormAttributes a msg
onChange msg formAttributes =
    { formAttributes | onChange = Just msg }


inputTypeToString : InputType -> String
inputTypeToString inputType =
    case inputType of
        Hidden ->
            "hidden"

        File ->
            "file"

        _ ->
            "text"


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


selectField : Html.Attribute msg -> String -> List ( String, Int ) -> Html msg
selectField name selected options =
    Html.select [ name ] (options |> List.map (option selected))


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


labelField : Maybe String -> Bool -> Bool -> Html msg
labelField text isHidden mandatory =
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


inputField : InputType -> List (Html.Attribute msg) -> String -> Html msg
inputField inputType attrs val =
    Html.input
        (Html.Attributes.type_ (inputTypeToString inputType)
            :: (if inputType == File then
                    attrs
                else
                    (Html.Attributes.value val) :: attrs
               )
        )
        []


genericInputField : String -> String -> FormAttributes a msg -> Bool -> Bool -> Maybe (FieldView a msg) -> Html msg
genericInputField objectName fieldName attributes isMandatory isHidden view =
    let
        inputType =
            if isHidden then
                Hidden
            else
                attributes.type_ |> withDefault Text

        val =
            attributes.value |> withDefault ""

        id =
            Html.Attributes.id (attributes.id |> withDefault "")

        name =
            Html.Attributes.name (inputName objectName fieldName)

        attrs =
            [ Html.Attributes.required isMandatory
            , name
            , id
            , Html.Attributes.placeholder
                (if val == "" then
                    (attributes.placeholder |> withDefault "")
                 else
                    ""
                )
            ]

        finalAttrs =
            List.concat
                [ case attributes.event of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onInput event ]
                , case attributes.onBlur of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onBlur (event objectName) ]
                , case attributes.onFocus of
                    Nothing ->
                        []

                    Just event ->
                        [ Html.Events.onFocus (event objectName) ]
                , case attributes.onChange of
                    Nothing -> []

                    Just msg ->
                        [ Html.Events.on "change" (Json.succeed msg) ]
                , attrs
                ]
    in
        case view of
            Nothing ->
                case attributes.options of
                    Nothing ->
                        case inputType of
                            TextArea ->
                                Html.textarea finalAttrs [ Html.text val ]

                            _ ->
                                inputField inputType finalAttrs val

                    Just options ->
                        selectField name val options

            Just v ->
                v attributes finalAttrs name val


compose : List (a -> a) -> (a -> a)
compose modifiers =
    modifiers
        |> List.foldr (<<) identity


objectField : String -> FormAttributes a msg -> Maybe (FieldView a msg) -> String -> List (AttributesModifier a msg) -> Html msg
objectField objectName default_attributes view fieldName custom_modifiers =
    let
        attributes =
            default_attributes |> compose custom_modifiers

        isMandatory =
            attributes.mandatory |> withDefault False

        isHidden =
            if isMandatory && (attributes.value |> withDefault "" |> blank) then
                False
            else
                attributes.hidden |> withDefault False

        noBottomPadding =
            attributes.noBottomPadding |> withDefault False
    in
        Html.div
            [ Html.Attributes.class
                (if isHidden || noBottomPadding then
                    ""
                 else
                    "pb"
                )
            ]
            [ labelField attributes.label isHidden isMandatory
            , genericInputField objectName fieldName attributes isMandatory isHidden view
            ]


sendUpdateMsg : (a -> msg) -> (String -> a) -> String -> msg
sendUpdateMsg type_ field variable =
    type_ (field variable)


onStandardFieldChange : (a -> msg) -> (String -> a) -> FormAttributes b msg -> FormAttributes b msg
onStandardFieldChange type_ =
    onInput << (sendUpdateMsg type_)


defaultInputField : String -> Maybe (FieldView {} msg) -> String -> List (AttributesModifier {} msg) -> Html msg
defaultInputField recordName view =
    objectField recordName defaultFieldAttributes Nothing


defaultInputHiddenField : String -> Maybe (FieldView {} msg) -> String -> List (AttributesModifier {} msg) -> Html msg
defaultInputHiddenField recordName view =
    objectField recordName (hidden defaultFieldAttributes) Nothing
