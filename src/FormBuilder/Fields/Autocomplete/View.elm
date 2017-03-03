module FormBuilder.Fields.Autocomplete.View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import FormBuilder.FieldBuilder as FieldBuilder exposing (FormAttributes, FieldView, AttributesModifier)
import FormBuilder.Fields.Autocomplete.Type exposing (AutocompleteAttributes)
import FormBuilder.Fields.Autocomplete.Attributes exposing (..)


defaultField : String -> Maybe (FieldView (AutocompleteAttributes a msg) msg) -> String -> List (AttributesModifier (AutocompleteAttributes a msg) msg) -> Html msg
defaultField recordName view =
    FieldBuilder.objectField recordName defaultFieldAttributes view


choicesView : List a -> (a -> String) -> (a -> msg) -> Maybe a -> List (Html msg)
choicesView choices viewFun onSelect selected =
    choices |> List.map (elementView viewFun onSelect selected)


elementView : (a -> String) -> (a -> msg) -> Maybe a -> a -> Html msg
elementView viewFun onSelect selected choices =
    let
        description =
            viewFun choices
    in
        Html.a
            [ Html.Attributes.style
                (List.append
                    [ ( "cursor", "pointer" )
                    , ( "padding", "3px" )
                    , ( "display", "block" )
                    , ( "width", "100%" )
                    , ( "padding-left", "12px" )
                    ]
                    (if selected == Just choices then
                        [ ( "background-color", "#28DBD2" )
                        , ( "color", "#fff" )
                        ]
                     else
                        []
                    )
                )
            , Html.Events.onMouseEnter
                (onSelect choices)
            ]
            [ Html.text (description) ]


inputField : FieldView (AutocompleteAttributes a msg) msg
inputField attributes commonAttrs name val =
    case attributes.choiceView of
        Nothing ->
            Html.text ""

        Just choiceView ->
            case attributes.onSelect of
                Nothing ->
                    Html.text ""

                Just click ->
                    case attributes.searchQuery of
                        Nothing ->
                            Html.text ""

                        Just text ->
                            display
                                (case attributes.choices of
                                    Nothing ->
                                        []

                                    Just choices ->
                                        choices
                                )
                                choiceView
                                click
                                text
                                attributes
                                commonAttrs


display : List ( String, List a ) -> (a -> String) -> (a -> msg) -> String -> FormAttributes (AutocompleteAttributes a msg) msg -> List (Html.Attribute msg) -> Html msg
display choices choiceView onSelect searchQuery attributes commonAttrs =
    case attributes.event of
        Just event ->
            case attributes.placeholder of
                Nothing ->
                    Html.text ""

                Just placeholder ->
                    Html.div
                        [ Html.Attributes.style
                            [ ( "position", "relative" )
                            ]
                        ]
                        [ FieldBuilder.inputField FieldBuilder.Text
                            (List.append
                                [ Html.Attributes.placeholder placeholder
                                , Html.Events.onInput event
                                , Html.Attributes.autocomplete False
                                ]
                                commonAttrs
                            )
                            searchQuery
                        , if (searchQuery |> String.length) < 5 || not attributes.focused then
                            Html.text ""
                          else
                            Html.div
                                [ Html.Attributes.style
                                    [ ( "border-bottom-left-radius", "7px" )
                                    , ( "border-bottom-right-radius", "7px" )
                                    , ( "overflow", "hidden" )
                                    , ( "border", "1px solid #ddd" )
                                    , ( "border-top", "none" )
                                    , ( "position", "absolute" )
                                    , ( "margin-top", "-2px" )
                                    , ( "width", "100%" )
                                    , ( "background-color", "#ffffff" )
                                    ]
                                ]
                                (choices |> List.map (placeLabel choiceView onSelect attributes.selectedElement))
                        ]

        Nothing ->
            Html.text ""


placeLabel : (a -> String) -> (a -> msg) -> Maybe a -> ( String, List a ) -> Html msg
placeLabel viewFun onSelect selected choices =
    Html.div []
        (if (List.length (Tuple.second choices)) == 0 then
            []
         else
            (List.append
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "padding", "2px" )
                        , ( "padding-left", "6px" )
                        , ( "display", "block" )
                        , ( "width", "100%" )
                        , ( "background-color", "#fbfbfb" )
                        , ( "font-size", "80%" )
                        , ( "border-bottom", "1px solid #ddd" )
                        , ( "border-top", "1px solid #ddd" )
                        , ( "color", "#888" )
                        ]
                    ]
                    [ Html.text (Tuple.first choices) ]
                ]
                (choicesView (Tuple.second choices) viewFun onSelect selected)
            )
        )
