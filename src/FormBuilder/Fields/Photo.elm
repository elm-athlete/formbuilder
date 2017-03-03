module FormBuilder.Fields.Photo exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import FormBuilder.FieldBuilder as FieldBuilder exposing (FormAttributes, FieldView, AttributesModifier)
import Maybe exposing (andThen, withDefault)


type alias PhotoAttributes =
    { photos : Maybe (List String)
    , selectedPhotoId : Maybe String
    }


defaultFieldAttributes : FormAttributes PhotoAttributes msg
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
    , photos = Nothing
    , selectedPhotoId = Nothing
    , onFocus = Nothing
    , onBlur = Nothing
    , onChange = Nothing
    }


selection : Maybe (List String) -> Maybe String -> (String -> msg) -> FormAttributes PhotoAttributes msg -> FormAttributes PhotoAttributes msg
selection photos selectedPhotoId event formAttributes =
    { formAttributes
        | photos = photos
        , selectedPhotoId = selectedPhotoId
        , event = Just event
    }


selectableImage : String -> String -> (String -> msg) -> Html msg
selectableImage google_source selected_google_source event =
    let
        selected =
            google_source == selected_google_source
    in
        Html.div
            [ Html.Attributes.style
                [ ( "opacity"
                  , if selected then
                        "1"
                    else
                        "0.5"
                  )
                , ( "display", "inline-block" )
                ]
            , Html.Events.onClick (event google_source)
            ]
            [ Html.img
                [ Html.Attributes.style [ ( "cursor", "pointer" ) ]
                , Html.Attributes.width 200
                , Html.Attributes.src google_source
                ]
                []
            ]


photoEventField : Maybe (List String) -> Maybe String -> Maybe (String -> msg) -> Html msg
photoEventField photos primarySelectedPhotoId event =
    case event of
        Just e ->
            Html.div
                []
                [ if List.isEmpty (photos |> withDefault []) then
                    Html.text ""
                  else
                    Html.div
                        [ Html.Attributes.style
                            [ ( "overflow-x", "auto" )
                            , ( "white-space", "nowrap" )
                            ]
                        ]
                        (photos
                            |> withDefault []
                            |> List.map (\element -> selectableImage element (primarySelectedPhotoId |> withDefault "") e)
                        )
                ]

        Nothing ->
            Html.text ""


inputField : FieldView PhotoAttributes msg
inputField attributes commonAttrs name val =
    Html.div
        []
        [ photoEventField attributes.photos attributes.selectedPhotoId attributes.event
        , FieldBuilder.inputField FieldBuilder.Hidden (name :: commonAttrs) val
        ]


defaultPhotoField : String -> Maybe (FieldView PhotoAttributes msg) -> String -> List (AttributesModifier PhotoAttributes msg) -> Html msg
defaultPhotoField recordName view =
    FieldBuilder.objectField recordName defaultFieldAttributes view
