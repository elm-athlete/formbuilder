module FormBuilder.FormBuilder exposing (..)

import Html exposing (Html)
import Html.Attributes
import FormBuilder.FieldBuilder as FieldBuilder


pluralize : String -> String
pluralize name =
    name ++ "s"


new : String -> String -> List (Html msg) -> Html msg
new recordName csrfToken fields =
    Html.form
        [ Html.Attributes.action ("/" ++ (pluralize recordName))
        , Html.Attributes.method "post"
        , Html.Attributes.enctype "multipart/form-data"
        ]
        ((FieldBuilder.formField "authenticity_token" csrfToken)
            :: fields
        )
