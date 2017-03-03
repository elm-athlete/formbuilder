module FormBuilder.FormBuilder exposing (new)

{-| Create a new form by using differents fields. The form is modular and accepts a list of Html msg, allowing everyone to add custom fields.

# Creation
@docs new
-}

import Html exposing (Html)
import Html.Attributes
import FormBuilder.FieldBuilder as FieldBuilder
import FormBuilder.FieldBuilder.Attributes as Attributes


pluralize : String -> String
pluralize name =
    name ++ "s"


{-| Creates and render a new form. Should be used in main views.
-}
new : String -> String -> List (Html msg) -> Html msg
new recordName csrfToken fields =
    Html.form
        [ Html.Attributes.action ("/" ++ (pluralize recordName))
        , Html.Attributes.method "post"
        , Html.Attributes.enctype "multipart/form-data"
        ]
    <|
        List.concat
            [ [ FieldBuilder.defaultHidden
                    [ Attributes.type_ Attributes.Hidden
                    , Attributes.fieldName "authenticity_token"
                    , Attributes.value csrfToken
                    ]
              ]
            , fields
            ]
