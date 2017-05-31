module FormBuilder exposing (new)

{-| Create a new form by using differents fields. The form is modular and accepts a list of Html msg, allowing everyone to add custom fields.


# Creation

@docs new

-}

import Html exposing (Html)
import Html.Attributes
import FormBuilder.FieldBuilder as FieldBuilder
import FormBuilder.FieldBuilder.Attributes as Attributes
import Helpers.ActiveRecord exposing (isNew)


pluralize : String -> String
pluralize name =
    name ++ "s"


url : String -> Maybe a -> String
url recordName id =
    "/"
        ++ (pluralize recordName)
        ++ (case id of
                Nothing ->
                    ""

                Just id_ ->
                    ("/" ++ (toString id_))
           )


{-| Creates and render a new form. Should be used in main views.
-}
new : String -> { a | id : Maybe Int } -> String -> List (Html msg) -> Html msg
new recordName ({ id } as element) csrfToken fields =
    Html.form
        [ Html.Attributes.action (url recordName id)
        , Html.Attributes.method "post"
        , Html.Attributes.enctype "multipart/form-data"
        ]
    <|
        List.concat
            [ [ FieldBuilder.defaultHidden
                    [ Attributes.fieldName "authenticity_token"
                    , Attributes.value csrfToken
                    ]
              , if isNew element then
                    Html.text ""
                else
                    FieldBuilder.defaultHidden
                        [ Attributes.fieldName "_method"
                        , Attributes.value "put"
                        ]
              ]
            , fields
            ]
