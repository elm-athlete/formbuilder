module FormBuilder.Fields.Autocomplete.Type exposing (..)


type alias AutocompleteAttributes a msg =
    { choices : Maybe (List ( String, List a ))
    , choiceView : Maybe (a -> String)
    , onSelect : Maybe (a -> msg)
    , searchQuery : Maybe String
    , focused : Bool
    , selectedElement : Maybe a
    }
