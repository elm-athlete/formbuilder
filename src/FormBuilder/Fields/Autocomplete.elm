module FormBuilder.Fields.Autocomplete exposing (..)

import List.Extra


changeSelectedElement : { b | selectedElement : Maybe Int, elements : List a } -> Int -> { b | selectedElement : Maybe Int, elements : List a }
changeSelectedElement autocompleteModel offset =
    let
        elementsSize =
            autocompleteModel.elements |> List.length
    in
        { autocompleteModel
            | selectedElement =
                if elementsSize == 0 then
                    Nothing
                else
                    Just
                        (case autocompleteModel.selectedElement of
                            Nothing ->
                                if offset == 1 then
                                    0
                                else
                                    elementsSize - 1

                            Just selection ->
                                if selection == 0 && offset == -1 then
                                    0
                                else if selection == (elementsSize - 1) && offset == 1 then
                                    elementsSize - 1
                                else
                                    selection + offset
                        )
        }


selectElement : a -> { b | selectedElement : Maybe Int, elements : List a } -> { b | selectedElement : Maybe Int, elements : List a }
selectElement element autocompleteModel =
    { autocompleteModel | selectedElement = List.Extra.elemIndex element autocompleteModel.elements }


selectedElement : { b | selectedElement : Maybe Int, elements : List a } -> Maybe a
selectedElement autocompleteModel =
    case autocompleteModel.selectedElement of
        Nothing ->
            Nothing

        Just index ->
            autocompleteModel.elements |> List.Extra.getAt index


deselectElement : { b | selectedElement : Maybe Int } -> { b | selectedElement : Maybe Int }
deselectElement autocompleteModel =
    { autocompleteModel | selectedElement = Nothing }
