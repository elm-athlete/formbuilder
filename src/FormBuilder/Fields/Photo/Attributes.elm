module FormBuilder.Fields.Photo.Attributes exposing (..)


type alias PhotoAttributes =
    { photos : Maybe (List String)
    , selectedPhotoId : Maybe String
    }
