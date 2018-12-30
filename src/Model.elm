module Model exposing (Model, Msg(..), envDecoder)

import Email
import Http
import Json.Decode as Decode
import Video


type alias Model =
    { env : { apiUrl : String }
    , email : Email.Model
    , videoState : Video.Model
    }


type Msg
    = EmailMsg Email.Msg
    | VideoMsg Video.Msg


envDecoder : Decode.Decoder { apiUrl : String }
envDecoder =
    Decode.map
        (\apiUrl -> { apiUrl = apiUrl })
        Decode.string
