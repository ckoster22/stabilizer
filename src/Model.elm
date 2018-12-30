module Model exposing (Model, Msg(..))

import Email
import Http
import Json.Decode as Decode
import Video


type alias Model =
    { email : Email.Model
    , videoState : Video.Model
    }


type Msg
    = EmailMsg Email.Msg
    | VideoMsg Video.Msg
