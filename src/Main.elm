module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, column, fill, height, spacing, width)
import Email
import Model exposing (Model, Msg(..))
import Video


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { email = Email.initialModel
      , videoState = Video.initialModel
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Video.uploadProgress model.videoState
        |> Sub.map VideoMsg


view : Model -> Document Msg
view model =
    { title = "Video Stabilizer"
    , body =
        [ Element.layout [] <|
            column
                [ width fill
                , height fill
                , spacing 16
                ]
                [ Email.view model.email |> Element.map EmailMsg
                , videoView model
                ]
        ]
    }


videoView : Model -> Element Msg
videoView model =
    if Email.isValid model.email then
        Video.view model.videoState |> Element.map VideoMsg

    else
        Video.disabledView |> Element.map VideoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailMsg emailMsg ->
            ( { model | email = Email.update emailMsg model.email }, Cmd.none )

        VideoMsg videoMsg ->
            let
                ( vidState, cmd ) =
                    Video.update
                        videoMsg
                        model.videoState
                        (Email.emailAddress model.email)
            in
            ( { model | videoState = vidState }, Cmd.map VideoMsg cmd )
