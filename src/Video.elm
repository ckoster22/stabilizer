module Video exposing (Model, Msg, disabledView, initialModel, update, uploadProgress, view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, rgb, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Environment
import File exposing (File)
import File.Select as Select
import Http exposing (Error(..), Progress(..))
import Url.Builder as UrlBuilder



-- Types


type Video
    = Video File


type Msg
    = OnFileSelected Video
    | OnInvalidFileSelected
    | UploadButtonClick
    | PresignedUrlResult Video (Result Http.Error String)
    | FileUploadResult Video (Result Http.Error ())
    | FileUploadProgress Video Http.Progress


type Model
    = NoVideo
    | NoVideoError
    | Selected Video AsyncState


type AsyncState
    = RetrievingUrl
    | UrlRetrieved String
    | Uploading Int Int
    | UploadSuccess
    | AsyncError Http.Error


type Status
    = Enabled
    | Disabled



-- Constants


fiftyMB : Int
fiftyMB =
    1024 * 1024 * 50


uploadTracker : String
uploadTracker =
    "fileupload"


initialModel =
    NoVideo



-- Update


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model email =
    case msg of
        UploadButtonClick ->
            ( model, select )

        OnFileSelected video ->
            ( Selected video RetrievingUrl, retrieveSignedUrl video email )

        OnInvalidFileSelected ->
            ( NoVideoError, Cmd.none )

        PresignedUrlResult video (Ok url) ->
            ( Selected video (Uploading 0 0), upload video url email )

        PresignedUrlResult video (Err httpError) ->
            ( Selected video (AsyncError httpError), Cmd.none )

        FileUploadResult video (Ok ()) ->
            ( Selected video UploadSuccess, Cmd.none )

        FileUploadResult video (Err httpError) ->
            ( Selected video (AsyncError httpError), Cmd.none )

        FileUploadProgress video (Sending { sent, size }) ->
            ( Selected video (Uploading sent size), Cmd.none )

        FileUploadProgress _ _ ->
            ( model, Cmd.none )



-- Commands


select : Cmd Msg
select =
    Select.file [ "video/mp4" ] validateSelectedFile


retrieveSignedUrl : Video -> String -> Cmd Msg
retrieveSignedUrl video email =
    Http.get
        { url =
            Environment.apiUrl
                ++ UrlBuilder.toQuery
                    [ UrlBuilder.string "name" (name video)
                    , UrlBuilder.string "email" email
                    ]
        , expect = Http.expectString (PresignedUrlResult video)
        }


upload : Video -> String -> String -> Cmd Msg
upload ((Video file) as video) url email =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Content-Type" "video/mp4"
            , Http.header "x-amz-tagging" ("email=" ++ email)
            ]
        , url = url
        , body = Http.fileBody file
        , expect = Http.expectWhatever (FileUploadResult video)
        , timeout = Just 600000 -- 10 minutes
        , tracker = Just uploadTracker
        }



-- Subscriptions


uploadProgress : Model -> Sub Msg
uploadProgress model =
    case model of
        Selected video _ ->
            Http.track uploadTracker (FileUploadProgress video)

        _ ->
            Sub.none



-- Utils


validateSelectedFile : File -> Msg
validateSelectedFile file =
    if File.size file > fiftyMB then
        OnInvalidFileSelected

    else
        OnFileSelected (Video file)


name : Video -> String
name (Video file) =
    File.name file


uploadProgressPercent : Int -> Int -> String
uploadProgressPercent sent size =
    if size == 0 then
        "0%"

    else
        let
            numStr =
                ((toFloat sent / toFloat size) * 1000)
                    |> floor
                    |> (\num -> toFloat num / 10)
                    |> String.fromFloat
        in
        numStr ++ "%"



-- View


view : Model -> Element Msg
view model =
    case model of
        Selected _ (AsyncError (BadUrl message)) ->
            text message

        Selected _ (AsyncError Timeout) ->
            text "The request timed out"

        Selected _ (AsyncError NetworkError) ->
            text "There was problem communicating with the network"

        Selected _ (AsyncError (BadStatus statusCode)) ->
            text <| "Bad request: " ++ String.fromInt statusCode

        Selected _ (AsyncError (BadBody message)) ->
            text <| "Bad request: " ++ message

        Selected video (Uploading sent size) ->
            uploadingView video sent size

        NoVideoError ->
            uploadView Enabled True

        _ ->
            uploadView Enabled False


disabledView : Element Msg
disabledView =
    uploadView Disabled False


uploadView : Status -> Bool -> Element Msg
uploadView status showError =
    let
        onPress =
            case status of
                Enabled ->
                    Just UploadButtonClick

                Disabled ->
                    Nothing

        labelText =
            if showError then
                "Invalid file. Upload a video (50MB max)"

            else
                "Upload a video (50MB max)"
    in
    Input.button
        [ centerX
        , centerY
        , padding 30
        , Border.dashed
        , Border.rounded 5
        , Border.width 1
        , Border.color (rgb 0.7 0.7 0.7)
        , Font.color (rgb 0.4 0.4 0.4)
        ]
        { onPress = onPress
        , label = text labelText
        }


uploadingView : Video -> Int -> Int -> Element Msg
uploadingView video sent size =
    el
        [ centerX
        , centerY
        ]
        (column
            [ Font.color (rgb 0.7 0.7 0.7) ]
            [ text <| "Uploading \"" ++ name video ++ "\""
            , text <| "Progress: " ++ uploadProgressPercent sent size
            ]
        )
