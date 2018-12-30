module Email exposing (Model, Msg, emailAddress, initialModel, isValid, update, view)

import Element exposing (Element, centerX, centerY, minimum, px, text, width)
import Element.Events as Event
import Element.Input as Input
import Regex exposing (Regex)



-- Types


type Email
    = Email String


type Msg
    = TypeEmail String
    | OnLostFocus


type Model
    = Value Email String



-- Constants


initialModel : Model
initialModel =
    Value (Email "") ""


validEmailRegex : Regex
validEmailRegex =
    Regex.fromString "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
        |> Maybe.withDefault Regex.never



-- Update


update : Msg -> Model -> Model
update msg (Value email error) =
    case msg of
        TypeEmail emailStr ->
            Value (Email emailStr) ""

        OnLostFocus ->
            Value email "Please enter a valid email address"



-- Utils


isValid : Model -> Bool
isValid (Value (Email emailStr) _) =
    Regex.contains validEmailRegex emailStr
        -- AWS has a bug with presigned URLs that have tags with plus signs..
        && not (String.contains "+" emailStr)


emailAddress : Model -> String
emailAddress (Value (Email emailStr) _) =
    emailStr



-- Views


view : Model -> Element Msg
view ((Value (Email emailStr) error) as model) =
    let
        label =
            if isValid model then
                Input.labelHidden "Email address"

            else
                Input.labelBelow [] (text error)
    in
    Input.text
        [ centerX
        , centerY
        , width <| px 370
        , Event.onLoseFocus OnLostFocus
        ]
        { onChange = TypeEmail
        , text = emailStr
        , placeholder = Just (Input.placeholder [] (text "email-for-download-link@example.com"))
        , label = label
        }
