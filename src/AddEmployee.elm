module AddEmployee exposing (..)

import Browser
import Http
import HttpError exposing (errorToString)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Employee =
    { id: Int
    , name: String
    , email: String
    , phone: Int
    }

type alias FormEmployee =
    { name: String
    , email: String
    , phone: Maybe Int
    }

type alias FModel =
    { eName: String
    , eEmail: String
    , ePhone: String
    , returnedEm: Employee
    , failure: String
    }

type AddEMessage
        = EName String
        | EEmail String
        | EPhone String
        | AddEmployee FormEmployee
        | ReturnedEm (Result Http.Error Employee)

init : () -> (FModel, Cmd AddEMessage)
init _ = (FModel "" "" "" (Employee 0 "" "" 0) "", Cmd.none)

update : AddEMessage -> FModel -> (FModel, Cmd AddEMessage)
update message model =
    case message of
        EName name ->
            ({model | eName = name}, Cmd.none)

        EEmail email ->
            ({model | eEmail = email}, Cmd.none)

        EPhone phone ->
            ({model | ePhone = phone}, Cmd.none)

        AddEmployee employee ->
            (model, addEmployee employee)

        ReturnedEm result ->
            case result of
                Ok employee -> ({model | returnedEm = employee}, Cmd.none)
                Err error -> ({model | failure = errorToString error}, Cmd.none)


view : FModel -> Html AddEMessage
view model =
    div []
    [ viewInput "text" "Name" model.eName EName
    , viewInput "text" "Email" model.eEmail EEmail
    , viewInput "number" "Phone number" model.ePhone EPhone
    , button[ onClick (AddEmployee (FormEmployee model.eName model.eEmail (String.toInt model.ePhone)))][text "Submit"]
    ]

addEmployee : FormEmployee -> Cmd AddEMessage
addEmployee employee = Http.post
    { url = "http://localhost:8080/startcode-ca3/api/employees"
    , body = Http.jsonBody (encodeEmployee (Employee 0 employee.name employee.email (validatePhone employee.phone)))
    , expect = Http.expectJson ReturnedEm decodeEmployee
    }

validatePhone : Maybe Int -> Int
validatePhone phone =
    case phone of
        Just pNumber -> pNumber
        Nothing -> 0

viewInput : String -> String -> String -> (String -> AddEMessage) -> Html AddEMessage
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

decodeEmployee: Decode.Decoder Employee
decodeEmployee =
       Decode.map4 Employee
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

encodeEmployee : Employee -> Encode.Value
encodeEmployee employee=
    Encode.object
        [("name", Encode.string employee.name)
        ,("email", Encode.string employee.email)
        ,("phone", Encode.int employee.phone)]

subscriptions : FModel -> Sub AddEMessage
subscriptions _ = Sub.none