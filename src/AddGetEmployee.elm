module AddGetEmployee exposing (..)

import Browser
import Http
import HttpError exposing (errorToString)
import Html exposing (Html, br, button, div, h1, input, p, text)
import Html.Attributes exposing (placeholder, style, type_, value)
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

type alias EmpFormModel =
    { eId: String
    , eName: String
    , eEmail: String
    , ePhone: String
    , returnedEm: Employee
    , failure: String
    }

type EmpFormMessage
        = EId String
        | EName String
        | EEmail String
        | EPhone String
        | AddEmployee FormEmployee
        | GetEmployee String
        | ReturnedEm (Result Http.Error Employee)

init : () -> (EmpFormModel, Cmd EmpFormMessage)
init _ = (EmpFormModel "" "" "" "" (Employee 0 "" "" 0) "", Cmd.none)

update : EmpFormMessage -> EmpFormModel -> (EmpFormModel, Cmd EmpFormMessage)
update message model =
    case message of
        EId id ->
            ({model | eId = id}, Cmd.none)

        EName name ->
            ({model | eName = name}, Cmd.none)

        EEmail email ->
            ({model | eEmail = email}, Cmd.none)

        EPhone phone ->
            ({model | ePhone = phone}, Cmd.none)

        AddEmployee employee ->
            ({model | failure = ""}, addEmployee employee)

        GetEmployee id ->
            ({model | failure = ""}, getEmployee id)

        ReturnedEm result ->
            case result of
                Ok employee -> ({model | returnedEm = employee}, Cmd.none)
                Err error -> ({model | failure = errorToString error}, Cmd.none)


view : EmpFormModel -> Html EmpFormMessage
view model =
    div [style "text-align" "center", style "margin-top" "200px"]
    [ h1 [] [text "Add employee"]
    , viewInput "text" "Name" model.eName EName
    , viewInput "text" "Email" model.eEmail EEmail
    , viewInput "number" "Phone number" model.ePhone EPhone
    , button[ onClick (AddEmployee
        (FormEmployee model.eName model.eEmail (String.toInt model.ePhone)))][text "Add"]

    , h1 [] [text "Get employee by ID"]
    , viewInput "number" "Id" model.eId EId
    , br [] []
    , button [onClick (GetEmployee model.eId)] [text "Search"]
    , showEmployee model.returnedEm
    , p [style "color" "red"] [text model.failure]
    ]

viewInput : String -> String -> String -> (String -> EmpFormMessage) -> Html EmpFormMessage
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

addEmployee : FormEmployee -> Cmd EmpFormMessage
addEmployee employee = Http.post
    { url = "http://localhost:8080/startcode-ca3/api/employees"
    , body = Http.jsonBody
        (employeeEncoder (Employee 0 employee.name employee.email (validatePhone employee.phone)))
    , expect = Http.expectJson ReturnedEm employeeDecoder
    }

getEmployee : String -> Cmd EmpFormMessage
getEmployee id = Http.get
    { url = "http://localhost:8080/startcode-ca3/api/employees/" ++ id
    , expect = Http.expectJson ReturnedEm employeeDecoder
    }

validatePhone : Maybe Int -> Int
validatePhone phone =
    case phone of
        Just pNumber -> pNumber
        Nothing -> 0

showEmployee : Employee -> Html EmpFormMessage
showEmployee employee =
    if employee.name /= "" then
    p []
    [ text ("ID: " ++ (String.fromInt employee.id))
    , br [] []
    , text ("Name: " ++ employee.name)
    , br [] []
    , text ("Email: " ++ employee.email)
    , br [] []
    , text ("Phone: " ++ String.fromInt employee.phone)
    ]
    else
    p [] [text ""]

employeeDecoder: Decode.Decoder Employee
employeeDecoder =
       Decode.map4 Employee
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

employeeEncoder : Employee -> Encode.Value
employeeEncoder employee =
    Encode.object
        [("name", Encode.string employee.name)
        ,("email", Encode.string employee.email)
        ,("phone", Encode.int employee.phone)]

subscriptions : EmpFormModel -> Sub EmpFormMessage
subscriptions _ = Sub.none