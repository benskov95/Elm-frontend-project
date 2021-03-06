module EmployeeTable exposing (..)

import Browser
import Utils exposing (Employee, FormEmployee, employeeDecoder, employeeEncoder, employeeListDecoder, tableStyle, trStyle)
import Html exposing (Attribute, Html, br, button, div, h1, input, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import HttpError exposing (errorToString)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL--

type EmpListModel
    = Waiting String
    | Loading
    | Success (List Employee)
    | Edit FormEmployee

type EmpListMessage
    = TryAgain
    | EmployeeResult (Result Http.Error (List Employee))
    | DeleteEmployee Int
    | DeleteResult (Result Http.Error Employee)
    | EditEmpName String
    | EditEmpEmail String
    | EditEmpPhone String
    | EditEmployee Employee
    | EditSave Employee
    | EditEmployeeResult (Result Http.Error Employee)

init : () -> ( EmpListModel, Cmd EmpListMessage )
init _ = ( Loading, getEmployees )

update : EmpListMessage -> EmpListModel -> ( EmpListModel, Cmd EmpListMessage )
update message model =
    case message of
        TryAgain ->
            ( Loading, getEmployees )

        EmployeeResult result ->
            case result of
                Ok employees ->
                    ( Success employees, Cmd.none )
                Err error ->
                    ( Waiting (errorToString error), Cmd.none )

        DeleteEmployee id ->
            ( Loading, deleteEmployee id )

        DeleteResult result ->
            case result of
                Ok _ ->
                    ( Loading, getEmployees )
                Err error ->
                    ( Waiting (errorToString error), Cmd.none )

        EditEmployee emp ->
            ( Edit (FormEmployee emp.id emp.name emp.email (String.fromInt emp.phone)), Cmd.none )

        EditEmpName name ->
            case model of
                Edit emp ->
                    ( Edit { emp | name = name }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        EditEmpEmail email ->
            case model of
                Edit emp ->
                    ( Edit { emp | email = email }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        EditEmpPhone phone ->
            case model of
                Edit emp ->
                    ( Edit { emp | phone = phone }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        EditSave emp ->
            ( Edit (FormEmployee emp.id emp.name emp.email (String.fromInt emp.phone)) , editEmployee emp )

        EditEmployeeResult result ->
            case result of
                Ok _ ->
                    ( Loading, getEmployees )

                Err error ->
                    ( Waiting (errorToString error), Cmd.none )

-- VIEW

view : EmpListModel -> Html EmpListMessage
view model =
    case model of

        Waiting msg ->
            div [ style "text-align" "center", style "margin-top" "200px"]
            [ p [style "color" "red"] [text msg]
            , button [ onClick TryAgain ] [ text "Reload" ]
            ]

        Loading ->
            text "Please wait ...."

        Success employees ->
            div
                [ style "text-align" "center"
                , style "margin-top" "100px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ h1 [] [ text "Employees" ]
                , table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "ID" ]
                            , th trStyle [ text "Name" ]
                            , th trStyle [ text "Email" ]
                            , th trStyle [ text "Phone" ]
                            , th trStyle [ text "" ]
                            , th trStyle [ text "" ]
                            ]
                        ]
                    , tbody [] (List.map viewEmployee employees)
                    ]
                , button [ onClick TryAgain ] [ text "Reload" ]
                ]

        Edit emp ->
            div
                [ style "text-align" "center"
                , style "margin-top" "200px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ input [ type_ "text", onInput EditEmpName, value emp.name ] []
                , input [ type_ "text", onInput EditEmpPhone, value emp.phone ] []
                , input [ type_ "text", onInput EditEmpEmail, value emp.email ] []
                , validateInput emp
                , br [] []
                , button [onClick TryAgain] [text "Back to table"]
                ]

viewEmployee : Employee -> Html EmpListMessage
viewEmployee employee =
    tr trStyle
        [ td trStyle [ text <| String.fromInt employee.id ]
        , td trStyle [ text employee.name ]
        , td trStyle [ text employee.email ]
        , td trStyle [ text <| String.fromInt employee.phone ]
        , td trStyle [ button [ onClick (DeleteEmployee employee.id) ] [ text "Delete" ]]
        , td trStyle [ button [ onClick (EditEmployee employee) ] [ text "Edit" ]]
        ]

getEmployees : Cmd EmpListMessage
getEmployees =
    Http.get
        { url = "http://localhost:8080/startcode-ca3/api/employees"
        , expect = Http.expectJson EmployeeResult employeeListDecoder
        }

deleteEmployee : Int -> Cmd EmpListMessage
deleteEmployee id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:8080/startcode-ca3/api/employees/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectJson DeleteResult employeeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

editEmployee : Employee -> Cmd EmpListMessage
editEmployee emp =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8080/startcode-ca3/api/employees"
        , body = Http.jsonBody (employeeEncoder emp)
        , expect = Http.expectJson EditEmployeeResult employeeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


validateInput : FormEmployee -> Html EmpListMessage
validateInput emp =
    if emp.name == "" || emp.email == "" || emp.phone == "" then
        div []
        [ button [disabled True] [ text "Save changes" ]
        , p [style "color" "red"] [text "All fields must be filled out."]
        ]
    else
        case String.toInt emp.phone of
            Just phoneNumber ->
                div []
                [ button [ onClick (EditSave (Employee emp.id emp.name emp.email phoneNumber)) ] [ text "Save changes" ]
                , p [] [text ""]
                ]
            Nothing ->
                div []
                [ button [disabled True] [ text "Save changes" ]
                , p [style "color" "red"] [text "Phone number must consist of numbers only."]
                ]

subscriptions : EmpListModel -> Sub EmpListMessage
subscriptions _ =
    Sub.none
