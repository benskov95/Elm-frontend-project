module GetDeleteEmployees exposing (..)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import HttpError exposing (errorToString)
import Json.Decode as Decode
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL--


type alias Employee =
    { id : Int
    , name : String
    , email : String
    , phone : Int
    }


type EmpListModel
    = Failure String
    | Waiting
    | Loading
    | Success (List Employee)
    | Edit Employee



type EmpListMessage
    = TryAgainPlease
    | EmployeeResult (Result Http.Error (List Employee))
    | DeleteEmployee Int
    | DeleteResult (Result Http.Error Employee)
    | EditEmpName String
    | EditEmpEmail String
    | EditEmpPhone Int
    | EditEmployee Employee
    | EditSave Employee
    | EditEmployeeResult (Result Http.Error Employee)
    | IncorrectInput String


init : () -> ( EmpListModel, Cmd EmpListMessage )
init _ =
    ( Loading, getEmployees )


update : EmpListMessage -> EmpListModel -> ( EmpListModel, Cmd EmpListMessage )
update message model =
    case message of
        TryAgainPlease ->
            ( Loading, getEmployees )

        EmployeeResult result ->
            case result of
                Ok employees ->
                    ( Success employees, Cmd.none )

                Err error ->
                    ( Failure (errorToString error), Cmd.none )

        DeleteEmployee id ->
            ( Loading, deleteEmployee id )

        DeleteResult result ->
            case result of
                Ok _ ->
                    ( Loading, getEmployees )

                Err error ->
                    ( Failure (errorToString error), Cmd.none )

        EditEmployee emp ->
            ( Edit emp, Cmd.none )

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
            ( Edit emp, editEmployee emp )

        EditEmployeeResult result ->
            case result of
                Ok emp ->
                    ( Loading, getEmployees )

                Err error ->
                    ( Failure (errorToString error), Cmd.none )

        IncorrectInput msg ->
            ( Failure msg, Cmd.none )



-- VIEW


view : EmpListModel -> Html EmpListMessage
view model =
    case model of
        Waiting ->
            div [ style "text-align" "center", style "margin-top" "200px" ]
                [ button [ onClick TryAgainPlease ] [ text "Reload" ]
                ]

        Failure msg ->
            text ("Something went wrong " ++ msg)

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
                , button [ onClick TryAgainPlease ] [ text "Reload" ]
                ]

        Edit emp ->
            div
                [ style "text-align" "center"
                , style "margin-top" "200px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ input [ type_ "text", placeholder emp.name, onInput EditEmpName, value emp.name ] []
                , input [ type_ "text", placeholder <| String.fromInt emp.phone, onInput phoneInput, value <| String.fromInt emp.phone ] []
                , input [ type_ "text", placeholder emp.email, onInput EditEmpEmail, value emp.email ] []
                , button [ onClick (EditSave emp) ] [ text "Save changes" ]
                , br [] []
                , button [style "margin-top" "20px", onClick TryAgainPlease] [text "Back to table"]
                ]


tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse"
    , style "width" "100%"
    , style "border" "1px solid black"
    ]


trStyle : List (Attribute msg)
trStyle =
    [ style "border" "1px solid black" ]


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
        , expect = Http.expectJson EmployeeResult allEmployeesDecoder
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


employeeDecoder : Decode.Decoder Employee
employeeDecoder =
    Decode.map4 Employee
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "phone" Decode.int)


employeeEncoder : Employee -> Encode.Value
employeeEncoder employee =
    Encode.object
        [ ( "id", Encode.int employee.id )
        , ( "name", Encode.string employee.name )
        , ( "email", Encode.string employee.email )
        , ( "phone", Encode.int employee.phone )
        ]


allEmployeesDecoder : Decode.Decoder (List Employee)
allEmployeesDecoder =
    Decode.list employeeDecoder


editEmployee : Employee -> Cmd EmpListMessage
editEmployee emp =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8080/startcode-ca3/api/employees/"
        , body = Http.jsonBody (employeeEncoder emp)
        , expect = Http.expectJson EditEmployeeResult employeeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


phoneInput : String -> EmpListMessage
phoneInput input =
    case String.toInt input of
        Just int ->
            EditEmpPhone int

        Nothing ->
            IncorrectInput "Incorrect phone number"


subscriptions : EmpListModel -> Sub EmpListMessage
subscriptions _ =
    Sub.none
