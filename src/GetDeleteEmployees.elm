module GetDeleteEmployees exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, h1, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import HttpError exposing (errorToString)
import Json.Decode as Decode

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL--

type alias Employee =
    { id  : Int
    , name: String
    , email: String
    , phone: Int
    }

type EmpListModel
    = Failure String
    | Waiting
    | Loading
    | Continue
    | Success (List Employee)


type EmpListMessage
    = TryAgainPlease
    | EmployeeResult (Result Http.Error (List Employee))
    | DeleteEmployee Int
    | DeleteResult (Result Http.Error Employee)

init : () -> ( EmpListModel, Cmd EmpListMessage)
init _ =
    ( Waiting, Cmd.none )


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
                    (Failure (errorToString error), Cmd.none)

        DeleteEmployee id ->
            (Loading, deleteEmployee id)

        DeleteResult result ->
            case result of
                Ok _ ->
                    (Continue, getEmployees)
                Err error ->
                    (Failure (errorToString error), Cmd.none)



-- VIEW

view : EmpListModel -> Html EmpListMessage
view model =
    case model of
        Waiting ->
            div [ style "text-align" "center", style "margin-top" "200px"]
            [
            button [ onClick TryAgainPlease ] [ text "Reload" ]
            ]

        Failure msg ->
            text ("Something went wrong " ++ msg)

        Loading ->
            text "Please wait ...."

        Success employees ->
            div [ style "text-align" "center"
                , style "margin-top" "100px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ h1 [] [text "Employees"]
                , table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "ID" ]
                            , th trStyle [ text "Name" ]
                            , th trStyle [ text "Email" ]
                            , th trStyle [ text "Phone" ]
                            , th trStyle [ text "" ]
                            ]
                        ]
                    , tbody [] (List.map viewEmployee employees)
                    ]
                , button [onClick TryAgainPlease] [text "Reload"]
                ]

        Continue ->
            text ""

tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse"
    , style "width" "100%"
    , style  "border" "1px solid black"
    ]
trStyle : List (Attribute msg)
trStyle =
    [style "border" "1px solid black"]


viewEmployee : Employee -> Html EmpListMessage
viewEmployee employee =
    tr trStyle
        [ td trStyle [ text <| String.fromInt employee.id ]
        , td trStyle [ text employee.name ]
        , td trStyle [ text employee.email ]
        , td trStyle [ text <| String.fromInt employee.phone ]
        , td trStyle [button [onClick (DeleteEmployee employee.id)] [text "Delete"]]
        ]

getEmployees : Cmd EmpListMessage
getEmployees = Http.get
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

employeeDecoder: Decode.Decoder Employee
employeeDecoder =
       Decode.map4 Employee
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

allEmployeesDecoder: Decode.Decoder (List Employee)
allEmployeesDecoder =
    Decode.list employeeDecoder

subscriptions : EmpListModel -> Sub EmpListMessage
subscriptions _ =
    Sub.none
