module GetEmployees exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, table, tbody, td, text, th, thead, tr)
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

init : () -> ( Model, Cmd Message)
init _ =
    ( Waiting, Cmd.none )


type Model
    = Failure String
    | Waiting
    | Loading
    | Success (List Employee)


type Message
    = TryAgainPlease
    | EmployeeResult (Result Http.Error (List Employee))


update : Message -> Model -> ( Model, Cmd Message )
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



-- VIEW

view : Model -> Html Message
view model =
    case model of
        Waiting ->
            button [ onClick TryAgainPlease ] [ text "Reload" ]

        Failure msg ->
            text ("Something went wrong " ++ msg)

        Loading ->
            text "Please wait ...."

        Success employees ->
            div [ style "text-align" "center" ]
                [ table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "ID" ]
                            , th trStyle [ text "Name" ]
                            , th trStyle [ text "Email" ]
                            , th trStyle [ text "Phone" ]
                            ]
                        ]
                    , tbody [] (List.map viewEmployee employees)
                    ]
                , button [onClick TryAgainPlease] [text "Reload"]
                ]

tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse"
    , style "width" "100%"
    , style  "border" "1px solid black"
    ]
trStyle : List (Attribute msg)
trStyle =
    [style "border" "1px solid black"]


viewEmployee : Employee -> Html Message
viewEmployee employee =
    tr trStyle
        [ td trStyle [ text <| String.fromInt employee.id ]
        , td trStyle [ text employee.name ]
        , td trStyle [ text employee.email ]
        , td trStyle [ text <| String.fromInt employee.phone ]
        ]

getEmployees : Cmd Message
getEmployees = Http.get
    { url = "http://localhost:8080/startcode_ca3/api/employees"
    , expect = Http.expectJson EmployeeResult allEmployeesDecoder
    }

subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none

type alias Employee =
    {
     id  : Int
    ,name: String
    ,email: String
    ,phone: Int
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
