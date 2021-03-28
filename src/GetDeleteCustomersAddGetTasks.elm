module GetDeleteCustomersAddGetTasks exposing (..)

import Browser
import GetDeleteEditEmployees exposing (Employee, employeeListDecoder)
import Html exposing (Attribute, Html, br, button, div, h1, input, p, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
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

type alias Customer =
    { id: Int
    , name: String
    , address: String
    , email: String
    , phone: Int
    }

type alias Task =
    { id: Int
    , title : String
    , date: String
    , description: String
    , employeeList: List Employee
    , customer: Customer
    }

type alias FormTask =
    { title: String
    , date: String
    , description: String
    , employeeIds: String
    , customerId: Int
    }

type CusListModel
    = Waiting String
    | Loading
    | CusSuccess (List Customer)
    | CusIdForFirstTask String
    | TasksSuccess (List Task)
    | NewTask FormTask


type CusListMessage
    = TryAgain
    | CustomerResult (Result Http.Error (List Customer))
    | DeleteCustomer Int
    | DeleteResult (Result Http.Error Customer)
    | GetCustomerTasks Int
    | GetTasksResult (Result Http.Error (List Task))
    | CusId String
    | MakeNewTask FormTask
    | TaskTitle String
    | TaskDate String
    | TaskDescription String
    | TaskEmployeeIds String
    | AddTask Task
    | AddResult (Result Http.Error Task)

init : () -> ( CusListModel, Cmd CusListMessage)
init _ = ( Loading, getCustomers )

update : CusListMessage -> CusListModel -> ( CusListModel, Cmd CusListMessage )
update message model =
    case message of
        TryAgain ->
            ( Loading, getCustomers )

        CustomerResult result ->
            case result of
                Ok employees ->
                    ( CusSuccess employees, Cmd.none )
                Err _ ->
                    (Waiting "Something went wrong. Please reload.", Cmd.none)

        DeleteCustomer id ->
            (Loading, deleteCustomer id)

        DeleteResult result ->
            case result of
                Ok _ ->
                    (Loading, getCustomers)
                Err _->
                    (Waiting "Something went wrong. Please reload.", Cmd.none)

        GetCustomerTasks id ->
            (Loading, getCustomerTasks id)

        GetTasksResult result ->
            case result of
                Ok tasks ->
                    (TasksSuccess tasks, Cmd.none)
                Err _ ->
                    (CusIdForFirstTask "", Cmd.none)

        CusId input ->
            case model of
                CusIdForFirstTask _ ->
                    (CusIdForFirstTask input, Cmd.none)
                _ ->
                    (model, Cmd.none)

        MakeNewTask task ->
            (NewTask task, Cmd.none)

        TaskTitle title ->
            case model of
                NewTask task ->
                    (NewTask {task | title = title}, Cmd.none)
                _ ->
                    (model, Cmd.none)

        TaskDate date ->
            case model of
                NewTask task ->
                    (NewTask {task | date = date}, Cmd.none)
                _ ->
                    (model, Cmd.none)

        TaskDescription description ->
            case model of
                NewTask task ->
                    (NewTask {task | description = description}, Cmd.none)
                _ ->
                    (model, Cmd.none)

        TaskEmployeeIds employeeIds ->
            case model of
                NewTask task ->
                    (NewTask {task | employeeIds = employeeIds}, Cmd.none)
                _ ->
                    (model, Cmd.none)

        AddTask task ->
            (Loading, addTask task)

        AddResult result ->
            case result of
                Ok task ->
                    (Loading, getCustomerTasks task.customer.id)
                Err _ ->
                    (Waiting "Something went wrong.", Cmd.none)


-- VIEW

view : CusListModel -> Html CusListMessage
view model =
    case model of

        Waiting msg ->
            div [ style "text-align" "center", style "margin-top" "200px"]
            [ p [style "color" "red"] [text msg]
            , button [ onClick TryAgain ] [ text "Reload" ]
            ]

        Loading ->
            text "Please wait ...."

        CusSuccess customers ->
            div [ style "text-align" "center"
                , style "margin-top" "100px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ h1 [] [text "Customers"]
                , table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "ID" ]
                            , th trStyle [ text "Name" ]
                            , th trStyle [ text "Address" ]
                            , th trStyle [ text "Email" ]
                            , th trStyle [ text "Phone" ]
                            , th trStyle [ text "" ]
                            , th trStyle [ text "" ]
                            ]
                        ]
                    , tbody [] (List.map viewCustomer customers)
                    ]
                , button [onClick TryAgain] [text "Reload"]
                ]

        CusIdForFirstTask cusId ->
            div [ style "text-align" "center", style "margin-top" "200px"]
            [ strong
            [style "color" "red"]
            [text "This customer has no tasks yet. To add the first one, enter the ID of the customer in the input field and click \"Go\"."]
            , br [] []
            , input [type_ "text", onInput CusId, value cusId, style "margin-top" "30px"] []
            , validateCusId cusId
            , button [onClick TryAgain, style "margin-top" "30px"] [text "Back to table"]
            ]

        TasksSuccess tasks ->
            div [ style "text-align" "center"
                , style "margin-top" "100px"
                , style "margin-left" "500px"
                , style "margin-right" "500px"
                ]
                [ h1 [] [text "Customer tasks"]
                , table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "Task ID" ]
                            , th trStyle [ text "Customer name" ]
                            , th trStyle [ text "Title" ]
                            , th trStyle [ text "Date" ]
                            , th trStyle [ text "Description" ]
                            , th trStyle [ text "Employees assigned" ]
                            ]
                        ]
                    , tbody [] (List.map viewTask tasks)
                    ]
                , button [onClick (GetCustomerTasks ((getCustomerFromTasks tasks).id))] [text "Reload"]
                , button [onClick (MakeNewTask (FormTask "" "" "" "" (getCustomerFromTasks tasks).id))] [text "Add new task"]
                , br [] []
                , button [style "margin-top" "20px", onClick TryAgain] [text "Back to table"]
                ]

        NewTask task ->
            div [style "text-align" "center", style "margin-top" "100px"]
            [ h1 [] [text "Add task"]
            , strong [] [text "Please check the employee table to see which employees are available when selecting."]
            , br [] [], br [] []
            , viewInput "text" "Title" task.title TaskTitle
            , viewInput "text" "Date" task.date TaskDate
            , viewInput "text" "Description" task.description TaskDescription
            , viewInput "text" "Employees" task.employeeIds TaskEmployeeIds
            , validateInput (task)
            , br [] []
            , button [style "margin-top" "20px", onClick (GetCustomerTasks task.customerId)] [text "Back to customer tasks"]
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

viewInput : String -> String -> String -> (String -> CusListMessage) -> Html CusListMessage
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewCustomer : Customer -> Html CusListMessage
viewCustomer customer =
    tr trStyle
        [ td trStyle [ text <| String.fromInt customer.id ]
        , td trStyle [ text customer.name ]
        , td trStyle [ text customer.address ]
        , td trStyle [ text customer.email ]
        , td trStyle [ text <| String.fromInt customer.phone ]
        , td trStyle [button [onClick (DeleteCustomer customer.id)] [text "Delete"]]
        , td trStyle [button [onClick (GetCustomerTasks customer.id)] [text "Get Tasks"]]
        ]

viewTask : Task -> Html CusListMessage
viewTask task =
    tr trStyle
        [ td trStyle [ text <| String.fromInt task.id ]
        , td trStyle [ text task.customer.name]
        , td trStyle [ text task.title ]
        , td trStyle [ text task.date ]
        , td trStyle [ text task.description ]
        , td trStyle [ text (String.join ", " (showEmployees task.employeeList))]
        ]

showEmployees : List Employee -> List String
showEmployees employees =
    let
        empToString employee = "Name: " ++ employee.name ++ ", ID: " ++ String.fromInt employee.id
    in
    List.map empToString employees

getCustomerFromTasks : List Task -> Customer
getCustomerFromTasks tasks =
    case tasks of
        task :: _ -> task.customer
        [] -> Customer 0 "" "" "" 0

getCustomers : Cmd CusListMessage
getCustomers = Http.get
    { url = "http://localhost:8080/startcode-ca3/api/customers"
    , expect = Http.expectJson CustomerResult customerListDecoder
    }

deleteCustomer : Int -> Cmd CusListMessage
deleteCustomer id =
    Http.request
    { method = "DELETE"
    , headers = []
    , url = "http://localhost:8080/startcode-ca3/api/customers/" ++ String.fromInt id
    , body = Http.emptyBody
    , expect = Http.expectJson DeleteResult customerDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

getCustomerTasks : Int -> Cmd CusListMessage
getCustomerTasks id = Http.get
        { url = "http://localhost:8080/startcode-ca3/api/tasks/" ++ String.fromInt id
        , expect = Http.expectJson GetTasksResult taskListDecoder
        }

addTask : Task -> Cmd CusListMessage
addTask task = Http.post
    { url = "http://localhost:8080/startcode-ca3/api/tasks"
    , body = Http.jsonBody
        (taskEncoder task)
    , expect = Http.expectJson AddResult taskDecoder
    }


validateInput : FormTask -> Html CusListMessage
validateInput task =
    if task.title == "" || task.date == "" || task.description == "" || task.employeeIds == "" then
        div []
        [ button [disabled True] [ text "Add" ]
        , p [style "color" "red"] [text "All fields must be filled out."]
        ]
    else
        let
            empIds = String.split ", " task.employeeIds
            intIds = List.map String.toInt empIds
            emps = List.map maybeIdsToEmps intIds
        in
            if (checkEmpIds emps) == True then
                div []
                [ button
                [ onClick
                    ( AddTask ( Task 0 task.title task.date task.description emps ( Customer task.customerId "" "" "" 0 )))
                ] [ text "Add" ]
                , p [] [text ""]
                ]
            else
                div []
                [ button [disabled True] [ text "Add" ]
                , p [style "color" "red"] [text "Write employee IDs with the following format: \"1, 2, ...\""]
                ]

validateCusId : String -> Html CusListMessage
validateCusId cusId =
    case String.toInt cusId of
        Just id ->
            div []
            [ button [onClick (MakeNewTask (FormTask "" "" "" "" id))] [text "Go"]
            ]
        Nothing ->
            div []
            [ button [disabled True] [text "Go"]
            , p [style "color" "red"] [text "Input must be a number."]
            ]

maybeIdsToEmps : Maybe Int -> Employee
maybeIdsToEmps maybeId =
        case maybeId of
             Just id ->
                 Employee id "" "" 0
             Nothing ->
                 Employee 0 "" "" 0

checkEmpIds : List Employee -> Bool
checkEmpIds employees =
    case employees of
        [] -> False
        [one] -> if one.id == 0 then False else True
        (first :: rest) ->
            if first.id == 0 then
                False
            else
                checkEmpIds rest

customerDecoder : Decode.Decoder Customer
customerDecoder =
       Decode.map5 Customer
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "address" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

taskEncoder : Task -> Encode.Value
taskEncoder task =
    Encode.object
    [("title", Encode.string task.title)
    ,("date", Encode.string task.date)
    ,("description", Encode.string task.description)
    ,("employeeList", Encode.list empOnlyIdEncoder task.employeeList)
    ,("customer", Encode.object[("id", Encode.int task.customer.id)])
    ]

empOnlyIdEncoder : Employee -> Encode.Value
empOnlyIdEncoder emp =
    Encode.object
    [("id", Encode.int emp.id)]

taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.map6 Task
    (Decode.field "id" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Decode.string)
    (Decode.field "description" Decode.string)
    (Decode.field "employeeList" employeeListDecoder)
    (Decode.field "customer" customerDecoder)

customerListDecoder : Decode.Decoder (List Customer)
customerListDecoder =
    Decode.list customerDecoder

taskListDecoder : Decode.Decoder (List Task)
taskListDecoder =
    Decode.list taskDecoder

subscriptions : CusListModel -> Sub CusListMessage
subscriptions _ =
    Sub.none
