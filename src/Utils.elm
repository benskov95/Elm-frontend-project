module Utils exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Json.Encode as Encode

-- HTML ELEMENTS

tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse"
    , style "width" "100%"
    , style "border" "1px solid black"
    ]


trStyle : List (Attribute msg)
trStyle =
    [ style "border" "1px solid black" ]

-- TYPE RECORDS

type alias Customer =
    { id: Int
    , name: String
    , address: String
    , email: String
    , phone: Int
    }

type alias FormCustomer =
    { name: String
    , address: String
    , email: String
    , phone: String
    }

type alias Employee =
    { id : Int
    , name : String
    , email : String
    , phone : Int
    }

type alias FormEmployee =
    { id : Int
    , name : String
    , email : String
    , phone : String
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

-- DECODERS

customerDecoder : Decode.Decoder Customer
customerDecoder =
       Decode.map5 Customer
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "address" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

customerListDecoder : Decode.Decoder (List Customer)
customerListDecoder =
    Decode.list customerDecoder

employeeDecoder : Decode.Decoder Employee
employeeDecoder =
    Decode.map4 Employee
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "phone" Decode.int)

employeeListDecoder : Decode.Decoder (List Employee)
employeeListDecoder =
    Decode.list employeeDecoder

taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.map6 Task
    (Decode.field "id" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Decode.string)
    (Decode.field "description" Decode.string)
    (Decode.field "employeeList" employeeListDecoder)
    (Decode.field "customer" customerDecoder)

taskListDecoder : Decode.Decoder (List Task)
taskListDecoder =
    Decode.list taskDecoder

-- ENCODERS

customerEncoder : Customer -> Encode.Value
customerEncoder customer =
    Encode.object
        [("name", Encode.string customer.name)
        ,("address", Encode.string customer.address)
        ,("email", Encode.string customer.email)
        ,("phone", Encode.int customer.phone)]

employeeEncoder : Employee -> Encode.Value
employeeEncoder employee =
    Encode.object
        [ ( "id", Encode.int employee.id )
        , ( "name", Encode.string employee.name )
        , ( "email", Encode.string employee.email )
        , ( "phone", Encode.int employee.phone )
        ]

empOnlyIdEncoder : Employee -> Encode.Value
empOnlyIdEncoder emp =
    Encode.object
    [("id", Encode.int emp.id)]

taskEncoder : Task -> Encode.Value
taskEncoder task =
    Encode.object
    [("title", Encode.string task.title)
    ,("date", Encode.string task.date)
    ,("description", Encode.string task.description)
    ,("employeeList", Encode.list empOnlyIdEncoder task.employeeList)
    ,("customer", Encode.object[("id", Encode.int task.customer.id)])
    ]

