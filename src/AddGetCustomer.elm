module AddGetCustomer exposing (..)

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
    , phone: Maybe Int
    }

type alias CusFormModel =
    { cId: String
    , cName: String
    , cAddress: String
    , cEmail: String
    , cPhone: String
    , returnedCus: Customer
    , failure: String
    }

type CusFormMessage
        = CId String
        | CName String
        | CAddress String
        | CEmail String
        | CPhone String
        | AddCustomer FormCustomer
        | GetCustomer String
        | ReturnedCus (Result Http.Error Customer)

init : () -> (CusFormModel, Cmd CusFormMessage)
init _ = (CusFormModel "" "" "" "" "" (Customer 0 "" "" "" 0) "", Cmd.none)

update : CusFormMessage -> CusFormModel -> (CusFormModel, Cmd CusFormMessage)
update message model =
    case message of
        CId id ->
            ({model | cId = id}, Cmd.none)

        CName name ->
            ({model | cName = name}, Cmd.none)

        CAddress address ->
            ({model | cAddress = address}, Cmd.none)

        CEmail email ->
            ({model | cEmail = email}, Cmd.none)

        CPhone phone ->
            ({model | cPhone = phone}, Cmd.none)

        AddCustomer employee ->
            ({model | failure = ""}, addCustomer employee)

        GetCustomer id ->
            ({model | failure = ""}, getCustomer id)

        ReturnedCus result ->
            case result of
                Ok customer -> ({model | returnedCus = customer}, Cmd.none)
                Err error -> ({model | failure = errorToString error}, Cmd.none)


view : CusFormModel -> Html CusFormMessage
view model =
    div [style "text-align" "center", style "margin-top" "200px"]
    [ h1 [] [text "Add customer"]
    , viewInput "text" "Name" model.cName CName
    , viewInput "text" "Address" model.cAddress CAddress
    , viewInput "text" "Email" model.cEmail CEmail
    , viewInput "number" "Phone number" model.cPhone CPhone
    , button[ onClick (AddCustomer
        (FormCustomer model.cName model.cAddress model.cEmail (String.toInt model.cPhone)))][text "Add"]

    , h1 [] [text "Get customer by ID"]
    , viewInput "number" "Id" model.cId CId
    , br [] []
    , button [onClick (GetCustomer model.cId)] [text "Search"]
    , showCustomer model.returnedCus
    , p [style "color" "red"] [text model.failure]
    ]

viewInput : String -> String -> String -> (String -> CusFormMessage) -> Html CusFormMessage
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

addCustomer : FormCustomer -> Cmd CusFormMessage
addCustomer customer = Http.post
    { url = "http://localhost:8080/startcode-ca3/api/customers"
    , body = Http.jsonBody
        (customerEncoder (Customer 0 customer.name customer.address customer.email (validatePhone customer.phone)))
    , expect = Http.expectJson ReturnedCus customerDecoder
    }

getCustomer : String -> Cmd CusFormMessage
getCustomer id = Http.get
    { url = "http://localhost:8080/startcode-ca3/api/customers/" ++ id
    , expect = Http.expectJson ReturnedCus customerDecoder
    }

validatePhone : Maybe Int -> Int
validatePhone phone =
    case phone of
        Just pNumber -> pNumber
        Nothing -> 0

showCustomer : Customer -> Html CusFormMessage
showCustomer customer =
    if customer.name /= "" then
    p []
    [ text ("ID: " ++ (String.fromInt customer.id))
    , br [] []
    , text ("Name: " ++ customer.name)
    , br [] []
    , text ("Address: " ++ customer.address)
    , br [] []
    , text ("Email: " ++ customer.email)
    , br [] []
    , text ("Phone: " ++ String.fromInt customer.phone)
    ]
    else
    p [] [text ""]

customerDecoder: Decode.Decoder Customer
customerDecoder =
       Decode.map5 Customer
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "address" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

customerEncoder : Customer -> Encode.Value
customerEncoder customer =
    Encode.object
        [("name", Encode.string customer.name)
        ,("address", Encode.string customer.address)
        ,("email", Encode.string customer.email)
        ,("phone", Encode.int customer.phone)]

subscriptions : CusFormModel -> Sub CusFormMessage
subscriptions _ = Sub.none