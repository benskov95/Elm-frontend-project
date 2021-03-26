module GetDeleteCustomers exposing (..)

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

type alias Customer =
    { id  : Int
    , name: String
    , address: String
    , email: String
    , phone: Int
    }

type CusListModel
    = Failure String
    | Waiting
    | Loading
    | Continue
    | Success (List Customer)


type CusListMessage
    = TryAgainPlease
    | CustomerResult (Result Http.Error (List Customer))
    | DeleteCustomer Int
    | DeleteResult (Result Http.Error Customer)

init : () -> ( CusListModel, Cmd CusListMessage)
init _ =
    ( Waiting, Cmd.none )


update : CusListMessage -> CusListModel -> ( CusListModel, Cmd CusListMessage )
update message model =
    case message of
        TryAgainPlease ->
            ( Loading, getCustomers )

        CustomerResult result ->
            case result of
                Ok employees ->
                    ( Success employees, Cmd.none )

                Err error ->
                    (Failure (errorToString error), Cmd.none)

        DeleteCustomer id ->
            (Loading, deleteCustomer id)

        DeleteResult result ->
            case result of
                Ok _ ->
                    (Continue, getCustomers)
                Err error ->
                    (Failure (errorToString error), Cmd.none)



-- VIEW

view : CusListModel -> Html CusListMessage
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

        Success customers ->
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
                            ]
                        ]
                    , tbody [] (List.map viewCustomer customers)
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


viewCustomer : Customer -> Html CusListMessage
viewCustomer customer =
    tr trStyle
        [ td trStyle [ text <| String.fromInt customer.id ]
        , td trStyle [ text customer.name ]
        , td trStyle [ text customer.address ]
        , td trStyle [ text customer.email ]
        , td trStyle [ text <| String.fromInt customer.phone ]
        , td trStyle [button [onClick (DeleteCustomer customer.id)] [text "Delete"]]
        ]

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

customerDecoder: Decode.Decoder Customer
customerDecoder =
       Decode.map5 Customer
       (Decode.field "id" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "address" Decode.string)
       (Decode.field "email" Decode.string)
       (Decode.field "phone" Decode.int)

customerListDecoder: Decode.Decoder (List Customer)
customerListDecoder =
    Decode.list customerDecoder

subscriptions : CusListModel -> Sub CusListMessage
subscriptions _ =
    Sub.none
