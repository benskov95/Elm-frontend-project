module HomePage exposing (..)


import Browser
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href, style)
main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model = {msg: String}


init : () -> (Model, Cmd Message )
init _ = (Model "Welcome", Cmd.none)

type Message = Home

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Home -> (Model "Welcome", Cmd.none)



view : Model -> Html Message
view model =
    div[style "text-align" "center", style "margin-top" "200px"] [
    h1[] [text model.msg],
    ul [] [
     li [] [a [ href "http://localhost:8000/src/CustomerTable.elm" ] [ text "Customer table" ]]
    ,li [] [a [ href "http://localhost:8000/src/CustomerForm.elm" ] [ text "Customer form" ]]
    ,li [] [a [ href "http://localhost:8000/src/EmployeeTable.elm" ] [ text "Employee table" ]]
    ,li [] [a [ href "http://localhost:8000/src/EmployeeForm.elm" ] [ text "Employee form" ]]
    ]]



subscriptions : Model -> Sub Message
subscriptions _ = Sub.none
