module HttpError exposing (..)

import Http

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadStatus code ->
          "Code: "++ (String.fromInt code)
        Http.NetworkError ->
         "Network Error"
        Http.BadBody err ->
         "Bad Body: "++ err
        Http.Timeout ->
         "Timeout"
        Http.BadUrl string ->
         "Bad Url: "++ string