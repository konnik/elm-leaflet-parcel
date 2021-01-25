module Api.Common exposing (delete, get, post, put, request)

import Auth exposing (Session)
import Html.Attributes exposing (method)
import Http
import OAuth


get : Session -> { url : String, expect : Http.Expect msg } -> Cmd msg
get session { url, expect } =
    request session
        { method = "GET"
        , url = url
        , expect = expect
        , body = Http.emptyBody
        }


post : Session -> { url : String, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
post session { url, expect, body } =
    request session
        { method = "POST"
        , url = url
        , expect = expect
        , body = body
        }


put : Session -> { url : String, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
put session { url, expect, body } =
    request session
        { method = "PUT"
        , url = url
        , expect = expect
        , body = body
        }


delete : Session -> { url : String, expect : Http.Expect msg } -> Cmd msg
delete session { url, expect } =
    request session
        { method = "DELETE"
        , url = url
        , expect = expect
        , body = Http.emptyBody
        }


request : Session -> { method : String, url : String, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
request session { method, url, expect, body } =
    Http.request
        { method = method
        , url = url
        , headers = OAuth.useToken session.token []
        , expect = expect
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }
