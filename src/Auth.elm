module Auth exposing (Session, UserInfo, associeraMedSession, hamtaAnvandare, intieraSession, loggaIn, rensaUrl)

import Browser.Navigation as Nav exposing (Key)
import Http
import Json.Decode as D
import OAuth
import OAuth.Implicit as Implicit
import Url exposing (Url)


type alias Session =
    { token : OAuth.Token
    , scope : List String
    , anvandare : Maybe UserInfo
    }


type alias UserInfo =
    { namn : String
    , bild : String
    }


intieraSession : Implicit.AuthorizationSuccess -> Session
intieraSession authSuccess =
    { token = authSuccess.token
    , scope = authSuccess.scope
    , anvandare = Nothing
    }


associeraMedSession : Session -> UserInfo -> Session
associeraMedSession session anvandare =
    { session | anvandare = Just anvandare }


authEndpoint : Url
authEndpoint =
    { protocol = Url.Https
    , host = "forsguiden.eu.auth0.com"
    , port_ = Nothing
    , path = "/authorize"
    , query = Nothing
    , fragment = Nothing
    }


hamtaAnvandare : OAuth.Token -> (Result Http.Error UserInfo -> msg) -> Cmd msg
hamtaAnvandare token toMsg =
    Http.request
        { method = "GET"
        , url = "https://forsguiden.eu.auth0.com/userinfo"
        , headers = OAuth.useToken token []
        , expect = Http.expectJson toMsg userInfoDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


userInfoDecoder : D.Decoder UserInfo
userInfoDecoder =
    D.map2 UserInfo
        (D.field "name" D.string)
        (D.field "picture" D.string)


rensaUrl : Url -> Key -> Cmd msg
rensaUrl url key =
    { url | fragment = Nothing }
        |> Url.toString
        |> Nav.replaceUrl key


loggaIn : Url -> Cmd msg
loggaIn url =
    { clientId = "5I2LnV4sjkf5y8qny94bTFSnila15vFU"
    , url = authEndpoint
    , redirectUri = url
    , scope =
        [ "openid"
        , "profile"
        , "email"
        , "redigera:lan"
        , "redigera:vattendrag"
        , "redigera:forsstracka"
        ]
    , state = Nothing
    }
        |> Implicit.makeAuthorizationUrl
        |> Url.toString
        |> Nav.load
