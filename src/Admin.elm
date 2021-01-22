module Admin exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import Json.Decode as D
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , forsar : List Fors
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [], hamtaForsar )


hamtaForsar : Cmd Msg
hamtaForsar =
    Http.get
        { url = "https://forsguden-api.herokuapp.com/forsstracka"
        , expect = Http.expectJson GotForsar (D.field "forsstracka" (D.list forsDecoder))
        }


forsDecoder : D.Decoder Fors
forsDecoder =
    D.map2 Fors
        (D.field "id" D.int)
        (D.field "namn" D.string)


type alias Fors =
    { id : Int, namn : String }


type Msg
    = GotForsar (Result Http.Error (List Fors))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotForsar (Ok forsar) ->
            ( { model | forsar = forsar }, Cmd.none )

        GotForsar (Err err) ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div []
            [ text <| "Antal forsar: " ++ String.fromInt (List.length model.forsar) ]
        ]
    }
