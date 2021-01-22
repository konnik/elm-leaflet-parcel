module Admin exposing (Model, Msg, init, subscriptions, update)

import Auth exposing (UserInfo)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import Json.Decode as D
import OAuth
import OAuth.Implicit as Implicit
import Url exposing (Url)


main : Program () AuthState Msg
main =
    Browser.application
        { init = init
        , view = authView
        , update = authUpdate
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type AuthState
    = Inloggad Model
    | LoggarIn
    | MisslyckadInloggning


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , auth : Implicit.AuthorizationSuccess
    , anvandare : Maybe UserInfo
    , forsar : List Fors
    , vattendrag : List Vattendrag
    , lan : List Lan
    }


type alias Fors =
    { id : Int, namn : String }


type alias Vattendrag =
    { id : Int, namn : String }


type alias Lan =
    { id : Int, namn : String }


type Msg
    = GotForsar (Result Http.Error (List Fors))
    | GotVattendrag (Result Http.Error (List Vattendrag))
    | GotLan (Result Http.Error (List Lan))
    | GotUser (Result Http.Error UserInfo)
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


init : () -> Url.Url -> Nav.Key -> ( AuthState, Cmd Msg )
init flags url key =
    case Implicit.parseToken url of
        Implicit.Empty ->
            ( LoggarIn, Auth.loggaIn url )

        Implicit.Error _ ->
            ( MisslyckadInloggning, Cmd.none )

        Implicit.Success auth ->
            ( Inloggad
                { key = key
                , url = url
                , auth = auth
                , anvandare = Nothing
                , lan = []
                , forsar = []
                , vattendrag = []
                }
            , Cmd.batch
                [ Auth.rensaUrl url key
                , Auth.hamtaAnvandare auth.token GotUser
                , hamtaForsar
                , hamtaVattendrag
                , hamtaLan
                ]
            )



-- UPDATE


authUpdate : Msg -> AuthState -> ( AuthState, Cmd Msg )
authUpdate msg authState =
    case authState of
        Inloggad model ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            ( Inloggad newModel, cmd )

        _ ->
            ( authState, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUser (Ok anvandare) ->
            ( { model | anvandare = Just anvandare }, Cmd.none )

        GotUser (Err _) ->
            ( model, Cmd.none )

        GotForsar (Ok forsar) ->
            ( { model | forsar = forsar }, Cmd.none )

        GotForsar (Err err) ->
            ( model, Cmd.none )

        GotVattendrag (Ok vattendrag) ->
            ( { model | vattendrag = vattendrag }, Cmd.none )

        GotVattendrag (Err err) ->
            ( model, Cmd.none )

        GotLan (Ok lan) ->
            ( { model | lan = lan }, Cmd.none )

        GotLan (Err err) ->
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


hamtaForsar : Cmd Msg
hamtaForsar =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/forsstracka"
        , expect = Http.expectJson GotForsar (D.field "forsstracka" (D.list forsDecoder))
        }


forsDecoder : D.Decoder Fors
forsDecoder =
    D.map2 Fors
        (D.field "id" D.int)
        (D.field "namn" D.string)


hamtaVattendrag : Cmd Msg
hamtaVattendrag =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/vattendrag"
        , expect = Http.expectJson GotVattendrag (D.field "vattendrag" (D.list vattendragDecoder))
        }


vattendragDecoder : D.Decoder Vattendrag
vattendragDecoder =
    D.map2 Vattendrag
        (D.field "id" D.int)
        (D.field "namn" D.string)


hamtaLan : Cmd Msg
hamtaLan =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/lan"
        , expect = Http.expectJson GotLan (D.field "lan" (D.list lanDecoder))
        }


lanDecoder : D.Decoder Lan
lanDecoder =
    D.map2 Lan
        (D.field "id" D.int)
        (D.field "namn" D.string)


subscriptions : AuthState -> Sub Msg
subscriptions authState =
    Sub.none



-- VIEW


authView : AuthState -> Browser.Document Msg
authView authState =
    { title = "Forsguiden Admin"
    , body =
        [ case authState of
            LoggarIn ->
                text "Loggar in..."

            Inloggad model ->
                view model

            MisslyckadInloggning ->
                text "Inloggning misslyckades!"
        ]
    }


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| "Inloggad som: " ++ (Maybe.map .namn model.anvandare |> Maybe.withDefault "?") ]
        , p []
            [ text <| "Forsar (" ++ String.fromInt (List.length model.forsar) ++ ")"
            , div [] [ forsarView model.forsar ]
            ]
        , p []
            [ text <| "Vattendrag (" ++ String.fromInt (List.length model.vattendrag) ++ ")"
            , div [] [ vattendragView model.vattendrag ]
            ]
        , p []
            [ text <| "Län (" ++ String.fromInt (List.length model.lan) ++ ")"
            , div [] [ lanView model.lan ]
            ]
        ]


forsarView : List Fors -> Html msg
forsarView forsar =
    forsar
        |> List.map .namn
        |> String.join ", "
        |> text


vattendragView : List Vattendrag -> Html msg
vattendragView vattendrag =
    vattendrag
        |> List.map .namn
        |> String.join ", "
        |> text


lanView : List Lan -> Html msg
lanView lan =
    lan
        |> List.map .namn
        |> String.join ", "
        |> text
