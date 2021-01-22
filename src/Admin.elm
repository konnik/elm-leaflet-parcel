module Admin exposing (Model, Msg, init, subscriptions, update)

import Api exposing (Fors, Lan, Vattendrag)
import Auth exposing (UserInfo)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, column, fill, height, padding, px, row, spacing, text, width)
import Element.Font as Font
import Html exposing (Html)
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
                [ Auth.hamtaAnvandare auth.token GotUser
                , Auth.rensaUrl url key
                , Api.hamtaForsar GotForsar
                , Api.hamtaVattendrag GotVattendrag
                , Api.hamtaLan GotLan
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


subscriptions : AuthState -> Sub Msg
subscriptions authState =
    Sub.none



-- VIEW


authView : AuthState -> Browser.Document Msg
authView authState =
    { title = "Forsguiden Admin"
    , body =
        [ Element.layout [ width fill ] <|
            case authState of
                LoggarIn ->
                    text "Loggar in..."

                Inloggad model ->
                    view model

                MisslyckadInloggning ->
                    text "Inloggning misslyckades!"
        ]
    }


view : Model -> Element Msg
view model =
    column [ spacing 30, width fill ]
        [ model.anvandare
            |> Maybe.map inloggadSomView
            |> Maybe.withDefault Element.none
        , sektionView "Forsar" (List.length model.forsar) <|
            forsarView model.forsar
        , sektionView "Vattendrag" (List.length model.vattendrag) <|
            vattendragView model.vattendrag
        , sektionView "Län" (List.length model.lan) <|
            lanView model.lan
        ]


sektionView : String -> Int -> Element msg -> Element msg
sektionView rubrik antal innehall =
    column [ spacing 20 ]
        [ Element.el [ Font.bold ] (text <| rubrik ++ " (" ++ String.fromInt antal ++ ")")
        , innehall
        ]


inloggadSomView : UserInfo -> Element msg
inloggadSomView { namn, bild } =
    row [ alignRight, spacing 20 ]
        [ text <| namn
        , Element.image [ width (px 50), height (px 50) ] { src = bild, description = "Profilbild" }
        ]


forsarView : List Fors -> Element msg
forsarView forsar =
    forsar
        |> List.map .namn
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []


vattendragView : List Vattendrag -> Element msg
vattendragView vattendrag =
    vattendrag
        |> List.map .namn
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []


lanView : List Lan -> Element msg
lanView lan =
    lan
        |> List.map .namn
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []
