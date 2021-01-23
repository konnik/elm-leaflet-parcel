module Admin exposing (Model, Msg, init, subscriptions, update)

import Admin.Route as Route exposing (Route)
import Admin.Vattendrag as VattendragPage
import Api exposing (Fors, Lan, Vattendrag)
import Auth exposing (UserInfo)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Font as Font
import Element.Input
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
    , session : Auth.Session
    , forsar : List Fors
    , vattendrag : List Vattendrag
    , lan : List Lan
    , vattendragModel : VattendragPage.Model
    , route : Route
    }


type alias Pages =
    { vattendrag : VattendragPage.Model
    }


type Msg
    = GotForsar (Result Http.Error (List Fors))
    | GotVattendrag (Result Http.Error (List Vattendrag))
    | GotLan (Result Http.Error (List Lan))
    | GotUser (Result Http.Error UserInfo)
    | RedigeraVattendrag Vattendrag
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | VattendragMsg VattendragPage.Msg



-- INIT


init : () -> Url.Url -> Nav.Key -> ( AuthState, Cmd Msg )
init flags url key =
    case Implicit.parseToken url of
        Implicit.Empty ->
            ( LoggarIn, Auth.loggaIn url )

        Implicit.Error _ ->
            ( MisslyckadInloggning, Cmd.none )

        Implicit.Success result ->
            let
                session =
                    Auth.intieraSession result

                ( vattendragModel, vattendragCmd ) =
                    VattendragPage.init session
            in
            ( Inloggad
                { key = key
                , url = url
                , session = session
                , lan = []
                , forsar = []
                , vattendrag = []
                , vattendragModel = vattendragModel
                , route = Route.Dashboard
                }
            , Cmd.batch
                [ Auth.hamtaAnvandare result.token GotUser
                , Auth.rensaUrl url key
                , Api.hamtaForsar GotForsar
                , Api.hamtaVattendrag GotVattendrag
                , Api.hamtaLan GotLan
                , Cmd.map VattendragMsg vattendragCmd
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
        RedigeraVattendrag vattendrag ->
            VattendragPage.redigera model.session vattendrag
                |> Tuple.mapFirst
                    (\m ->
                        { model
                            | vattendragModel = m
                            , route = Route.RedigeraVattendrag
                        }
                    )
                |> Tuple.mapSecond (Cmd.map VattendragMsg)

        VattendragMsg pageMsg ->
            VattendragPage.update model.session pageMsg model.vattendragModel
                |> Tuple.mapFirst (\m -> { model | vattendragModel = m })
                |> Tuple.mapSecond (Cmd.map VattendragMsg)

        GotUser (Ok anvandare) ->
            ( { model
                | session =
                    anvandare |> Auth.associeraMedSession model.session
              }
            , Cmd.none
            )

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
        [ Element.layout [ width fill, height fill, padding 20 ] <|
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
        [ headerView model
        , case model.route of
            Route.Dashboard ->
                dashboardView model

            Route.RedigeraVattendrag ->
                VattendragPage.view model.vattendragModel |> Element.map VattendragMsg
        ]


headerView : Model -> Element Msg
headerView model =
    case model.session.anvandare of
        Nothing ->
            Element.none

        Just { namn, bild } ->
            row [ alignRight, spacing 20 ]
                [ text <| namn
                , Element.image [ width (px 50), height (px 50) ] { src = bild, description = "Profilbild" }
                ]


dashboardView : Model -> Element Msg
dashboardView model =
    column [ spacing 30 ]
        [ el [ Font.size 40 ] (text "Forsguiden admin")
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
        [ el [ Font.bold ] (text <| rubrik ++ " (" ++ String.fromInt antal ++ ")")
        , innehall
        ]


forsarView : List Fors -> Element msg
forsarView forsar =
    forsar
        |> List.map .namn
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []


vattendragView : List Vattendrag -> Element Msg
vattendragView vattendrag =
    let
        link v =
            Element.Input.button []
                { label = text v.namn
                , onPress = Just (RedigeraVattendrag v)
                }
    in
    vattendrag
        |> List.map link
        |> List.intersperse (text ", ")
        |> Element.paragraph []


lanView : List Lan -> Element msg
lanView lan =
    lan
        |> List.map .namn
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []
