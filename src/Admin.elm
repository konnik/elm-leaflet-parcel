module Admin exposing (Model, Msg, init, main, subscriptions, update)

import Admin.Route as Route exposing (Route(..))
import Admin.Vattendrag as VattendragPage
import Api exposing (Fors, Lan, Resurs(..), Vattendrag)
import Auth exposing (UserInfo)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Font as Font
import Element.Input
import Http
import OAuth
import OAuth.Implicit as Implicit
import Url


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
    , forsar : List (Resurs Fors)
    , vattendrag : List (Resurs Vattendrag)
    , lan : List Lan
    , vattendragModel : VattendragPage.Model
    , route : Route
    }


type Msg
    = GotForsar (Result Http.Error (List (Resurs Fors)))
    | GotVattendrag (Result Http.Error (List (Resurs Vattendrag)))
    | GotLan (Result Http.Error (List Lan))
    | GotUser (Result Http.Error UserInfo)
    | RedigeraVattendrag (Resurs Vattendrag)
    | NyttVattendrag
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | VattendragMsg VattendragPage.Msg
    | NavigeraTillDashboard



-- INIT


init : () -> Url.Url -> Nav.Key -> ( AuthState, Cmd Msg )
init _ url key =
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
        NavigeraTillDashboard ->
            ( { model | route = Dashboard }, Cmd.none )
                |> refreshDashboard

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

        NyttVattendrag ->
            VattendragPage.nytt model.session
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

        GotForsar (Err _) ->
            ( model, Cmd.none )

        GotVattendrag (Ok vattendrag) ->
            ( { model | vattendrag = vattendrag }, Cmd.none )

        GotVattendrag (Err _) ->
            ( model, Cmd.none )

        GotLan (Ok lan) ->
            ( { model | lan = lan }, Cmd.none )

        GotLan (Err _) ->
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


refreshDashboard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshDashboard ( model, cmd ) =
    ( { model
        | lan = []
        , forsar = []
        , vattendrag = []
      }
    , Cmd.batch
        [ cmd
        , Api.hamtaForsar GotForsar
        , Api.hamtaVattendrag GotVattendrag
        , Api.hamtaLan GotLan
        ]
    )


subscriptions : AuthState -> Sub Msg
subscriptions _ =
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
            row [ width fill ]
                [ Element.Input.button [] { label = el [ Font.size 30 ] (text "Dashboard"), onPress = Just NavigeraTillDashboard }
                , row [ alignRight, spacing 20 ]
                    [ text <| namn
                    , Element.image [ width (px 50), height (px 50) ] { src = bild, description = "Profilbild" }
                    ]
                ]


dashboardView : Model -> Element Msg
dashboardView model =
    column [ spacing 30 ]
        [ sektionView "Forsar" (List.length model.forsar) Nothing <|
            forsarView model.forsar
        , sektionView "Vattendrag" (List.length model.vattendrag) (Just NyttVattendrag) <|
            vattendragView model.vattendrag
        , sektionView "Län" (List.length model.lan) Nothing <|
            lanView model.lan
        ]


sektionView : String -> Int -> Maybe msg -> Element msg -> Element msg
sektionView rubrik antal newMsg innehall =
    column [ spacing 20 ]
        [ row [ spacing 30 ]
            [ el [ Font.bold ] (text <| rubrik ++ " (" ++ String.fromInt antal ++ ")")
            , Element.Input.button [] { label = text "[Nytt]", onPress = newMsg }
            ]
        , innehall
        ]


forsarView : List (Resurs Fors) -> Element msg
forsarView forsar =
    forsar
        |> List.map (\(Resurs id fors) -> fors.namn ++ "(" ++ String.fromInt id ++ ")")
        |> String.join ", "
        |> text
        |> List.singleton
        |> Element.paragraph []


vattendragView : List (Resurs Vattendrag) -> Element Msg
vattendragView vattendrag =
    let
        link : Resurs Vattendrag -> Element Msg
        link ((Resurs id v) as res) =
            Element.Input.button []
                { label =
                    text
                        (if v.namn == "" then
                            "<blank>" ++ "(" ++ String.fromInt id ++ ")"

                         else
                            v.namn ++ "(" ++ String.fromInt id ++ ")"
                        )
                , onPress = Just (RedigeraVattendrag res)
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
