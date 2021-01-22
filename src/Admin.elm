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
    , vattendrag : List Vattendrag
    , lan : List Lan
    }


type alias Fors =
    { id : Int, namn : String }


type alias Vattendrag =
    { id : Int, namn : String }


type alias Lan =
    { id : Int, namn : String }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url [] [] [], Cmd.batch [ hamtaForsar, hamtaVattendrag, hamtaLan ] )


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


type Msg
    = GotForsar (Result Http.Error (List Fors))
    | GotVattendrag (Result Http.Error (List Vattendrag))
    | GotLan (Result Http.Error (List Lan))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Forsguiden Admin"
    , body =
        [ div []
            [ p []
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
        ]
    }


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
