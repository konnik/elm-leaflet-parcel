module Lab exposing (main)

import Api.Hojd exposing (Hojd)
import Api.Smhi exposing (Smhipunkt)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerX, column, el, fill, height, htmlAttribute, padding, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Karta exposing (Karta, Kartlager(..))
import RemoteData
import Url


type alias Model =
    { karta : Maybe Karta
    , message : String
    , hojd : RemoteData.WebData Float
    , smhipunkt : RemoteData.WebData Smhipunkt
    , dolj : Bool
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ValjKartlager Karta Kartlager
    | GotKartEvent Karta.Event
    | DoljKarta
    | GotSmhiPunkt Karta (RemoteData.WebData Smhipunkt)
    | GotHojd Karta (RemoteData.WebData Hojd)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { karta = Nothing
      , message = "Klicka gärna lite i kartan!"
      , hojd = RemoteData.NotAsked
      , smhipunkt = RemoteData.NotAsked
      , dolj = False
      }
    , Karta.initiera "karta1"
    )



--60.71786, 17.14122
-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSmhiPunkt karta smhipunkt ->
            ( { model | smhipunkt = smhipunkt }
            , RemoteData.unwrap Cmd.none (\punkt -> Karta.placeraKartnal "smhi" punkt.koordinater karta) smhipunkt
            )

        GotHojd karta hojd ->
            ( { model | hojd = hojd }, Cmd.none )

        DoljKarta ->
            ( { model | dolj = not model.dolj }, Cmd.none )

        ValjKartlager karta lager ->
            ( model, Karta.visaLager lager karta )

        GotKartEvent (Karta.Skapad karta) ->
            ( { model | karta = Just karta, message = "Klicka lite i kartan vettja..." }
            , Cmd.none
            )

        GotKartEvent (Karta.KlickIKarta karta lat long) ->
            ( { model
                | message = "Koordinat: " ++ String.fromFloat lat ++ ", " ++ String.fromFloat long
                , hojd = RemoteData.Loading
                , smhipunkt = RemoteData.Loading
              }
            , Cmd.batch
                [ karta |> Karta.placeraKartnal "klick" { lat = lat, long = long }
                , Api.Smhi.sokSmhipunkt { lat = lat, long = long } (GotSmhiPunkt karta)
                , Api.Hojd.hamtaHojd { lat = lat, long = long } (GotHojd karta)
                ]
            )

        GotKartEvent (Karta.Unknown error) ->
            ( { model | message = error }, Cmd.none )

        UrlRequested _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Lab"
    , body =
        [ Element.layout [ width fill, height fill ] <|
            Element.column [ paddingXY 0 10, width fill, height fill ]
                [ el [ width fill, height fill ]
                    (Maybe.map kartaView model.karta
                        |> Maybe.withDefault (Element.text "Initierar kartan...")
                    )
                , row [ centerX, spacing 20 ]
                    [ Element.text model.message
                    , viewRemoteData viewHojd model.hojd
                    , viewRemoteData viewSmhipunkt model.smhipunkt
                    ]
                ]
        ]
    }


viewHojd : Float -> Element msg
viewHojd hojd =
    Element.text <| "Höjd: " ++ String.fromFloat hojd


viewSmhipunkt : Smhipunkt -> Element msg
viewSmhipunkt smhipunkt =
    Element.text <|
        "Smhipunkt: "
            ++ String.fromInt smhipunkt.punkt


viewRemoteData : (a -> Element msg) -> RemoteData.WebData a -> Element msg
viewRemoteData toElement data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "..."

        RemoteData.Failure _ ->
            text "FEL!"

        RemoteData.Success x ->
            toElement x


kartaView : Karta -> Element Msg
kartaView karta =
    column
        [ width fill
        , height fill
        , spacing 10
        ]
        [ kartlagervaljare karta
        , el
            [ Border.width 2
            , width fill
            , height fill
            ]
          <|
            Karta.toElement True karta
        ]


kartlagervaljare : Karta -> Element Msg
kartlagervaljare karta =
    Element.row [ Element.spacing 5, centerX ]
        [ lagerBtn "Ortofoto" karta Karta.Orto
        , lagerBtn "Karta" karta Karta.Topowebb
        , lagerBtn "Nedtonad karta " karta Karta.TopowebbNedtonad
        , lagerBtn "Terrängskuggning" karta Karta.Terrangskuggning
        ]


lagerBtn : String -> Karta -> Karta.Kartlager -> Element Msg
lagerBtn label karta kartlager =
    Element.Input.button
        [ Border.width 2
        , Border.rounded 14
        , Background.color (rgb255 200 255 200)
        , paddingXY 10 5
        , Font.size 17
        ]
        { label = Element.text label, onPress = Just (ValjKartlager karta kartlager) }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Karta.subscribe GotKartEvent


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
