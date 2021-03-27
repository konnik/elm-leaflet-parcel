module Lab exposing (main)

import Api.Hojd exposing (Hojd)
import Api.Smhi exposing (Smhipunkt)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignRight, column, el, fill, height, htmlAttribute, padding, px, row, spacing, text, width)
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
    | GotSmhiPunkt (RemoteData.WebData Smhipunkt)
    | GotHojd (RemoteData.WebData Hojd)


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
        GotSmhiPunkt smhipunkt ->
            ( { model | smhipunkt = smhipunkt }, Cmd.none )

        GotHojd hojd ->
            ( { model | hojd = hojd }, Cmd.none )

        DoljKarta ->
            ( { model | dolj = not model.dolj }, Cmd.none )

        ValjKartlager karta lager ->
            ( model, Karta.visaLager lager karta )

        GotKartEvent (Karta.Skapad karta) ->
            ( { model | karta = Just karta, message = "karta skapad" }
            , karta |> Karta.placeraKartnal { lat = 60.67570159984868, long = 17.17447728525011 }
            )

        GotKartEvent (Karta.KlickIKarta karta lat long) ->
            ( { model
                | message = "Du klickade på koordinat " ++ String.fromFloat lat ++ ", " ++ String.fromFloat long
                , hojd = RemoteData.Loading
                , smhipunkt = RemoteData.Loading
              }
            , Cmd.batch
                [ karta |> Karta.placeraKartnal { lat = lat, long = long }
                , Api.Smhi.sokSmhipunkt { lat = lat, long = long } GotSmhiPunkt
                , Api.Hojd.hamtaHojd { lat = lat, long = long } GotHojd
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
        [ Element.layout [] <|
            Element.column []
                [ el []
                    (Maybe.map (kartaView model.dolj) model.karta
                        |> Maybe.withDefault (Element.text "Initierar kartan...")
                    )
                , Element.text model.message
                , viewRemoteData viewHojd model.hojd
                , viewRemoteData viewSmhipunkt model.smhipunkt
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
            ++ " ("
            ++ String.fromFloat smhipunkt.koordinater.lat
            ++ ", "
            ++ String.fromFloat smhipunkt.koordinater.long
            ++ ")"


viewRemoteData : (a -> Element msg) -> RemoteData.WebData a -> Element msg
viewRemoteData toElement data =
    case data of
        RemoteData.NotAsked ->
            text "Not asked..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure _ ->
            text "FEL!"

        RemoteData.Success x ->
            toElement x


kartaView : Bool -> Karta -> Element Msg
kartaView hide karta =
    column [ width (px 700), height (px 500) ]
        [ Element.Input.button [] { label = text "Dölj", onPress = Just DoljKarta }
        , kartlagervaljare karta
        , el [ width fill, height fill ] <| Karta.toElement (not hide) karta
        , text
            (if hide then
                "DOLD"

             else
                "SYNLIG"
            )
        ]


kartlagervaljare : Karta -> Element Msg
kartlagervaljare karta =
    Element.row [ Element.spacing 30 ]
        [ lagerBtn "Orto" karta Karta.Orto
        , lagerBtn "Topowebb" karta Karta.Topowebb
        , lagerBtn "Topowebb nedtonad" karta Karta.TopowebbNedtonad
        ]


lagerBtn : String -> Karta -> Karta.Kartlager -> Element Msg
lagerBtn label karta kartlager =
    Element.Input.button
        []
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
