module Lab exposing (main)

import Api.Smhi
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
    , dolj : Bool
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ValjKartlager Karta Kartlager
    | GotKartEvent Karta.Event
    | DoljKarta
    | GotSmhiPunkt (RemoteData.WebData String)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { karta = Nothing
      , message = "Klicka gärna lite i kartan!"
      , dolj = False
      }
    , Cmd.batch
        [ Karta.initiera "karta1"
        , Api.Smhi.sokSmhipunkt { x = 0, y = 0 } GotSmhiPunkt
        ]
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSmhiPunkt webData ->
            let
                m =
                    case webData of
                        RemoteData.NotAsked ->
                            "?"

                        RemoteData.Loading ->
                            "..."

                        RemoteData.Failure _ ->
                            "kunde inte hämta smhi-punkt"

                        RemoteData.Success punkt ->
                            "Smhipunkt: " ++ punkt
            in
            ( { model | message = m }, Cmd.none )

        DoljKarta ->
            ( { model | dolj = not model.dolj }, Cmd.none )

        ValjKartlager karta lager ->
            ( model, Karta.visaLager lager karta )

        GotKartEvent (Karta.Skapad karta) ->
            ( { model | karta = Just karta, message = "karta skapad" }
            , karta |> Karta.placeraKartnal { lat = 60.67570159984868, long = 17.17447728525011 }
            )

        GotKartEvent (Karta.KlickIKarta karta lat long) ->
            ( { model | message = Karta.identitet karta ++ ": " ++ String.fromFloat lat ++ ", " ++ String.fromFloat long }
            , karta |> Karta.placeraKartnal { lat = lat, long = long }
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
                ]
        ]
    }


kartaView : Bool -> Karta -> Element Msg
kartaView hide karta =
    column []
        [ Element.Input.button [] { label = text "Dölj", onPress = Just DoljKarta }
        , kartlagervaljare karta
        , Karta.toElement (not hide) karta
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
