module Lab exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, alignRight, column, el, fill, height, htmlAttribute, padding, px, row, spacing, text, width)
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import KartLab exposing (Karta, Kartlager(..))
import Url


type alias Model =
    { kartor : Dict String Karta
    , message : String
    , dolj : List Karta
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ValjKartlager Karta Kartlager
    | GotKartEvent KartLab.Event
    | NyKarta
    | DoljKarta Karta


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    let
        kartor =
            [ "karta1", "karta2" ] |> List.map (\x -> ( x, KartLab.skapa x )) |> Dict.fromList
    in
    ( { kartor = kartor
      , message = "Klicka gärna lite i kartan!"
      , dolj = []
      }
    , Dict.values kartor
        |> List.map KartLab.initiera
        |> Cmd.batch
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoljKarta karta ->
            let
                dolj =
                    if List.member karta model.dolj then
                        List.filter (\k -> k /= karta) model.dolj

                    else
                        karta :: model.dolj
            in
            ( { model | dolj = dolj }, Cmd.none )

        NyKarta ->
            let
                id =
                    "karta" ++ String.fromInt (Dict.size model.kartor + 1)

                karta =
                    KartLab.skapa id
            in
            ( { model | kartor = Dict.insert id karta model.kartor }, KartLab.initiera karta )

        ValjKartlager karta lager ->
            ( model, KartLab.visaLager lager karta )

        GotKartEvent (KartLab.KlickIKarta id lat long) ->
            ( { model | message = id ++ ": " ++ String.fromFloat lat ++ ", " ++ String.fromFloat long }, Cmd.none )

        GotKartEvent (KartLab.Unknown error) ->
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
                [ Element.Input.button []
                    { label = text "Lägg till karta!", onPress = Just NyKarta }
                , column [] <| List.map (\( id, k ) -> kartaView (List.member k model.dolj) id k) (Dict.toList model.kartor |> List.sortBy Tuple.first)
                , Element.text model.message
                ]
        ]
    }


kartaView : Bool -> String -> Karta -> Element Msg
kartaView hide rubrik karta =
    let
        display =
            if hide then
                "none"

            else
                "block"
    in
    column []
        [ Element.el [ Font.size 30 ] <| Element.text rubrik
        , Element.Input.button [] { label = text "Dölj", onPress = Just (DoljKarta karta) }
        , kartlagervaljare karta
        , el [ Element.htmlAttribute (Html.Attributes.style "display" display) ] <| KartLab.toElement karta
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
        [ lagerBtn "Orto" karta KartLab.Orto
        , lagerBtn "Topowebb" karta KartLab.Topowebb
        , lagerBtn "Topowebb nedtonad" karta KartLab.TopowebbNedtonad
        ]


lagerBtn : String -> Karta -> KartLab.Kartlager -> Element Msg
lagerBtn label karta kartlager =
    Element.Input.button
        []
        { label = Element.text label, onPress = Just (ValjKartlager karta kartlager) }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    KartLab.subscribe GotKartEvent


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
