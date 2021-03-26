port module Karta exposing (Event(..), Karta, Kartlager(..), identitet, initiera, placeraKartnal, subscribe, toElement, visaLager)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Json.Encode as E


port kartaOutgoing : E.Value -> Cmd msg


port kartaIncoming : (D.Value -> msg) -> Sub msg


type Karta
    = Karta { id : String }


type Event
    = Skapad Karta
    | KlickIKarta Karta Float Float
    | Unknown String


type Kartlager
    = Orto
    | Topowebb
    | TopowebbNedtonad



-- frågor


identitet : Karta -> String
identitet (Karta { id }) =
    id



-- utgående kommandon


initiera : String -> Cmd msg
initiera id =
    kartaOutgoing <|
        E.object
            [ ( "typ", E.string "ny_karta" )
            , ( "id", E.string id )
            ]


visaLager : Kartlager -> Karta -> Cmd msg
visaLager lager (Karta { id }) =
    kartaOutgoing <|
        E.object
            [ ( "typ", E.string "visa_lager" )
            , ( "id", E.string id )
            , ( "lagernamn", E.string (lagernamn lager) )
            ]


placeraKartnal : { lat : Float, long : Float } -> Karta -> Cmd msg
placeraKartnal { lat, long } (Karta { id }) =
    kartaOutgoing <|
        E.object
            [ ( "typ", E.string "placera_kartnal" )
            , ( "id", E.string id )
            , ( "lat", E.float lat )
            , ( "long", E.float long )
            ]


toElement : Karta -> Element msg
toElement (Karta { id }) =
    Element.html <|
        Html.div
            [ HtmlAttr.id id
            , HtmlAttr.style "width" "600"
            , HtmlAttr.style "height" "300"
            , HtmlAttr.style "border" "2px solid red"
            ]
            []


subscribe : (Event -> msg) -> Sub msg
subscribe toMsg =
    Sub.map toMsg (kartaIncoming parseEvent)


parseEvent : D.Value -> Event
parseEvent value =
    let
        decoder : D.Decoder Event
        decoder =
            D.field "typ" D.string
                |> D.andThen
                    (\typ ->
                        case typ of
                            "karta_skapad" ->
                                D.map Skapad kartaDecoder

                            "klick_i_karta" ->
                                D.map3 KlickIKarta
                                    kartaDecoder
                                    (D.field "lat" D.float)
                                    (D.field "long" D.float)

                            _ ->
                                D.fail <| "Felaktig typ: " ++ typ
                    )
    in
    case D.decodeValue decoder value of
        Ok event ->
            event

        Err error ->
            Unknown <| D.errorToString error


kartaDecoder : D.Decoder Karta
kartaDecoder =
    D.field "id" D.string |> D.andThen (\id -> D.succeed (Karta { id = id }))


lagernamn : Kartlager -> String
lagernamn kartlager =
    case kartlager of
        Orto ->
            "ortofoto"

        Topowebb ->
            "topowebb_normal"

        TopowebbNedtonad ->
            "topowebb_nedtonad"
