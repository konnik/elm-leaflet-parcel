port module KartLab exposing (Event(..), Kartlager(..), subscribe, visaLager)

import Json.Decode as D
import Json.Encode as E


port send : E.Value -> Cmd msg


port receive : (D.Value -> msg) -> Sub msg


type Event
    = OnClick Float Float
    | Unknown String


type Kartlager
    = Orto
    | Topowebb
    | TopowebbNedtonad


subscribe : (Event -> msg) -> Sub msg
subscribe toMsg =
    Sub.map toMsg (receive parseEvent)


parseEvent : D.Value -> Event
parseEvent value =
    let
        decoder : D.Decoder Event
        decoder =
            D.field "typ" D.string
                |> D.andThen
                    (\typ ->
                        case typ of
                            "click" ->
                                D.map2 OnClick
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


visaLager : Kartlager -> Cmd msg
visaLager lager =
    send <|
        E.object
            [ ( "typ", E.string "visa_lager" )
            , ( "namn", E.string (kartlagerId lager) )
            ]


kartlagerId : Kartlager -> String
kartlagerId kartlager =
    case kartlager of
        Orto ->
            "ortofoto"

        Topowebb ->
            "topowebb_normal"

        TopowebbNedtonad ->
            "topowebb_nedtonad"
