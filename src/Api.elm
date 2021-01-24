module Api exposing (Fors, Lan, Vattendrag, hamtaForsar, hamtaLan, hamtaVattendrag)

import Auth exposing (hamtaAnvandare)
import Fors exposing (beskrivning)
import Http
import Json.Decode as D


type alias Fors =
    { id : Int, namn : String }


type alias Vattendrag =
    { id : Int
    , namn : String
    , beskrivning : String
    , lan : List Lan
    }


type alias Lan =
    { id : Int, namn : String }


hamtaForsar : (Result Http.Error (List Fors) -> msg) -> Cmd msg
hamtaForsar toMsg =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/forsstracka"
        , expect = Http.expectJson toMsg (D.field "forsstracka" (D.list forsDecoder))
        }


forsDecoder : D.Decoder Fors
forsDecoder =
    D.map2 Fors
        (D.field "id" D.int)
        (D.field "namn" D.string)


hamtaVattendrag : (Result Http.Error (List Vattendrag) -> msg) -> Cmd msg
hamtaVattendrag toMsg =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/vattendrag"
        , expect = Http.expectJson toMsg (D.field "vattendrag" (D.list vattendragDecoder))
        }


vattendragDecoder : D.Decoder Vattendrag
vattendragDecoder =
    D.map4 Vattendrag
        (D.field "id" D.int)
        (D.field "namn" D.string)
        (D.field "beskrivning" D.string)
        (D.field "lan" (D.list lanDecoder))


hamtaLan : (Result Http.Error (List Lan) -> msg) -> Cmd msg
hamtaLan toMsg =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/lan"
        , expect = Http.expectJson toMsg (D.field "lan" (D.list lanDecoder))
        }


lanDecoder : D.Decoder Lan
lanDecoder =
    D.map2 Lan
        (D.field "id" D.int)
        (D.field "namn" D.string)
