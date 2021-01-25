module Api exposing (Fors, Lan, Vattendrag, hamtaForsar, hamtaLan, hamtaVattendrag, nyttVattendrag, uppdateraVattendrag)

import Auth exposing (Session)
import Http
import Json.Decode as D
import Json.Encode as E
import OAuth


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



-- forsar


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



-- vattendrag


hamtaVattendrag : (Result Http.Error (List Vattendrag) -> msg) -> Cmd msg
hamtaVattendrag toMsg =
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/vattendrag"
        , expect = Http.expectJson toMsg (D.field "vattendrag" (D.list vattendragDecoder))
        }


nyttVattendrag : Session -> Vattendrag -> (Result Http.Error Vattendrag -> msg) -> Cmd msg
nyttVattendrag session vattendrag toMsg =
    let
        lanJson : E.Value
        lanJson =
            vattendrag.lan
                |> E.list
                    (\x ->
                        E.object
                            [ ( "id", E.int x.id )
                            , ( "namn", E.string x.namn )
                            ]
                    )

        body =
            E.object <|
                [ ( "id", E.int vattendrag.id )
                , ( "namn", E.string vattendrag.namn )
                , ( "beskrivning", E.string vattendrag.beskrivning )
                , ( "lan", lanJson )
                ]
    in
    Http.request
        { method = "POST"
        , url = "https://forsguiden-api.herokuapp.com/vattendrag"
        , headers = OAuth.useToken session.token []
        , expect = Http.expectJson toMsg vattendragDecoder
        , body = Http.jsonBody body
        , timeout = Nothing
        , tracker = Nothing
        }


uppdateraVattendrag : Session -> Vattendrag -> (Result Http.Error Vattendrag -> msg) -> Cmd msg
uppdateraVattendrag session vattendrag toMsg =
    let
        lanJson : E.Value
        lanJson =
            vattendrag.lan
                |> E.list
                    (\x ->
                        E.object
                            [ ( "id", E.int x.id )
                            , ( "namn", E.string x.namn )
                            ]
                    )

        body =
            E.object <|
                [ ( "id", E.int vattendrag.id )
                , ( "namn", E.string vattendrag.namn )
                , ( "beskrivning", E.string vattendrag.beskrivning )
                , ( "lan", lanJson )
                ]
    in
    Http.request
        { method = "PUT"
        , url = "https://forsguiden-api.herokuapp.com/vattendrag/" ++ String.fromInt vattendrag.id
        , headers = OAuth.useToken session.token []
        , expect = Http.expectJson toMsg vattendragDecoder
        , body = Http.jsonBody body
        , timeout = Nothing
        , tracker = Nothing
        }


vattendragDecoder : D.Decoder Vattendrag
vattendragDecoder =
    D.map4 Vattendrag
        (D.field "id" D.int)
        (D.field "namn" D.string)
        (D.field "beskrivning" D.string)
        (D.field "lan" (D.list lanDecoder))



-- län


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
