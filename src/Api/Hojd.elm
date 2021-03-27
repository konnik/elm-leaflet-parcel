module Api.Hojd exposing (Hojd, hamtaHojd)

import Http
import Json.Decode as D
import Proj
import RemoteData exposing (WebData)


type alias Hojd =
    Float


hamtaHojd : { lat : Float, long : Float } -> (WebData Hojd -> msg) -> Cmd msg
hamtaHojd { lat, long } toMsg =
    let
        latRad =
            lat * pi / 180.0

        longRad =
            long * pi / 180.0

        { x, y } =
            Proj.fromLatLong { lat = latRad, long = longRad }
    in
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/hojd/?north=" ++ String.fromFloat x ++ "&east=" ++ String.fromFloat y
        , expect = Http.expectJson (RemoteData.fromResult >> toMsg) hojdDecoder
        }


hojdDecoder : D.Decoder Hojd
hojdDecoder =
    D.field "hojd" D.float
