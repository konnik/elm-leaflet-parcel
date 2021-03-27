module Api.Smhi exposing (Smhipunkt, sokSmhipunkt)

import Http
import Json.Decode as D
import Proj
import RemoteData exposing (WebData)


type alias Smhipunkt =
    { punkt : Int
    , koordinater :
        { lat : Float
        , long : Float
        }
    }


sokSmhipunkt : { lat : Float, long : Float } -> (WebData Smhipunkt -> msg) -> Cmd msg
sokSmhipunkt { lat, long } toMsg =
    let
        latRad =
            lat * pi / 180.0

        longRad =
            long * pi / 180.0

        { x, y } =
            Proj.fromLatLong { lat = latRad, long = longRad }
    in
    Http.get
        { url = "https://forsguiden-api.herokuapp.com/smhipunkt/?x=" ++ String.fromFloat y ++ "&y=" ++ String.fromFloat x
        , expect = Http.expectJson (RemoteData.fromResult >> toMsg) smhiDecoder
        }


smhiDecoder : D.Decoder Smhipunkt
smhiDecoder =
    D.map2 Smhipunkt
        (D.field "poi" D.int)
        (D.field "poiCenter" koordDecoder)


koordDecoder : D.Decoder { lat : Float, long : Float }
koordDecoder =
    D.list D.float
        |> D.andThen
            (\koords ->
                case koords of
                    [ x, y ] ->
                        let
                            { lat, long } =
                                Proj.fromGrid ( y, x )
                        in
                        D.succeed { lat = lat * 180 / pi, long = long * 180 / pi }

                    _ ->
                        D.fail "Oväntat antal element i poicenter."
            )
