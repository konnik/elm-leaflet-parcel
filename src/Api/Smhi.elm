module Api.Smhi exposing (sokSmhipunkt)

import Http
import Json.Decode as D
import RemoteData exposing (WebData)


sokSmhipunkt : { x : Float, y : Float } -> (WebData String -> msg) -> Cmd msg
sokSmhipunkt { x, y } toMsg =
    Http.get
        { url = "https://vattenwebb.smhi.se/hydronu/data/point?x=577050.6352558124&y=6760575.570486804"
        , expect = Http.expectJson (RemoteData.fromResult >> toMsg) smhiDecoder
        }


smhiDecoder : D.Decoder String
smhiDecoder =
    D.string
