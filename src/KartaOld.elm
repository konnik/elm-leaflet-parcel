port module KartaOld exposing (invalidera)

import Json.Encode as E


port karta : E.Value -> Cmd msg


invalidera : Cmd msg
invalidera =
    karta <|
        E.object
            [ ( "type", E.string "invalidera" )
            ]
