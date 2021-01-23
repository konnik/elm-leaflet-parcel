module Admin.Vattendrag exposing (Model, Msg, init, redigera, update, view)

import Api exposing (Vattendrag)
import Auth exposing (Session)
import Browser
import Element exposing (Element)
import Html exposing (..)


type alias Model =
    { vattendrag : Maybe Vattendrag
    }


type Msg
    = Msg1


init : Session -> ( Model, Cmd Msg )
init session =
    ( { vattendrag = Nothing }, Cmd.none )


redigera : Session -> Vattendrag -> ( Model, Cmd Msg )
redigera session vattendrag =
    ( { vattendrag = Just vattendrag }, Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    case model.vattendrag of
        Just vattendrag ->
            Element.text <| "Redigera " ++ vattendrag.namn ++ "."

        Nothing ->
            Element.text <| "Inget vattendrag valt."
