module Admin.Vattendrag exposing (Model, Msg, init, nytt, redigera, update, view)

import Api exposing (Lan, Vattendrag)
import Auth exposing (Session)
import Element exposing (Element, alignRight, column, el, padding, px, rgb255, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


type alias Model =
    { id : Maybe Int
    , namn : String
    , beskrivning : String
    , lan : String
    }


type Msg
    = InputNamn String
    | InputBeskr String
    | InputLan String
    | Spara
    | SparaResult (Result Http.Error Vattendrag)


init : Session -> ( Model, Cmd Msg )
init =
    nytt


nytt : Session -> ( Model, Cmd Msg )
nytt session =
    ( { id = Nothing, namn = "", beskrivning = "", lan = "" }, Cmd.none )


redigera : Session -> Vattendrag -> ( Model, Cmd Msg )
redigera session vattendrag =
    ( { id = Just vattendrag.id
      , namn = vattendrag.namn
      , beskrivning = vattendrag.beskrivning
      , lan = vattendrag.lan |> List.map (.id >> String.fromInt) |> String.join ", "
      }
    , Cmd.none
    )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Spara ->
            validera model
                |> Result.map
                    (\vattendrag ->
                        case model.id of
                            Just _ ->
                                ( model, Api.uppdateraVattendrag session vattendrag SparaResult )

                            Nothing ->
                                ( model, Api.nyttVattendrag session vattendrag SparaResult )
                    )
                |> Result.withDefault ( model, Cmd.none )

        SparaResult (Ok vattendrag) ->
            redigera session vattendrag

        SparaResult (Err err) ->
            ( model, Cmd.none )

        InputNamn str ->
            ( { model | namn = str }, Cmd.none )

        InputBeskr str ->
            ( { model | beskrivning = str }, Cmd.none )

        InputLan str ->
            ( { model | lan = str }, Cmd.none )


validera : Model -> Result String Vattendrag
validera model =
    let
        lan =
            model.lan
                |> String.split ","
                |> List.map String.trim
                |> List.filterMap String.toInt
                |> List.map (\id -> Lan id "")
    in
    Ok
        { id = model.id |> Maybe.withDefault -1
        , namn = model.namn
        , beskrivning = model.beskrivning
        , lan = lan
        }


view : Model -> Element Msg
view model =
    let
        rubrik =
            model.id |> Maybe.map (always "Redigera vattendrag") |> Maybe.withDefault "Nytt vattendrag"
    in
    column [ spacing 20 ]
        [ el [ Font.size 30 ] (text rubrik)
        , input "Namn" model.namn InputNamn
        , input "Län" model.lan InputLan
        , textbox "Beskrivning" model.beskrivning InputBeskr
        , knapp "Spara" Spara
        ]


input : String -> String -> (String -> msg) -> Element msg
input label value toMsg =
    Input.text [ width (px 600) ]
        { label = Input.labelAbove [] (text label)
        , placeholder = Nothing
        , onChange = toMsg
        , text = value
        }


textbox : String -> String -> (String -> msg) -> Element msg
textbox label value toMsg =
    Input.multiline [ width (px 600) ]
        { onChange = toMsg
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] (text label)
        , spellcheck = False
        }


knapp : String -> msg -> Element msg
knapp label toMsg =
    Input.button [ Border.width 1, padding 10, alignRight ]
        { onPress = Just toMsg
        , label = text label
        }
