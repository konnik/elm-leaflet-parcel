module Admin.Vattendrag exposing (Model, Msg, init, nytt, redigera, update, view)

import Api exposing (Lan, Vattendrag)
import Auth exposing (Session)
import Element exposing (Element, alignRight, column, el, fill, padding, px, rgb255, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


type Model
    = Redigera (Maybe Int) Form Status
    | BekraftaRadera Int Form
    | Raderar
    | RaderadOk
    | RaderadFel


type Status
    = Inmatning
    | Sparar


type Meddelande
    = Info String
    | Fel String


type alias Form =
    { namn : String
    , beskrivning : String
    , lan : String
    }


type Msg
    = InputNamn String
    | InputBeskr String
    | InputLan String
    | Spara
    | SparaResult (Result Http.Error Vattendrag)
    | Radera
    | RaderaBekraftad
    | RaderaAvbruten
    | RaderaResult (Result Http.Error ())


init : Session -> ( Model, Cmd Msg )
init =
    nytt


nytt : Session -> ( Model, Cmd Msg )
nytt session =
    ( Redigera Nothing
        { namn = ""
        , beskrivning = ""
        , lan = ""
        }
        Inmatning
    , Cmd.none
    )


redigera : Session -> Vattendrag -> ( Model, Cmd Msg )
redigera session vattendrag =
    ( Redigera (Just vattendrag.id)
        { namn = vattendrag.namn
        , beskrivning = vattendrag.beskrivning
        , lan = vattendrag.lan |> List.map (.id >> String.fromInt) |> String.join ", "
        }
        Inmatning
    , Cmd.none
    )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( model, msg ) of
        ( Redigera (Just id) form Inmatning, Radera ) ->
            ( BekraftaRadera id form, Cmd.none )

        ( BekraftaRadera id form, RaderaBekraftad ) ->
            ( Raderar, Api.raderaVattendrag session id RaderaResult )

        ( BekraftaRadera id form, RaderaAvbruten ) ->
            ( Redigera (Just id) form Inmatning, Cmd.none )

        ( Raderar, RaderaResult (Ok ()) ) ->
            ( RaderadOk, Cmd.none )

        ( Raderar, RaderaResult (Err err) ) ->
            ( RaderadFel, Cmd.none )

        ( Redigera Nothing form Inmatning, Spara ) ->
            validera -1 form
                |> Result.map
                    (\vattendrag ->
                        ( Redigera Nothing form Sparar, Api.nyttVattendrag session vattendrag SparaResult )
                    )
                |> Result.withDefault ( model, Cmd.none )

        ( Redigera (Just id) form Inmatning, Spara ) ->
            validera id form
                |> Result.map
                    (\vattendrag ->
                        ( Redigera (Just id) form Sparar, Api.uppdateraVattendrag session vattendrag SparaResult )
                    )
                |> Result.withDefault ( model, Cmd.none )

        ( Redigera _ form _, SparaResult (Ok vattendrag) ) ->
            --redigera session vattendrag
            ( Redigera (Just vattendrag.id) form Inmatning, Cmd.none )

        ( Redigera id form _, SparaResult (Err err) ) ->
            ( Redigera id form Inmatning, Cmd.none )

        -- form input
        ( Redigera id form Inmatning, InputNamn str ) ->
            ( Redigera id { form | namn = str } Inmatning, Cmd.none )

        ( Redigera id form Inmatning, InputBeskr str ) ->
            ( Redigera id { form | beskrivning = str } Inmatning, Cmd.none )

        ( Redigera id form Inmatning, InputLan str ) ->
            ( Redigera id { form | lan = str } Inmatning, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


validera : Int -> Form -> Result String Vattendrag
validera id form =
    let
        lan =
            form.lan
                |> String.split ","
                |> List.map String.trim
                |> List.filterMap String.toInt
                |> List.map (\id_ -> Lan id_ "")
    in
    Ok
        { id = id
        , namn = form.namn
        , beskrivning = form.beskrivning
        , lan = lan
        }


view : Model -> Element Msg
view model =
    case model of
        Redigera Nothing form status ->
            column [ spacing 20 ]
                [ el [ Font.size 30 ] (text "Lägg till nytt vattendrag")
                , input "Namn" form.namn InputNamn
                , input "Län" form.lan InputLan
                , textbox "Beskrivning" form.beskrivning InputBeskr
                , knappSpara status
                ]

        Redigera (Just id) form status ->
            column [ spacing 20 ]
                [ el [ Font.size 30 ] (text <| "Redigera vattendrag (" ++ String.fromInt id ++ ")")
                , input "Namn" form.namn InputNamn
                , input "Län" form.lan InputLan
                , textbox "Beskrivning" form.beskrivning InputBeskr
                , knappSpara status
                , column [ spacing 20, width fill ]
                    [ el [ alignRight ] <| text "DANGER ZONE BELOW"
                    , el [ alignRight ] <| knapp "Radera" (Just Radera)
                    ]
                ]

        BekraftaRadera id form ->
            column [ spacing 20 ]
                [ text "Är du säker på att du vill radera vattendraget?"
                , row [ spacing 20 ]
                    [ knapp "Bekräfta radera" (Just RaderaBekraftad)
                    , knapp "Avbryt" (Just RaderaAvbruten)
                    ]
                ]

        Raderar ->
            text "Raderar..."

        RaderadOk ->
            text "Vattendraget har raderats."

        RaderadFel ->
            text "Kunde inte radera vattendrag."


knappSpara : Status -> Element Msg
knappSpara status =
    case status of
        Inmatning ->
            knapp "Spara" (Just Spara)

        Sparar ->
            knapp "Sparar..." Nothing


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


knapp : String -> Maybe msg -> Element msg
knapp label maybeToMsg =
    Input.button [ Border.width 1, padding 10, alignRight ]
        { onPress = maybeToMsg
        , label = text label
        }
