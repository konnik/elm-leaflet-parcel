module Admin.Vattendrag exposing (Model, Msg, init, nytt, redigera, update, view)

import Api exposing (Lan, Vattendrag)
import Auth exposing (Session)
import Element exposing (Element, alignRight, column, el, fill, padding, px, rgb255, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


type Model
    = SkapaNy Form
    | Redigera Int Form
    | Sparar
    | SparadOk Vattendrag
    | SparadFel
    | BekraftaRadera Int Form
    | Raderar
    | RaderadOk
    | RaderadFel


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
    ( SkapaNy
        { namn = ""
        , beskrivning = ""
        , lan = ""
        }
    , Cmd.none
    )


redigera : Session -> Vattendrag -> ( Model, Cmd Msg )
redigera session vattendrag =
    ( Redigera vattendrag.id
        { namn = vattendrag.namn
        , beskrivning = vattendrag.beskrivning
        , lan = vattendrag.lan |> List.map (.id >> String.fromInt) |> String.join ", "
        }
    , Cmd.none
    )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case ( model, msg ) of
        ( Redigera id form, Radera ) ->
            ( BekraftaRadera id form, Cmd.none )

        ( BekraftaRadera id form, RaderaBekraftad ) ->
            ( Raderar, Api.raderaVattendrag session id RaderaResult )

        ( BekraftaRadera id form, RaderaAvbruten ) ->
            ( Redigera id form, Cmd.none )

        ( Raderar, RaderaResult (Ok ()) ) ->
            ( RaderadOk, Cmd.none )

        ( Raderar, RaderaResult (Err err) ) ->
            ( RaderadFel, Cmd.none )

        ( SkapaNy form, Spara ) ->
            validera -1 form
                |> Result.map
                    (\vattendrag ->
                        ( Sparar, Api.nyttVattendrag session vattendrag SparaResult )
                    )
                |> Result.withDefault ( model, Cmd.none )

        ( Redigera id form, Spara ) ->
            validera id form
                |> Result.map
                    (\vattendrag ->
                        ( Sparar, Api.uppdateraVattendrag session vattendrag SparaResult )
                    )
                |> Result.withDefault ( model, Cmd.none )

        ( Sparar, SparaResult (Ok vattendrag) ) ->
            --redigera session vattendrag
            ( SparadOk vattendrag, Cmd.none )

        ( Sparar, SparaResult (Err err) ) ->
            ( SparadFel, Cmd.none )

        -- form input
        ( Redigera id form, InputNamn str ) ->
            ( Redigera id { form | namn = str }, Cmd.none )

        ( Redigera id form, InputBeskr str ) ->
            ( Redigera id { form | beskrivning = str }, Cmd.none )

        ( Redigera id form, InputLan str ) ->
            ( Redigera id { form | lan = str }, Cmd.none )

        ( SkapaNy form, InputNamn str ) ->
            ( SkapaNy { form | namn = str }, Cmd.none )

        ( SkapaNy form, InputBeskr str ) ->
            ( SkapaNy { form | beskrivning = str }, Cmd.none )

        ( SkapaNy form, InputLan str ) ->
            ( SkapaNy { form | lan = str }, Cmd.none )

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
        SkapaNy form ->
            column [ spacing 20 ]
                [ el [ Font.size 30 ] (text "Lägg till nytt vattendrag")
                , input "Namn" form.namn InputNamn
                , input "Län" form.lan InputLan
                , textbox "Beskrivning" form.beskrivning InputBeskr
                , knapp "Spara" Spara
                ]

        Redigera id form ->
            column [ spacing 20 ]
                [ el [ Font.size 30 ] (text <| "Redigera vattendrag (" ++ String.fromInt id ++ ")")
                , input "Namn" form.namn InputNamn
                , input "Län" form.lan InputLan
                , textbox "Beskrivning" form.beskrivning InputBeskr
                , knapp "Spara" Spara
                , column [ spacing 20, width fill ]
                    [ el [ alignRight ] <| text "DANGER ZONE BELOW"
                    , el [ alignRight ] <| knapp "Radera" Radera
                    ]
                ]

        BekraftaRadera id form ->
            column [ spacing 20 ]
                [ text "Är du säker på att du vill radera vattendraget?"
                , row [ spacing 20 ]
                    [ knapp "Bekräfta radera" RaderaBekraftad
                    , knapp "Avbryt" RaderaAvbruten
                    ]
                ]

        Sparar ->
            text "Sparar..."

        SparadOk vattendrag ->
            text "Vattendraget har sparats."

        SparadFel ->
            text "Kunde inte spara vattendrag."

        Raderar ->
            text "Raderar..."

        RaderadOk ->
            text "Vattendraget har raderats."

        RaderadFel ->
            text "Kunde inte radera vattendrag."


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
