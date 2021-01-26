module Admin.Vattendrag exposing (Model, Msg, init, nytt, redigera, update, view)

import Api exposing (Lan, Vattendrag)
import Auth exposing (Session)
import Element exposing (Attribute, Element, alignRight, centerX, column, el, fill, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


type alias Model =
    { id : Maybe Int
    , form : Form
    , status : Status
    , meddelande : Maybe Meddelande
    }


type Status
    = Inmatning
    | Sparar
    | BekraftaRadera
    | Raderar
    | Raderad


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
    ( { id = Nothing
      , form =
            { namn = ""
            , beskrivning = ""
            , lan = ""
            }
      , status = Inmatning
      , meddelande = Nothing
      }
    , Cmd.none
    )


redigera : Session -> Vattendrag -> ( Model, Cmd Msg )
redigera session vattendrag =
    ( { id = Just vattendrag.id
      , form =
            { namn = vattendrag.namn
            , beskrivning = vattendrag.beskrivning
            , lan = vattendrag.lan |> List.map (.id >> String.fromInt) |> String.join ", "
            }
      , status = Inmatning
      , meddelande = Nothing
      }
    , Cmd.none
    )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Radera ->
            ( { model | status = BekraftaRadera }, Cmd.none )

        RaderaBekraftad ->
            case model.id of
                Just id ->
                    ( { model | status = Raderar }, Api.raderaVattendrag session id RaderaResult )

                _ ->
                    ( model, Cmd.none )

        RaderaAvbruten ->
            ( { model | status = Inmatning }, Cmd.none )

        RaderaResult (Ok ()) ->
            ( { model | id = Nothing, status = Raderad, meddelande = Just <| Info "Vattendraget har raderats." }, Cmd.none )

        RaderaResult (Err err) ->
            ( { model | status = Inmatning, meddelande = Just <| Fel "Det gick inte att radera vattendraget." }, Cmd.none )

        Spara ->
            case validera model.id model.form of
                Ok vattendrag ->
                    ( { model | status = Sparar }
                    , if vattendrag.id == -1 then
                        Api.nyttVattendrag session vattendrag SparaResult

                      else
                        Api.uppdateraVattendrag session vattendrag SparaResult
                    )

                Err felmeddelande ->
                    ( { model | status = Inmatning, meddelande = Just <| Fel felmeddelande }, Cmd.none )

        SparaResult (Ok vattendrag) ->
            --redigera session vattendrag
            ( { model | id = Just vattendrag.id, status = Inmatning, meddelande = Just <| Info "Vattendraget har sparats." }, Cmd.none )

        SparaResult (Err err) ->
            ( { model | status = Inmatning, meddelande = Just <| Fel "Det gick inte att spara vattendraget." }, Cmd.none )

        -- form input
        InputNamn str ->
            let
                f =
                    model.form
            in
            ( { model | form = { f | namn = str } }, Cmd.none )

        InputBeskr str ->
            let
                f =
                    model.form
            in
            ( { model | form = { f | beskrivning = str } }, Cmd.none )

        InputLan str ->
            let
                f =
                    model.form
            in
            ( { model | form = { f | lan = str } }, Cmd.none )


validera : Maybe Int -> Form -> Result String Vattendrag
validera maybeId form =
    let
        lan =
            form.lan
                |> String.split ","
                |> List.map String.trim
                |> List.filterMap String.toInt
                |> List.map (\id_ -> Lan id_ "")
    in
    Ok
        { id = maybeId |> Maybe.withDefault -1
        , namn = form.namn
        , beskrivning = form.beskrivning
        , lan = lan
        }


view : Model -> Element Msg
view model =
    column [ spacing 20, width fill ]
        [ model.meddelande |> Maybe.map meddelandeView |> Maybe.withDefault Element.none
        , if model.status /= Raderad then
            redigeraView model

          else
            Element.none
        ]


redigeraView : Model -> Element Msg
redigeraView model =
    column [ spacing 20 ]
        [ rubrikView model.id
        , formView model.form
        , knappSpara model.status
        , if model.id /= Nothing then
            viewRadera model.status

          else
            Element.none
        ]


meddelandeView : Meddelande -> Element msg
meddelandeView meddelande =
    let
        msgBox : List (Attribute msg) -> String -> Element msg
        msgBox attrs msg =
            el (attrs ++ [ width fill, padding 20, Border.width 2, Border.rounded 10 ]) <| text msg
    in
    case meddelande of
        Info msg ->
            msgBox [ Bg.color (rgb255 200 255 200) ] msg

        Fel msg ->
            msgBox [ Bg.color (rgb255 255 200 200) ] msg


rubrikView : Maybe Int -> Element Msg
rubrikView maybeId =
    let
        rubrik =
            "Vattendrag " ++ (maybeId |> Maybe.map (\id -> " (" ++ String.fromInt id ++ ")") |> Maybe.withDefault "( nytt )")
    in
    el [ Font.size 30 ] (text rubrik)


formView : Form -> Element Msg
formView form =
    column [ spacing 20 ]
        [ input "Namn" form.namn InputNamn
        , input "Län" form.lan InputLan
        , textbox "Beskrivning" form.beskrivning InputBeskr
        ]


viewRadera : Status -> Element Msg
viewRadera status =
    case status of
        BekraftaRadera ->
            row [ spacing 20, alignRight ]
                [ knapp "Bekräfta radera" (Just RaderaBekraftad)
                , knapp "Avbryt" (Just RaderaAvbruten)
                ]

        Raderar ->
            el [ alignRight ] <| knapp "Raderar..." Nothing

        _ ->
            el [ alignRight ] <| knapp "Radera" (Just Radera)


knappSpara : Status -> Element Msg
knappSpara status =
    case status of
        Inmatning ->
            knapp "Spara" (Just Spara)

        Sparar ->
            knapp "Sparar..." Nothing

        _ ->
            knapp "Spara" Nothing


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
