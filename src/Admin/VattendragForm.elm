module Admin.VattendragForm exposing (Model, Msg, init, nytt, redigera, update, view)

import Api exposing (Lan, Resurs(..), Vattendrag)
import Auth exposing (Session)
import Element exposing (Attribute, Element, alignRight, centerX, centerY, column, el, fill, height, maximum, minimum, padding, paddingEach, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Process
import Task


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


type alias Meddelande =
    { typ : MeddelandeTyp
    , text : String
    }


type MeddelandeTyp
    = Info
    | Fel


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
    | SparaResult (Result Http.Error (Resurs Vattendrag))
    | Radera
    | RaderaBekraftad
    | RaderaAvbruten
    | RaderaResult (Result Http.Error ())
    | UppdateraModel Model


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


redigera : Session -> Resurs Vattendrag -> ( Model, Cmd Msg )
redigera session (Resurs id vattendrag) =
    ( { id = Just id
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
            ( model, fordrojUppdatering 1000 { model | id = Nothing, status = Raderad, meddelande = Just <| info "Vattendraget har raderats." } )

        RaderaResult (Err err) ->
            ( model, fordrojUppdatering 1000 { model | status = Inmatning, meddelande = Just <| fel "Det gick inte att radera vattendraget." } )

        Spara ->
            case validera model.form of
                Ok vattendrag ->
                    ( { model | status = Sparar, meddelande = Nothing }
                    , case model.id of
                        Nothing ->
                            Api.nyttVattendrag session vattendrag SparaResult

                        Just id ->
                            Api.uppdateraVattendrag session (Resurs id vattendrag) SparaResult
                    )

                Err valideringsFel ->
                    ( { model | status = Inmatning, meddelande = Just <| fel valideringsFel }, Cmd.none )

        SparaResult (Ok (Resurs id vattendrag)) ->
            --redigera session vattendrag
            ( model, fordrojUppdatering 1000 { model | id = Just id, status = Inmatning, meddelande = Just <| info "Vattendraget har sparats." } )

        SparaResult (Err err) ->
            ( model, fordrojUppdatering 1000 { model | status = Inmatning, meddelande = Just <| fel "Det gick inte att spara vattendraget." } )

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

        UppdateraModel newModel ->
            ( newModel, Cmd.none )


fordrojUppdatering : Float -> Model -> Cmd Msg
fordrojUppdatering millis model =
    let
        task =
            Process.sleep millis
                |> Task.map (\_ -> model)
    in
    Task.perform UppdateraModel task


info : String -> Meddelande
info text =
    { text = text, typ = Info }


fel : String -> Meddelande
fel text =
    { text = text, typ = Fel }


validera : Form -> Result String Vattendrag
validera form =
    let
        lan =
            form.lan
                |> String.split ","
                |> List.map String.trim
                |> List.filterMap String.toInt
                |> List.map (\id_ -> Lan id_ "")
    in
    Ok
        { namn = form.namn
        , beskrivning = form.beskrivning
        , lan = lan
        }


view : Model -> Element Msg
view model =
    column [ spacing 20, centerX, centerY, width (fill |> maximum 600) ]
        [ if model.status /= Raderad then
            redigeraView model

          else
            el [ Font.color (rgb255 0 100 0) ] <| text "Vattendraget har raderats."
        ]


redigeraView : Model -> Element Msg
redigeraView model =
    let
        omId : Element Msg -> Element Msg
        omId elem =
            if model.id /= Nothing then
                elem

            else
                Element.none
    in
    column [ spacing 20, width fill ]
        [ rubrikView model.id
        , formView model.form
        , row [ spacing 20, width fill ]
            [ viewRadera model.status |> omId
            , knappSpara model.status
            ]
        , model.meddelande |> Maybe.map meddelandeView |> Maybe.withDefault Element.none
        ]


meddelandeView : Meddelande -> Element msg
meddelandeView meddelande =
    let
        msgBox : List (Attribute msg) -> String -> Element msg
        msgBox attrs msg =
            el (attrs ++ [ alignRight ]) <| text msg
    in
    case meddelande.typ of
        Info ->
            msgBox [ Font.color (rgb255 0 100 0) ] meddelande.text

        Fel ->
            msgBox [ Font.color (rgb255 200 0 0) ] meddelande.text


rubrikView : Maybe Int -> Element Msg
rubrikView maybeId =
    let
        rubrik =
            "Vattendrag " ++ (maybeId |> Maybe.map (\id -> " (" ++ String.fromInt id ++ ")") |> Maybe.withDefault "( nytt )")
    in
    el [ Font.size 30 ] (text rubrik)


formView : Form -> Element Msg
formView form =
    column [ spacing 20, width fill ]
        [ input "Namn" form.namn InputNamn
        , input "Län" form.lan InputLan
        , textbox "Beskrivning" form.beskrivning InputBeskr
        ]


viewRadera : Status -> Element Msg
viewRadera status =
    case status of
        BekraftaRadera ->
            row [ spacing 20, alignRight ]
                [ knapp { label = "Bekräfta radera", state = Aktiv } RaderaBekraftad
                , knapp { label = "Avbryt", state = Aktiv } RaderaAvbruten
                ]

        Raderar ->
            row [ spacing 20, alignRight ]
                [ knapp { label = "Raderar...", state = Spinning } RaderaAvbruten
                , knapp { label = "Avbryt", state = Inaktiverad } RaderaAvbruten
                ]

        Inmatning ->
            el [ alignRight ] <| knapp { label = "Radera", state = Aktiv } Radera

        _ ->
            el [ alignRight ] <| knapp { label = "Radera", state = Inaktiverad } Radera


knappSpara : Status -> Element Msg
knappSpara status =
    case status of
        Inmatning ->
            knapp { label = "Spara", state = Aktiv } Spara

        Sparar ->
            knapp { label = "Sparar...", state = Spinning } Spara

        _ ->
            knapp { label = "Spara", state = Inaktiverad } Spara


input : String -> String -> (String -> msg) -> Element msg
input label value toMsg =
    Input.text [ width fill ]
        { label = Input.labelAbove [] (text label)
        , placeholder = Nothing
        , onChange = toMsg
        , text = value
        }


textbox : String -> String -> (String -> msg) -> Element msg
textbox label value toMsg =
    Input.multiline [ width fill ]
        { onChange = toMsg
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] (text label)
        , spellcheck = False
        }


type KnappState
    = Aktiv
    | Inaktiverad
    | Spinning


knapp : { label : String, state : KnappState } -> msg -> Element msg
knapp { label, state } toMsg =
    Input.button
        [ Font.center
        , Border.width 1
        , paddingEach { left = 10, right = 10, top = 0, bottom = 0 }
        , alignRight
        , width (shrink |> minimum 100)
        , height (px 40)
        ]
        { onPress =
            if state == Aktiv then
                Just toMsg

            else
                Nothing
        , label =
            case state of
                Spinning ->
                    Element.image [ centerX, centerY, width (px 30) ] { src = "/spinner.svg", description = label }

                Aktiv ->
                    el [ centerX, centerY ] <| text label

                Inaktiverad ->
                    el [ centerX, centerY, Font.color (rgb255 200 200 200) ] <| text label
        }
