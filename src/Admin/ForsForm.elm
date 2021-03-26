module Admin.ForsForm exposing (Model, Msg, init, nytt, redigera, update, updateKarta, view)

import Api exposing (Fors, Grad, Lan, Resurs(..), Vattendrag)
import Auth exposing (Session)
import Dict exposing (Dict)
import Element exposing (Attribute, Element, alignRight, centerX, centerY, column, el, fill, height, maximum, minimum, padding, paddingEach, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (s)
import Http
import Karta exposing (Karta)
import Process
import String
import String.Verify exposing (isInt, notBlank)
import Task
import Verify exposing (Validator, fromMaybe, validate, verify)


type alias Model =
    { id : Maybe Int
    , form : Form
    , status : Status
    , meddelande : Maybe Meddelande
    , lan : Dict Int Lan
    , vattendrag : Dict Int Vattendrag
    , karta : Maybe Karta
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
    , langd : String
    , fallhojd : String
    , klass : String
    , lyft : String
    , koordinater : String
    , smhipunkt : String
    , minimum : String
    , optimal : String
    , maximum : String
    , lan : String
    , vattendrag : String
    }


type Msg
    = InputNamn String
    | InputLangd String
    | InputFallhojd String
    | InputKlass String
    | InputLyft String
    | InputKoordinater String
    | InputSmhipunkt String
    | InputMinimum String
    | InputOptimal String
    | InputMaximum String
    | InputVattendrag String
    | InputLan String
    | Spara
    | SparaResult (Result Http.Error (Resurs Fors))
    | Radera
    | RaderaBekraftad
    | RaderaAvbruten
    | RaderaResult (Result Http.Error ())
    | UppdateraModel Model


init : ( Model, Cmd Msg )
init =
    ( { id = Nothing
      , form = emptyForm
      , status = Inmatning
      , meddelande = Nothing
      , lan = Dict.empty
      , vattendrag = Dict.empty
      , karta = Nothing
      }
    , Cmd.none
    )


emptyForm : Form
emptyForm =
    { namn = ""
    , langd = ""
    , klass = ""
    , lyft = ""
    , fallhojd = ""
    , koordinater = ""
    , smhipunkt = ""
    , minimum = ""
    , optimal = ""
    , maximum = ""
    , lan = ""
    , vattendrag = ""
    }


nytt : Session -> List Lan -> List (Resurs Vattendrag) -> ( Model, Cmd Msg )
nytt session lan vattendrag =
    ( { id = Nothing
      , form = emptyForm
      , status = Inmatning
      , meddelande = Nothing
      , lan = Dict.fromList <| List.map (\l -> ( l.id, l )) lan
      , vattendrag = Dict.fromList <| List.map (\(Resurs id v) -> ( id, v )) vattendrag
      , karta = Nothing
      }
    , Karta.initiera "koordinater"
    )


redigera : Session -> List Lan -> List (Resurs Vattendrag) -> Resurs Fors -> ( Model, Cmd Msg )
redigera session lan vattendrag (Resurs id fors) =
    ( { id = Just id
      , form =
            { emptyForm
                | namn = fors.namn
                , langd = fors.langd |> String.fromInt
                , fallhojd = fors.fallhojd |> String.fromInt
                , klass = Api.gradToString fors.gradering.klass
                , lyft = fors.gradering.lyft |> List.map Api.gradToString |> String.join ", "
                , koordinater = [ fors.koordinater.lat, fors.koordinater.long ] |> List.map String.fromFloat |> String.join ", "
                , smhipunkt = fors.flode.smhipunkt |> String.fromInt
                , maximum = fors.flode.maximum |> String.fromInt
                , minimum = fors.flode.minimum |> String.fromInt
                , optimal = fors.flode.optimal |> String.fromInt
                , vattendrag = fors.vattendrag |> List.map (.id >> String.fromInt) |> String.join ", "
                , lan = fors.lan |> List.map (.id >> String.fromInt) |> String.join ", "
            }
      , status = Inmatning
      , meddelande = Nothing
      , lan = Dict.fromList <| List.map (\l -> ( l.id, l )) lan
      , vattendrag = Dict.fromList <| List.map (\(Resurs x v) -> ( x, v )) vattendrag
      , karta = Nothing
      }
    , Karta.initiera "koordinater"
    )


updateKarta : Karta.Event -> Model -> ( Model, Cmd Msg )
updateKarta kartevent model =
    case kartevent of
        Karta.Skapad karta ->
            ( { model | karta = Just karta }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Radera ->
            ( { model | status = BekraftaRadera }, Cmd.none )

        RaderaBekraftad ->
            case model.id of
                Just id ->
                    ( { model | status = Raderar }, Api.raderaFors session id RaderaResult )

                _ ->
                    ( model, Cmd.none )

        RaderaAvbruten ->
            ( { model | status = Inmatning }, Cmd.none )

        RaderaResult (Ok ()) ->
            ( model, fordrojUppdatering 1000 { model | id = Nothing, status = Raderad, meddelande = Just <| info "Forsen har raderats." } )

        RaderaResult (Err err) ->
            ( model, fordrojUppdatering 1000 { model | status = Inmatning, meddelande = Just <| fel "Det gick inte att radera forsen." } )

        Spara ->
            case validera model of
                Ok fors ->
                    ( { model | status = Sparar, meddelande = Nothing }
                    , case model.id of
                        Nothing ->
                            Api.nyFors session fors SparaResult

                        Just id ->
                            Api.uppdateraFors session (Resurs id fors) SparaResult
                    )

                Err valideringsFel ->
                    ( { model | status = Inmatning, meddelande = Just <| fel valideringsFel }, Cmd.none )

        SparaResult (Ok (Resurs id fors)) ->
            ( model, fordrojUppdatering 1000 { model | id = Just id, status = Inmatning, meddelande = Just <| info "Forsen har sparats." } )

        SparaResult (Err err) ->
            ( model, fordrojUppdatering 1000 { model | status = Inmatning, meddelande = Just <| fel "Det gick inte att spara forsen." } )

        -- form input
        InputNamn str ->
            model |> updateForm (\f -> { f | namn = str })

        InputLangd str ->
            model |> updateForm (\f -> { f | langd = str })

        InputFallhojd str ->
            model |> updateForm (\f -> { f | fallhojd = str })

        InputKlass str ->
            model |> updateForm (\f -> { f | klass = str })

        InputLyft str ->
            model |> updateForm (\f -> { f | lyft = str })

        InputKoordinater str ->
            model |> updateForm (\f -> { f | koordinater = str })

        InputSmhipunkt str ->
            model |> updateForm (\f -> { f | smhipunkt = str })

        InputMinimum str ->
            model |> updateForm (\f -> { f | minimum = str })

        InputMaximum str ->
            model |> updateForm (\f -> { f | maximum = str })

        InputOptimal str ->
            model |> updateForm (\f -> { f | optimal = str })

        InputVattendrag str ->
            model |> updateForm (\f -> { f | vattendrag = str })

        InputLan str ->
            model |> updateForm (\f -> { f | lan = str })

        UppdateraModel newModel ->
            ( newModel, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm updateFunc model =
    ( { model | form = updateFunc model.form }, Cmd.none )


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


validera : Model -> Result String Fors
validera model =
    formValidator model model.form
        |> Result.mapError (\( first, rest ) -> String.join " " (first :: rest))


tillFors : String -> Int -> Int -> Grad -> List Grad -> { lat : Float, long : Float } -> Int -> Int -> Int -> Int -> List Lan -> List { id : Int, namn : String } -> Fors
tillFors namn langd fallhojd klass lyft koordinater smhipunkt minimum maximum optimal lan vattendrag =
    { namn = namn
    , langd = langd
    , fallhojd = fallhojd
    , gradering =
        { klass = klass
        , lyft = lyft
        }
    , koordinater = koordinater
    , flode =
        { smhipunkt = smhipunkt
        , minimum = minimum
        , maximum = maximum
        , optimal = optimal
        }
    , vattendrag = vattendrag
    , lan = lan
    }


formValidator : { a | lan : Dict Int Lan, vattendrag : Dict Int Vattendrag } -> Verify.Validator String Form Fors
formValidator { lan, vattendrag } =
    validate tillFors
        |> verify .namn (notBlank "Namn måste anges")
        |> verify .langd (isInt "Längd måste vara ett heltal")
        |> verify .fallhojd (isInt "Fallhöjd måste vara ett heltal")
        |> verify .klass (validGrad (\gradStr -> "Ogiltig klass: " ++ gradStr))
        |> verify .lyft (validLyft (\gradStr -> "Ogiltig lyft: " ++ gradStr))
        |> verify .koordinater (validKoordinater "Felaktiga koordinater.")
        |> verify .smhipunkt (fromMaybe String.toInt "Smhipunkt måste vara ett heltal.")
        |> verify .minimum (fromMaybe String.toInt "Flöde min måste vara ett heltal.")
        |> verify .maximum (fromMaybe String.toInt "Flöde max måste vara ett heltal.")
        |> verify .optimal (fromMaybe String.toInt "Flöde optimal måste vara ett heltal.")
        |> verify .lan (valideraLanLista lan)
        |> verify .vattendrag (valideraVattendragLista vattendrag)


valideraVattendragLista : Dict Int Vattendrag -> Validator String String (List { id : Int, namn : String })
valideraVattendragLista giltigaVattendrag =
    validLista (validVattendrag giltigaVattendrag) (\error -> "Ogiltigt vattendrag: " ++ error)
        |> Verify.compose (ejTomLista "Ange minst ett vattendrag.")


valideraLanLista : Dict Int Lan -> Validator String String (List Lan)
valideraLanLista giltigaLan =
    validLista (validLan giltigaLan) (\error -> "Ogiltigt lan: " ++ error)
        |> Verify.compose (ejTomLista "Ange minst ett län.")


ejTomLista : error -> Validator error (List a) (List a)
ejTomLista error lista =
    case lista of
        [] ->
            Err ( error, [] )

        _ ->
            Ok lista


validVattendrag : Dict Int Vattendrag -> Int -> Result String { id : Int, namn : String }
validVattendrag d id =
    Dict.get id d
        |> Result.fromMaybe ("Vattendrag med id " ++ String.fromInt id ++ " saknas.")
        |> Result.map (\v -> { id = id, namn = v.namn })


validLan : Dict Int Lan -> Int -> Result String Lan
validLan d id =
    Dict.get id d |> Result.fromMaybe ("Län med id " ++ String.fromInt id ++ " saknas.")


validLista : (Int -> Result String a) -> (String -> error) -> Verify.Validator error String (List a)
validLista toLan toError inputStr =
    let
        parseLanId : String -> Result String a
        parseLanId idStr =
            String.toInt idStr
                |> Result.fromMaybe ("Felaktigt format: " ++ idStr)
                |> Result.andThen toLan

        results : List (Result String a)
        results =
            if String.trim inputStr == "" then
                []

            else
                String.split "," inputStr
                    |> List.map (String.trim >> parseLanId)

        oks : List a
        oks =
            results |> List.filterMap Result.toMaybe

        errors : List String
        errors =
            results
                |> List.filterMap
                    (\res ->
                        case res of
                            Ok _ ->
                                Nothing

                            Err err ->
                                Just err
                    )
    in
    if List.isEmpty errors then
        Ok oks

    else
        Err ( toError <| String.join ", " errors, [] )


validKoordinater : error -> Verify.Validator error String { lat : Float, long : Float }
validKoordinater error inputStr =
    let
        coords : List (Maybe Float)
        coords =
            inputStr
                |> String.split ","
                |> List.map (String.trim >> String.toFloat)
    in
    case coords of
        [ Just lat, Just long ] ->
            Ok { lat = lat, long = long }

        _ ->
            Err ( error, [] )


validLyft : (String -> error) -> Verify.Validator error String (List Grad)
validLyft toError inputStr =
    let
        results : List (Result ( String, List String ) Grad)
        results =
            if String.trim inputStr |> String.isEmpty then
                []

            else
                String.split "," inputStr
                    |> List.map String.trim
                    |> List.map (validGrad (\str -> "'" ++ str ++ "'"))

        oks : List Grad
        oks =
            results |> List.filterMap Result.toMaybe

        errors : List String
        errors =
            results
                |> List.filterMap
                    (\res ->
                        case res of
                            Ok _ ->
                                Nothing

                            Err ( first, rest ) ->
                                Just <| String.join "," (first :: rest)
                    )
    in
    if List.isEmpty errors then
        Ok oks

    else
        Err ( toError <| String.join ", " errors, [] )


validGrad : (String -> error) -> Verify.Validator error String Grad
validGrad toError inputStr =
    case Api.gradFromString inputStr of
        Just grad ->
            Ok grad

        Nothing ->
            Err ( toError inputStr, [] )


view : Model -> Element Msg
view model =
    column [ spacing 20, centerX, centerY, width (fill |> maximum 600) ]
        [ if model.status /= Raderad then
            redigeraView model

          else
            el [ Font.color (rgb255 0 100 0) ] <| text "Forsen har raderats."
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
        , formView model.karta model.form
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
            "Fors " ++ (maybeId |> Maybe.map (\id -> " (" ++ String.fromInt id ++ ")") |> Maybe.withDefault "( nytt )")
    in
    el [ Font.size 30 ] (text rubrik)


formView : Maybe Karta -> Form -> Element Msg
formView maybeKarta form =
    column [ spacing 20, width fill ]
        [ input "Namn" form.namn InputNamn
        , row [ spacing 20 ]
            [ input "Längd" form.langd InputLangd
            , input "Fallhöjd" form.fallhojd InputFallhojd
            ]
        , row [ spacing 20 ]
            [ input "Klass" form.klass InputKlass
            , input "Lyft" form.lyft InputLyft
            ]
        , input "Koordinater (lat, long)" form.koordinater InputKoordinater
        , Maybe.map Karta.toElement maybeKarta |> Maybe.withDefault (Element.text "Initerar karta...")
        , input "Smhipunkt" form.smhipunkt InputSmhipunkt
        , row [ spacing 20 ]
            [ input "Flöde min" form.minimum InputMinimum
            , input "Flöde max" form.maximum InputMaximum
            , input "Flöde optimal" form.optimal InputOptimal
            ]
        , input "Vattendrag" form.vattendrag InputVattendrag
        , input "Län" form.lan InputLan
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
