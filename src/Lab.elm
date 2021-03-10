module Lab exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, column, el, fill, height, htmlAttribute, padding, px, row, spacing, text, width)
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import KartLab exposing (Kartlager(..))
import Url


type alias Model =
    { message : String }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ValjKartlager KartLab.Kartlager
    | GotKartEvent KartLab.Event


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { message = "!!!" }, KartLab.visaLager Topowebb )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValjKartlager lager ->
            ( model, KartLab.visaLager lager )

        GotKartEvent (KartLab.OnClick lat long) ->
            ( { model | message = String.fromFloat lat ++ ", " ++ String.fromFloat long }, Cmd.none )

        GotKartEvent (KartLab.Unknown error) ->
            ( { model | message = error }, Cmd.none )

        UrlRequested _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Lab"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ kartlagervaljare
                , Element.html (karta "karta1")
                , Element.text "korv korv korv"
                , Element.html (karta "karta2")
                , kartlagervaljare
                , Element.text model.message
                ]
        ]
    }


kartlagervaljare : Element Msg
kartlagervaljare =
    Element.row [ Element.spacing 30 ]
        [ lagerBtn "Orto" KartLab.Orto
        , lagerBtn "Topowebb" KartLab.Topowebb
        , lagerBtn "Topowebb nedtonad" KartLab.TopowebbNedtonad
        ]


lagerBtn : String -> KartLab.Kartlager -> Element Msg
lagerBtn label karlager =
    Element.Input.button
        []
        { label = Element.text label, onPress = Just (ValjKartlager karlager) }


karta : String -> Html msg
karta id =
    Html.div
        [ HtmlAttr.id id
        , HtmlAttr.style "width" "700"
        , HtmlAttr.style "height" "500"
        , HtmlAttr.style "border" "2px solid red"
        ]
        []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    KartLab.subscribe GotKartEvent


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
