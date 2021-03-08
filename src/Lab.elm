module Lab exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, column, el, fill, height, htmlAttribute, padding, px, row, spacing, text, width)
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Url


type alias Model =
    {}


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( {}, Cmd.none )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Lab"
    , body =
        [ Html.text "lkökl"
        , karta
        ]
    }


karta : Html msg
karta =
    Html.div
        [ HtmlAttr.id "map"
        , HtmlAttr.style "width" "700"
        , HtmlAttr.style "height" "800"
        , HtmlAttr.style "border" "2px solid red"
        ]
        []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
