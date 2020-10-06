module Main exposing (main)

import Browser
import Element exposing (Element, alignBottom, alignLeft, alignRight, centerX, centerY, column, el, fill, height, inFront, layout, padding, paragraph, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html, div, h1)
import Html.Attributes as HtmlAttr


type alias Model =
    { showSidebar : Bool }


type Msg
    = ToggleSidebar


init : () -> ( Model, Cmd Msg )
init _ =
    ( { showSidebar = False }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSidebar ->
            ( { model | showSidebar = not model.showSidebar }
            , Cmd.none
            )


white : Element.Color
white =
    rgb255 255 255 255


blue : Element.Color
blue =
    rgb255 45 124 172


lightBlue : Element.Color
lightBlue =
    rgb255 172 219 255


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column [ width fill, height fill ]
            [ header
            , mainContent model
            , footer
            ]


header : Element Msg
header =
    let
        testBtn : Element Msg
        testBtn =
            Input.button [] { label = text "test", onPress = Just ToggleSidebar }
    in
    row
        [ padding 10
        , width fill
        , height (px 50)
        , Border.widthEach { top = 0, bottom = 5, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ centerX, centerY ] (Element.text "Hello Forsguiden")
        , el [ alignRight ] testBtn
        ]


footer : Element msg
footer =
    el
        [ alignBottom
        , width fill
        , height (px 20)
        , Border.widthEach { top = 3, bottom = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        (el
            [ Font.size 10
            , centerX
            , centerY
            ]
            (text "© Elm Lovers")
        )


mainContent : Model -> Element msg
mainContent model =
    row
        [ width fill
        , height fill
        , Background.color white
        ]
    <|
        [ mapView ]
            ++ (if model.showSidebar then
                    [ sidebar ]

                else
                    []
               )


sidebar : Element msg
sidebar =
    el [ width (px 200), height fill, padding 20 ] <|
        text "Forsinfo"


mapView : Element msg
mapView =
    el
        [ width fill
        , height fill
        ]
        (Element.html mapHtml)


mapHtml : Html msg
mapHtml =
    Html.div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "z-index" "0"
        ]
        [ Html.div
            [ HtmlAttr.id "map"
            , HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "relative"
            , HtmlAttr.style "z-index" "0"
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
