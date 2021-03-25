module Main exposing (main)

import Browser
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, inFront, layout, padding, paragraph, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fors
import Html exposing (Attribute, Html, div, h1)
import Html.Attributes as HtmlAttr
import KartaOld
import Lab


type alias Model =
    { showSidebar : Bool }


type Msg
    = ToggleSidebar


init : () -> ( Model, Cmd Msg )
init _ =
    ( { showSidebar = False }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSidebar ->
            ( { model | showSidebar = not model.showSidebar }
            , KartaOld.invalidera
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
            Input.button
                [ Border.width 1
                , Border.rounded 5
                , padding 5
                , Bg.color lightBlue
                ]
                { label = text "Forsinfo", onPress = Just ToggleSidebar }
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
        , Bg.color white
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
    row
        [ width (px 300)
        , height fill
        , Element.clip
        , Element.htmlAttribute (HtmlAttr.style "flex-shrink" "1")
        ]
        [ el [ width (px 5), Bg.color blue, height fill ] Element.none
        , el
            [ padding 20
            , alignTop
            , width fill
            , height fill
            , Element.scrollbarY
            ]
          <|
            Fors.beskrivning
        ]


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
