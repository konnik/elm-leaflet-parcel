module Main exposing (main)

import Element exposing (Element, alignBottom, alignLeft, alignRight, centerX, centerY, column, el, fill, height, inFront, layout, padding, paragraph, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Attribute, Html, div, h1)
import Html.Attributes as HtmlAttr


white : Element.Color
white =
    rgb255 255 255 255


blue : Element.Color
blue =
    rgb255 45 124 172


lightBlue : Element.Color
lightBlue =
    rgb255 172 219 255


main : Html msg
main =
    layout [ width fill, height fill ] <|
        column [ width fill, height fill ]
            [ header
            , mainContent
            , footer
            ]


header : Element msg
header =
    el
        [ width fill
        , height (px 50)
        , Border.widthEach { top = 0, bottom = 5, left = 0, right = 0 }
        , Border.color blue
        ]
        (el [ centerX, centerY ] (Element.text "Forsguiden PoC"))


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


mainContent : Element msg
mainContent =
    el
        [ width fill
        , height fill
        , Background.color white
        ]
        mapView


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
