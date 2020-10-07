module Korv exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Html exposing (Html)
import Html.Attributes


main : Html msg
main =
    layout
        [ height fill
        , width fill
        ]
        viewColFirst


viewColFirst : Element msg
viewColFirst =
    column
        [ height fill
        , width fill
        ]
        [ top
        , row
            [ height fill
            , width fill
            , Element.clip
            , Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            ]
            [ left, container, right ]
        , bottom
        ]


viewRowFirst : Element msg
viewRowFirst =
    row
        [ height fill
        , width fill
        ]
        [ left
        , column
            [ height fill
            , width fill
            ]
            [ top
            , container
            , bottom
            ]
        , right
        ]


left : Element msg
left =
    el
        [ width (px 60)
        , height fill
        , Bg.color (rgb255 255 0 255)
        , alignLeft
        ]
        (text
            "Left"
        )


right : Element msg
right =
    el
        [ width (px 60)
        , height fill
        , alignRight
        , Bg.color (rgb255 0 0 255)
        ]
        (text
            "Right"
        )


top : Element msg
top =
    el
        [ height (px 50)
        , width fill
        , alignBottom
        , Bg.color (rgb255 255 255 0)
        ]
        (text "Top")


bottom : Element msg
bottom =
    el
        [ height (px 50)
        , width fill
        , alignBottom
        , Bg.color (rgb255 255 0 0)
        ]
        (text "Bottom")


container : Element msg
container =
    el
        [ width fill
        , height fill
        , scrollbars
        ]
        mybox


mybox : Element msg
mybox =
    List.range 1 30
        |> List.map hello
        |> column
            [ width fill
            , height fill
            , Bg.color (rgb255 100 255 100)
            ]


hello : Int -> Element msg
hello n =
    el []
        (text <| "Hello " ++ String.fromInt n)
