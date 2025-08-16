module View.Dialog exposing (Dialog, view)

import Html exposing (Html, div, button, text, h3)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode


type alias Dialog msg =
    { title : String
    , onClose : msg
    , content : Html msg
    }


view : Dialog msg -> Html msg
view dialogData =
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "dialog-title"
        , Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background-color" "rgba(0,0,0,0.5)"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "z-index" "1000"
        , Events.onClick dialogData.onClose
        ]
        [ div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.75rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "95%"
            , Attr.style "margin" "1rem"
            , Attr.style "max-height" "90vh"
            , Attr.style "overflow-y" "auto"
            , Events.stopPropagationOn "click" (Json.Decode.succeed ( dialogData.onClose, True ))
            ]
            [ div
                [ Attr.style "padding" "1.5rem"
                , Attr.style "border-bottom" "1px solid #e5e7eb"
                ]
                [ div
                    [ Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "center"
                    ]
                    [ h3
                        [ Attr.id "dialog-title"
                        , Attr.style "font-size" "1.25rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin" "0"
                        ]
                        [ text dialogData.title ]
                    , button
                        [ Events.onClick dialogData.onClose
                        , Attr.style "background" "none"
                        , Attr.style "border" "none"
                        , Attr.style "font-size" "1.5rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "color" "#6b7280"
                        ]
                        [ text "×" ]
                    ]
                ]
            , div
                [ Attr.style "padding" "1.5rem" ]
                [ dialogData.content ]
            ]
        ]
