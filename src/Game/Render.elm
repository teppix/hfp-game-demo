module Game.Render exposing (sprite, world)

import Game.Config as Config
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, style)
import String.Interpolate exposing (interpolate)


transform : List String -> Html.Attribute msg
transform parts =
    style "transform" (String.join " " parts)


translate : String -> String -> String
translate xstr ystr =
    "translate(" ++ xstr ++ "," ++ ystr ++ ")"


world : List (Html msg) -> Html msg
world contents =
    Html.div
        [ attribute "style" <|
            interpolate
                "--tile-size: {0}; --scale: {1}; --world-width: {2}; --world-height: {3};"
                [ String.fromInt Config.tileSize ++ "px"
                , String.fromInt Config.scale
                , String.fromInt Config.worldWidth
                , String.fromInt Config.worldHeight
                ]
        ]
        [ Html.div [ class "world" ] contents ]


sprite : String -> Bool -> ( Int, Int ) -> ( Int, Int ) -> Html msg
sprite imageSrc flip tileOffset worldPosition =
    let
        ( ox, oy ) =
            tileOffset

        ( x, y ) =
            worldPosition

        bgPosition =
            interpolate "calc(var(--tile-size)*{0}) calc(var(--tile-size)*{1})"
                [ String.fromInt ox
                , String.fromInt -oy
                ]

        translateX =
            interpolate "calc(var(--tile-size)*{0})" [ String.fromInt x ]

        translateY =
            interpolate "calc(-1*var(--tile-size)*{0})" [ String.fromInt y ]
    in
    Html.div
        [ class "globalVars sprite"
        , style "background" <| interpolate "url({0})" [ imageSrc ]
        , style "background-position" bgPosition
        , transform
            [ translate translateX translateY
            , if flip then
                "scale(-1,1)"

              else
                ""
            ]
        ]
        []
