module Game.Engine exposing (Key(..), renderSprite, sub, renderWorld)

import Browser.Events
import Game.Config as Config
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, style)
import Json.Decode as Decode
import String.Interpolate exposing (interpolate)


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Ignored String


sub : Sub Key
sub =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Key
keyDecoder =
    let
        fromKey keyCode =
            case keyCode of
                "ArrowUp" ->
                    ArrowUp

                "ArrowDown" ->
                    ArrowDown

                "ArrowLeft" ->
                    ArrowLeft

                "ArrowRight" ->
                    ArrowRight

                _ ->
                    Ignored keyCode
    in
    Decode.field "key" Decode.string |> Decode.map fromKey


transform : List String -> Html.Attribute msg
transform parts =
    style "transform" (String.join " " parts)


translate : String -> String -> String
translate xstr ystr =
    "translate(" ++ xstr ++ "," ++ ystr ++ ")"


renderWorld : List (Html msg) -> Html msg
renderWorld contents =
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


renderSprite : String -> Bool -> ( Int, Int ) -> ( Int, Int ) -> Html msg
renderSprite imageSrc flip tileOffset worldPosition =
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
