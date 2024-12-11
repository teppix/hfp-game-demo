module Game.Render exposing (sprite, world)

import Html exposing (Html)
import Html.Attributes exposing (class, property, style)
import Json.Encode
import Game.Config as Config
import String.Interpolate exposing (interpolate)


transform : List String -> Html.Attribute msg
transform parts =
    style "transform" (String.join " " parts)


translate : String -> String -> String
translate xstr ystr =
    "translate(" ++ xstr ++ "," ++ ystr ++ ")"


css : String
css =
    [ String.fromInt Config.worldWidth
    , String.fromInt Config.worldHeight
    ]
        |> interpolate """
.globalVars {
    --tile-size: 16px;
}
.world {
    position: absolute;
    --scale: 4;
    --world-width: {0};
    --world-height: {1};

    top: 200px;
    left: 50%;

    width:  calc(var(--tile-size)
             * var(--world-height));

    height: calc(var(--tile-size)
              * var(--world-height));

    border: 1px solid black;
    position: relative;
    transform-origin: top left;
    transform: scale(var(--scale)) translateX(-50%);
    image-rendering: pixelated;
}

.sprite {
    position: absolute;
    bottom: 0;
    left: 0;
    width: var(--tile-size);
    height: var(--tile-size);
    transform-origin: 50% 50%;
}

.sprite-player {
    background: url('../assets/spritesheets/8 idle.png');
}

.sprite-crate {
    background: url('../assets/orb.png');
}
"""


styleSheet : Html msg
styleSheet =
    Html.node "style" [ property "innerText" (Json.Encode.string css) ] []


world : List (Html msg) -> Html msg
world contents =
    Html.div
        [ Html.Attributes.attribute "style"
            (interpolate
                "--tile-size: {0}; --scale: {1}; --world-width: {2}; --world-height: {3};"
                [ String.fromInt Config.tileSize ++ "px"
                , String.fromInt Config.scale
                , String.fromInt Config.worldWidth
                , String.fromInt Config.worldHeight
                ]
            )
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
