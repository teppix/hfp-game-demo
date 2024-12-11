module Game.Input exposing (Key(..), sub)

import Browser.Events
import Json.Decode as Decode


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
