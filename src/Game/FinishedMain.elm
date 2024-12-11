module Game.FinishedMain exposing (..)

import Browser
import Html exposing (Html)
import Game.Config as Config
import Game.Input as Input
import Game.Render as Render


type Msg
    = KeyWasPressed Input.Key


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Player =
    { direction : Direction
    , position : ( Int, Int )
    }


type alias Model =
    { player : Player
    , orbs : List ( Int, Int )
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyWasPressed Input.sub


move : Direction -> ( Int, Int ) -> ( Int, Int )
move dir ( x, y ) =
    case dir of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


clamp : comparable -> comparable -> comparable -> comparable
clamp lower upper val =
    max lower (min upper val)


constrainToWorld : ( Int, Int ) -> ( Int, Int )
constrainToWorld ( x, y ) =
    ( clamp 0 (Config.worldWidth - 1) x
    , clamp 0 (Config.worldHeight - 1) y
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyWasPressed key ->
            let
                nextPlayer =
                    updatePlayer key model.player

                nextCrates =
                    updateCrates nextPlayer model.orbs
            in
            ( { model | player = nextPlayer, orbs = nextCrates }
            , Cmd.none
            )


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { player = { direction = Up, position = ( 4, 0 ) }
      , orbs = [ ( 6, 3 ), ( 3, 6 ) ]
      }
    , Cmd.none
    )


updatePlayer : Input.Key -> Player -> Player
updatePlayer key player =
    let
        direction =
            case key of
                Input.ArrowUp ->
                    Just Up

                Input.ArrowDown ->
                    Just Down

                Input.ArrowLeft ->
                    Just Left

                Input.ArrowRight ->
                    Just Right

                Input.Ignored _ ->
                    Nothing
    in
    case direction of
        Just direction_ ->
            { direction = direction_
            , position =
                player.position
                    |> move direction_
                    |> constrainToWorld
            }

        Nothing ->
            player


updateCrates : Player -> List ( Int, Int ) -> List ( Int, Int )
updateCrates player orbs =
    let
        f orb =
            if player.position == orb then
                move player.direction orb

            else
                orb
    in
    List.map f orbs


view : Model -> Html Msg
view model =
    Render.world <|
        List.concat
            [ [ viewPlayer model.player ]
            , viewOrbs model.orbs
            ]


viewPlayer : Player -> Html msg
viewPlayer { direction, position } =
    let
        ( offset, flip ) =
            case direction of
                Up ->
                    ( ( 0, 2 ), False )

                Down ->
                    ( ( 0, 0 ), False )

                Left ->
                    ( ( 0, 1 ), True )

                Right ->
                    ( ( 0, 1 ), False )
    in
    Render.sprite "./assets/hero1.png" flip offset position


viewOrbs : List ( Int, Int ) -> List (Html msg)
viewOrbs boxes =
    let
        f =
            Render.sprite "./assets/orb.png" False ( 0, 0 )
    in
    List.map f boxes
