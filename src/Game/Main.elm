module Game.Main exposing (main)

import Browser
import Browser.Events
import Game.Config as Config
import Game.Engine as Engine
import Html exposing (Html)
import Random


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
    Sub.map KeyPressed <| Browser.Events.onKeyDown Engine.keyDecoder


type Msg
    = KeyPressed Engine.Key
    | RandomNumbers (List Int)


type alias Point =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Player =
    { direction : Direction
    , position : Point
    }


type alias Model =
    { player : Player
    , orbs : List Point
    , randomNumbers : List Int
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { player = { direction = Up, position = ( 4, 0 ) }
      , orbs = []
      , randomNumbers = []
      }
      -- ! Makes the assumption that world width and height are the same
    , Random.generate RandomNumbers (Random.list (2 * Config.numberOfOrbs) (Random.int 0 (Config.worldWidth - 1)))
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed key ->
            let
                nextPlayer =
                    updatePlayer key model.player

                nextOrb =
                    updateOrbs nextPlayer model.orbs
            in
            ( { model | player = nextPlayer, orbs = nextOrb }
            , Cmd.none
            )

        RandomNumbers numbers ->
            ( { model | orbs = toTuples numbers }, Cmd.none )


updatePlayer : Engine.Key -> Player -> Player
updatePlayer key player =
    let
        direction =
            case key of
                Engine.ArrowUp ->
                    Just Up

                Engine.ArrowDown ->
                    Just Down

                Engine.ArrowLeft ->
                    Just Left

                Engine.ArrowRight ->
                    Just Right

                Engine.Ignored _ ->
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


updateOrbs : Player -> List Point -> List Point
updateOrbs player orbs =
    let
        orbAvoidPlayer : Point -> Point
        orbAvoidPlayer orb =
            if orb == move player.direction player.position then
                orb

            else
                orb
                    |> move (invertDirection player.direction)

        orbAvoidOrbs allOrbs orb =
            if List.any (\otherOrb -> otherOrb == orb) allOrbs then
                orb
                    |> move (invertDirection player.direction)
                    |> move (invertDirection player.direction)

            else
                orb

        f orb =
            if player.position == orb then
                move player.direction orb
                    |> constrainToWorld
                    |> orbAvoidPlayer
                    |> orbAvoidOrbs orbs

            else
                orb
    in
    List.map f orbs


move : Direction -> Point -> Point
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


invertDirection : Direction -> Direction
invertDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


constrainToWorld : Point -> Point
constrainToWorld ( x, y ) =
    ( clamp 0 (Config.worldWidth - 1) x
    , clamp 0 (Config.worldHeight - 1) y
    )


clamp : comparable -> comparable -> comparable -> comparable
clamp lower upper val =
    max lower (min upper val)


toTuples : List Int -> List Point
toTuples list =
    case list of
        a :: b :: rest ->
            ( a, b ) :: toTuples rest

        _ ->
            []



-- VIEW


view : Model -> Html Msg
view model =
    Engine.renderWorld <|
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
    Engine.renderSprite "./assets/hero1.png" flip offset position


viewOrbs : List Point -> List (Html msg)
viewOrbs boxes =
    let
        f =
            Engine.renderSprite "./assets/orb.png" False ( 0, 0 )
    in
    List.map f boxes
