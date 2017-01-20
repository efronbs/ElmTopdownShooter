module Main exposing (..)

import Html exposing(Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Keyboard

-- MAIN 

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Ship = {
    backX : Float 
    , backY : Float 
    , backR : Float
    , frontX : Float
    , frontY : Float
    , frontR : Float
    , velocityX : Float
    , velocityY : Float
}

type alias Model =
    {
        player : Ship
        , keypress : (HorizontalButtonState, VerticalButtonState)
        , oldTime : Maybe Time
    }

init : (Model, Cmd Msg)
init = 
    (
        {
            player = {
                backX = 250
                , backY = 350
                , backR = 25
                , frontX = 250
                , frontY = 325
                , frontR = 15
                , velocityX = 500
                , velocityY = 500
            }
            , keypress = (HNone, VNone)
            , oldTime = Nothing 
        }
        , Cmd.none
    )

-- UPDATE
type HorizontalButtonState
  = Left | Right | HBoth | HNone

type VerticalButtonState
    = Up | Down | VBoth | VNone

type Msg = 
    Key HorizontalButtonState VerticalButtonState | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Key h v -> 
            ({model | keypress = (h, v)} , Cmd.none)
        Tick newTime ->
            case model.oldTime of 
                Just oldTime ->
                    ({model | oldTime = Just newTime
                        , player = (updatePlayerLocation model (newTime - oldTime) )}, Cmd.none)
                Nothing -> 
                    ({model | oldTime = Just newTime}, Cmd.none)

updatePlayerLocation : Model -> Float -> Ship
updatePlayerLocation model timeDelta =
    let
        player = model.player
        (horizontalKeypress, verticallKeypress) = model.keypress
        horizVel = 
            case horizontalKeypress of 
                Left -> -1 * player.velocityX
                Right -> player.velocityX
                HBoth -> 0
                HNone -> 0
        vertVel = 
            case verticallKeypress of 
                Up -> -1 * player.velocityY
                Down -> player.velocityY
                VBoth -> 0
                VNone -> 0
    in
        {player | frontX = player.frontX + (timeDelta / 1000) * horizVel
            , backX = player.backX + (timeDelta / 1000) * horizVel
            , frontY = player.frontY + (timeDelta / 1000) * vertVel
            , backY = player.backY + (timeDelta / 1000) * vertVel}

-- VIEW
view : Model -> Html Msg
view model =
    Html.div [] [
       -- Html.hr [] []
        svg 
            [viewBox "0 0 900 650", width "900px"] 
            (ship model.player)
       -- , Html.hr [] [] 
    ]

ship : Ship -> List (Svg Msg)
ship s =  
    [
        circle [cx (toString s.backX), cy (toString s.backY), r (toString s.backR), fill "red"] []
       , circle [cx (toString s.frontX), cy (toString s.frontY), r (toString s.frontR), fill "red"] []
    ]

-- SUBSCRIPTIONS

-- 37 is left
-- 38 is up
-- 39 is right
-- 40 is down

handleDown : (Keyboard.KeyCode, (HorizontalButtonState, VerticalButtonState) ) -> Msg
handleDown state =
    case state of
        -- Horizontal keys
        (37, (Right, currentVertical)) -> Key HBoth currentVertical
        (37, (HNone, currentVertical)) -> Key Left currentVertical
        (39, (Left, currentVertical)) -> Key HBoth currentVertical
        (39, (HNone, currentVertical)) -> Key Right currentVertical

        -- Vertical keys
        (38, (currentHorizontal, Down)) -> Key currentHorizontal VBoth
        (38, (currentHorizontal, VNone)) -> Key currentHorizontal Up
        (40, (currentHorizontal, Up)) -> Key currentHorizontal VBoth
        (40, (currentHorizontal, VNone)) -> Key currentHorizontal Down
        -- Default
        (_, (currentHorizontal, currentVertical)) -> Key currentHorizontal currentVertical 

handleUp : (Keyboard.KeyCode, (HorizontalButtonState, VerticalButtonState) ) -> Msg
handleUp state =
    case state of
        -- Horizontal keys
        (37, (Left, currentVertical)) -> Key HNone currentVertical
        (37, (HBoth, currentVertical)) -> Key Right currentVertical
        (39, (Right, currentVertical)) -> Key HNone currentVertical
        (39, (HBoth, currentVertical)) -> Key Left currentVertical
        -- Vertical keys
        (38, (currentHorizontal, Up)) -> Key currentHorizontal VNone
        (38, (currentHorizontal, VBoth)) -> Key currentHorizontal Down
        (40, (currentHorizontal, Down)) -> Key currentHorizontal VNone
        (40, (currentHorizontal, VBoth)) -> Key currentHorizontal Up
        -- Default
        (_ , (currentHorizontal, currentVertical)) -> Key currentHorizontal currentVertical


-- on my browser keyboard presses does not work properly
-- so I'm using ups and downs
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Keyboard.downs (\k -> handleDown (k, model.keypress))
      , Keyboard.ups (\k -> handleUp (k, model.keypress))
      , Time.every (Time.millisecond * 40) Tick
     ]