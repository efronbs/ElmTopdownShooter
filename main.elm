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
type BUpdater = BUpdater (Float,Float) (Float ->List BUpdater)

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
        , straightBullet : List BUpdater 
        , tripleBullet : List BUpdater
        , boomerangBullet : List BUpdater
        , angleBullet : List BUpdater
        , bombBullet : List BUpdater
    }

init : (Model, Cmd Msg)
init = 
    (
        {
            player = {
                backX = 800
                , backY = 600
                , backR = 25
                , frontX = 800
                , frontY = 575
                , frontR = 15
                , velocityX = 500
                , velocityY = 500
            }
            , keypress = (HNone, VNone)
            , oldTime = Nothing 
            , straightBullet = lineBulletUpdater 50 500 0 200 0
            , tripleBullet = tripleBulletCreate 150 500
            , boomerangBullet = boomerangBullerUpdater 250 500 -1 0
            , angleBullet = angleShotCreate 350 500
            , bombBullet = bombBulletUpdater 450 500 300 0
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


lineBulletUpdater : Float -> Float -> Float -> Float -> Float -> List BUpdater
lineBulletUpdater x y vx vy delta =
    let newY = y - vy * (delta / 1000)
        newX = x + vx * (delta / 1000)
    in [BUpdater (newX, newY) (lineBulletUpdater newX newY vx vy)]

tripleBulletCreate : Float -> Float -> List BUpdater
tripleBulletCreate x topY  =
    let midY = topY + 20
        botY = topY + 40
    in List.concat(
        [
            lineBulletUpdater x topY 0 200 0 
            , lineBulletUpdater x midY 0 200 0
            , lineBulletUpdater x botY 0 200 0
        ])

boomerangBullerUpdater : Float -> Float -> Float -> Float -> List BUpdater
boomerangBullerUpdater x y multiplier delta =
    let
        newMultiplier = if y <= 0 then multiplier * -1  else multiplier
        newX = x 
        newY = y + newMultiplier * 200 * (delta / 1000)
    in 
        [BUpdater (newX, newY) (boomerangBullerUpdater newX newY newMultiplier )]

angleShotCreate : Float -> Float -> List BUpdater
angleShotCreate x y =
    let centerXMultiplier = 0
        rightXMultiplier = 100
        leftXMultiplier = -100 
    in List.concat(
    [
        lineBulletUpdater x y centerXMultiplier 200 0 
        , lineBulletUpdater x y rightXMultiplier 200 0
        , lineBulletUpdater x y leftXMultiplier 200 0
    ])

bombBulletUpdater : Float -> Float -> Float -> Float -> List BUpdater
bombBulletUpdater x y goalY delta = 
    if y > goalY then
        let newY = y - 200 * (delta / 1000)
            newX = x
        in [BUpdater (newX, newY) (bombBulletUpdater newX newY goalY)]
    else
        List.concat([
                lineBulletUpdater x y 0 200 0 -- straight up
                , lineBulletUpdater x y 200 200 0 -- right up angle
                , lineBulletUpdater x y -200 200 0 -- left up angle
                , lineBulletUpdater x y 200 0 0 -- right horizontal
                , lineBulletUpdater x y -200 0 0 -- left horizontal
                , lineBulletUpdater x y 200 -200 0 -- right down angle
                , lineBulletUpdater x y -200 -200 0 -- left down angle
                , lineBulletUpdater x y 0 -200 0 -- straight down
            ])

updateBulletPos: Float -> BUpdater -> List BUpdater
updateBulletPos delta updater =
    case updater of
        BUpdater _ func -> func delta

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Key h v -> 
            ({model | keypress = (h, v)} , Cmd.none)
        Tick newTime ->
            case model.oldTime of 
                Just oldTime ->
                    ({model | oldTime = Just newTime
                        , player = (updatePlayerLocation model (newTime - oldTime) )
                        , straightBullet = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.straightBullet)
                        , tripleBullet = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.tripleBullet)
                        , boomerangBullet = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.boomerangBullet)
                        , angleBullet = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.angleBullet)
                        , bombBullet = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.bombBullet)
                        }, Cmd.none)
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
            (List.concat
                [
                    [
                        -- background
                        rect [x "0", y "0", width "900px", height "650px", fill "rgb(248,199,255)"] []
                        -- straight bullet
                    ]
                    , (ship model.player)
                    , List.map (\updater -> drawBullet updater) model.straightBullet
                    , List.map (\updater -> drawBullet updater) model.tripleBullet
                    , List.map (\updater -> drawBullet updater) model.boomerangBullet
                    , List.map (\updater -> drawBullet updater) model.angleBullet
                    , List.map (\updater -> drawBullet updater) model.bombBullet
                ]
            )
       -- , Html.hr [] []
    ]


drawBullet: BUpdater -> Svg Msg
drawBullet updater =
 case updater of
   BUpdater (x,y) _ -> circle [ cx (toString x), cy (toString y), r "5", fill "#0B79CE" ] []

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