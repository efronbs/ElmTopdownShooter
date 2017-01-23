module Main exposing (..)

import Html exposing(Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Keyboard
import Debug

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
    , bulletType : BulletType
}

type alias Model =
    {
        player : Ship
        , keypress : (HorizontalButtonState, VerticalButtonState)
        , oldTime : Maybe Time
        , bullets : List BUpdater
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
                , bulletType = LineBullet
            }
            , keypress = (HNone, VNone)
            , oldTime = Nothing 
            , bullets = []
        }, Cmd.none )

-- UPDATE
type HorizontalButtonState
  = Left | Right | HBoth | HNone

type VerticalButtonState
    = Up | Down | VBoth | VNone

type BulletType
    = LineBullet | TripleShot | Boomerang | AngleShot | Bomb

type Msg = 
    None | Key HorizontalButtonState VerticalButtonState | Tick Time | FireBullet | SetBullet BulletType


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

boomerangBulletUpdater : Float -> Float -> Float -> Float -> List BUpdater
boomerangBulletUpdater x y multiplier delta =
    let
        newMultiplier = if y <= 0 then multiplier * -1  else multiplier
        newX = x 
        newY = y + newMultiplier * 200 * (delta / 1000)
    in 
        [BUpdater (newX, newY) (boomerangBulletUpdater newX newY newMultiplier )]

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

playerFireBullet : Model -> Model
playerFireBullet model = 
    let bulletStartY = (model.player.frontY + 3 - model.player.frontR)
        bulletStartX = model.player.backX
        newBulletUpdater = 
            case model.player.bulletType of
                LineBullet -> lineBulletUpdater bulletStartX bulletStartY 0 200 0
                TripleShot -> tripleBulletCreate bulletStartX bulletStartY
                Boomerang -> boomerangBulletUpdater bulletStartX bulletStartY -1 0
                AngleShot -> angleShotCreate bulletStartX bulletStartY
                Bomb -> bombBulletUpdater bulletStartX bulletStartY (bulletStartY - 200) 0
    in 
        {model | bullets = List.append model.bullets newBulletUpdater} 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Key h v -> 
            ({model | keypress = (h, v)} , Cmd.none)

        Tick newTime ->
            (updateBoardState newTime model, Cmd.none)

        FireBullet -> (playerFireBullet model , Cmd.none)
        
        SetBullet btype -> 
            let newPlayer = 
                let player = model.player
                in {player | bulletType = btype}
            in ({model | player = newPlayer}, Cmd.none)       

        None -> (model, Cmd.none)

updateBoardState : Time -> Model -> Model
updateBoardState newTime model =
    case model.oldTime of 
        Just oldTime ->
            let newBullets = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.bullets)
                newPlayer = (updatePlayerLocation model (newTime - oldTime) )
            in
                {model | oldTime = Just newTime
                    , player = newPlayer
                    , bullets = newBullets
                    }
        Nothing -> 
            {model | oldTime = Just newTime}                                       

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
                    , List.map (\updater -> drawBullet updater) model.bullets
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

bulletOperation : Keyboard.KeyCode -> Model -> Msg
bulletOperation key model =
    case key of
        32 -> FireBullet
        113 -> SetBullet LineBullet --q for line
        119 -> SetBullet TripleShot --w for triple
        101 -> SetBullet Boomerang  --e for boomerang
        114 -> SetBullet AngleShot  --r for angle
        116 -> SetBullet Bomb       --t for bomb
        _ -> None

-- on my browser keyboard presses does not work properly
-- so I'm using ups and downs
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Keyboard.downs (\k -> handleDown (k, model.keypress))
      , Keyboard.ups (\k -> handleUp (k, model.keypress))
      , Keyboard.presses (\k -> bulletOperation k model)
      , Time.every (Time.millisecond * 40) Tick
     ]