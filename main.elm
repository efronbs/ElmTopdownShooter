module Main exposing (..)

import Html exposing(Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Random
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
type BUpdater = BUpdater (Float,Float) (Float -> List BUpdater)

type EnemyUpdater = EnemyUpdater (Float, Float, String) (Float -> List EnemyUpdater) 

type alias Ship = {
    backX : Float 
    , backY : Float 
    , backR : Float
    , frontX : Float
    , frontY : Float
    , frontR : Float
    , baseVelocityX : Float
    , baseVelocityY : Float
    , velocityX : Float
    , velocityY : Float
    , bulletType : BulletType
}

type alias Model =
    {
        background : List (Float, Float, Float) --list of stars in the form x, y, radius
        , player : Ship
        , keypress : (HorizontalButtonState, VerticalButtonState)
        , oldTime : Maybe Time
        , bullets : List BUpdater
        , enemies : List EnemyUpdater
    }

init : (Model, Cmd Msg)
init = 
    (
        {
            background = [(200, 200, 2)]
            , player = {
                backX = 800
                , backY = 600
                , backR = 25
                , frontX = 800
                , frontY = 575
                , frontR = 15
                , baseVelocityX = 500
                , baseVelocityY = 500
                , velocityX = 0
                , velocityY = 0
                , bulletType = LineBullet
            }
            , keypress = (HNone, VNone)
            , oldTime = Nothing 
            , bullets = []
            , enemies = []
        }, Cmd.none )

-- UPDATE
type HorizontalButtonState
  = Left | Right | HBoth | HNone

type VerticalButtonState
    = Up | Down | VBoth | VNone

type BulletType
    = LineBullet | TripleShot | Boomerang | AngleShot | Bomb

type Msg = 
    None | Key HorizontalButtonState VerticalButtonState | Tick Time | FireBullet | SetBullet BulletType | StarCreate (Float, Float) 
    | EnemyCreate (Int, Float, Float, Float)

distance : (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) =
    sqrt((y2 - y1)^2 + (x2 - x1)^2 )

lineEnemyUpdater : Float -> Float -> Float -> Float -> Float -> List EnemyUpdater
lineEnemyUpdater x y vx vy delta = 
    let newX = x + vx * (delta / 1000)
        newY = y - vy * (delta / 1000)
    in [EnemyUpdater (newX, newY, "red") (lineEnemyUpdater newX newY vx vy)]

zigzagEnemyUpdater : Float -> Float -> Float -> Float -> Float -> List EnemyUpdater
zigzagEnemyUpdater x y vx vy delta = 
    let newXVel = if x <= 0 || x >= 900 then -1 * vx else vx
        newX = x + newXVel * (delta / 1000)
        newY = y - vy * (delta / 1000)
    in [EnemyUpdater (newX, newY, "blue") (zigzagEnemyUpdater newX newY newXVel vy)]

sineEnemyCreate : Float -> Float -> List EnemyUpdater
sineEnemyCreate x y = 
    sineEnemyUpdate x y x y 20 0

sineEnemyUpdate : Float -> Float -> Float -> Float -> Int -> Float -> List EnemyUpdater
sineEnemyUpdate x y initialX initialY limit delta = 
    let newX = x + delta / 10
        newY = initialY + 100*sin (deltaX/20)
        deltaX = newX - initialX
        behindShip = if limit <= 0 then [] else [EnemyUpdater (newX, newY, "purple") (sineEnemyUpdate x y initialX initialY (limit - 1))]
    in  if limit <= 0 then [EnemyUpdater (newX, newY, "purple") (sineEnemyUpdate newX newY initialX initialY 0)]
        else 
            [
                EnemyUpdater (newX, newY, "purple") (sineEnemyUpdate newX newY initialX initialY 0)
                , EnemyUpdater (newX, newY, "purple") (sineEnemyUpdate x y initialX initialY (limit - 1))
            ]

rushEnemyCreate : Float -> Float -> List EnemyUpdater
rushEnemyCreate x y =
    rushEnemyUpdate x y (y + 200) 0

rushEnemyUpdate : Float -> Float -> Float -> Float -> List EnemyUpdater
rushEnemyUpdate x y limit delta =
    if y <= limit then 
        [EnemyUpdater (x, y + 20, "yellow") (rushEnemyUpdate x (y + 20) limit)]
    else
        let xVel = if x > 450 then -600 else 600 
            yVel = -1200
        in [EnemyUpdater (x, y, "yellow") (lineEnemyUpdater x y xVel yVel)]

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

updateEnemyPos: Float -> EnemyUpdater -> List EnemyUpdater
updateEnemyPos delta updater =
    case updater of
        EnemyUpdater _ func -> func delta

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

starPairGenerator : Random.Generator (Float, Float)
starPairGenerator =
    Random.pair (Random.float 0 2) (Random.float 0 900)

createNewStar : (Float, Float) -> Model -> List (Float, Float, Float)
createNewStar result model = 
    let (radius, location) = result
    in [(location, 5, radius)]

enemyPairGenerator : Random.Generator (Int, Float, Float, Float)
enemyPairGenerator =
     Random.map4 (,,,) (Random.int 0 100) (Random.float 0 1) (Random.float 0 900) (Random.float 0 10)

createNewEnemy : (Int, Float, Float, Float) -> List EnemyUpdater
createNewEnemy (shipToSpawn, exists, xStart, xVel) =
    let newShip =
        if shipToSpawn < 60 then
            let x = xStart
                y = 0
                newXVel = if xStart >= 450 then -30 * xVel else 30 * xVel
            in lineEnemyUpdater x y newXVel -700 0
        else if shipToSpawn < 80 then 
            let x = xStart
                y = 0
                newXVel = if xStart >= 450 then -500 else 500
                newYVel = -300
            in zigzagEnemyUpdater x y newXVel newYVel 0
        else if shipToSpawn < 90 then 
            let x = 0
                y = xStart
            in sineEnemyCreate x y
        else if shipToSpawn <= 100 then
            let x = xStart
                y = 0
            in rushEnemyCreate x y
        else []
    in if exists > 0.1 then [] else newShip


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Key h v -> 
            ({model | keypress = (h, v)},  Cmd.none)

        Tick newTime ->
            (checkPlayerCollisions (checkEnemyCollisions (garbageCollectEnemies (garbageCollectBullets (garbageCollectStars (updateBoardState newTime model))))), 
                Cmd.batch [
                    (Random.generate StarCreate starPairGenerator)
                    , (Random.generate EnemyCreate enemyPairGenerator)
                ])

        FireBullet -> (playerFireBullet model , Cmd.none)
        
        SetBullet btype -> 
            let newPlayer = 
                let player = model.player
                in {player | bulletType = btype}
            in ({model | player = newPlayer}, Cmd.none)       

        StarCreate result ->
            ({model | background = List.append model.background (createNewStar result model)}, Cmd.none)

        EnemyCreate result ->
            ({model | enemies = List.append model.enemies (createNewEnemy result)}, Cmd.none)
        
        None -> (model, Cmd.none)

checkPlayerCollisions : Model -> Model
checkPlayerCollisions model =
    let remainingEnemies 
        = List.foldl (\(EnemyUpdater (ex, ey, color) efunc) currentAliveEnemies->
                            let topCollision = 
                                    if (distance (ex, ey - 20) (model.player.frontX, model.player.frontY)) <= player.frontR
                                        then True
                                        else False
                                bottomCollision = 
                                    if (distance (ex, ey- 20) (model.player.backX, model.player.backY)) <= player.backR
                                        then True
                                        else False
                            in 
                                if topCollision || bottomCollision 
                                    then currentAliveEnemies
                                    else (EnemyUpdater (ex, ey, color) efunc) :: currentAliveEnemies) [] model.enemies
        player = model.player
        newPlayer = 
        if List.length remainingEnemies == List.length model.enemies 
            then player
            else {player | frontX = -1000, frontY = -1000, backX = -1000, backY = -1000}
    in {model | enemies=remainingEnemies, player=newPlayer} 

checkEnemyCollisions : Model -> Model
checkEnemyCollisions model = 
    let (remainingBullets, remainingEnemies) = recursivelyCheckEnemyCollisions model.bullets model.enemies
    in {model | enemies= Debug.log "remaining enemies: " remainingEnemies, bullets= Debug.log "remaining bullets: " remainingBullets}

recursivelyCheckEnemyCollisions : List BUpdater -> List EnemyUpdater -> (List BUpdater, List EnemyUpdater)
recursivelyCheckEnemyCollisions aliveBullets aliveEnemies =
    let topBullet = List.head aliveBullets
    in  case topBullet of
            -- Base case
            Nothing -> 
                ([], aliveEnemies)
            Just (BUpdater (bx, by) bfunc) ->
                let singleParseAliveEnemies = 
                    (List.foldl (\(EnemyUpdater (ex, ey, color) efunc) currentAliveEnemies-> 
                        if (distance (bx, by) (ex, ey - 20)) <= 15 then currentAliveEnemies else (EnemyUpdater (ex, ey, color) efunc) :: currentAliveEnemies
                    ) [] aliveEnemies)
                    bulletAlive = if List.length singleParseAliveEnemies == List.length aliveEnemies then [(BUpdater (bx, by) bfunc)] else []
                    remainingBullets = 
                        let
                            tailBullets = List.tail aliveBullets
                        in 
                            case tailBullets of
                                Nothing -> []
                                Just tailBullets -> tailBullets
                    (upBullets, upEnemies) = (recursivelyCheckEnemyCollisions remainingBullets singleParseAliveEnemies)
                in (List.append bulletAlive upBullets, upEnemies)


garbageCollectStars : Model -> Model
garbageCollectStars model = 
    let newBackground = List.foldl (\(x, y, rad) currentList -> 
            if y > 1000 then currentList else (x, y, rad) :: currentList) [] model.background
    in {model | background = newBackground}

garbageCollectBullets : Model -> Model
garbageCollectBullets model = 
    let newBullets = List.foldl (\(BUpdater (x, y) func) currentList -> 
            if y > 750 || y < -100 || x > 1000 || x < -100 then currentList else (BUpdater (x,y) func) :: currentList) [] model.bullets
    in {model | bullets = newBullets}

garbageCollectEnemies : Model -> Model
garbageCollectEnemies model = 
    let newEnemies = List.foldl (\(EnemyUpdater (x, y, color) func) currentList -> 
            if y > 900 || y < -300 || x > 1200 || x < -300 then currentList else (EnemyUpdater (x,y, color) func) :: currentList) [] model.enemies
    in {model | enemies = newEnemies}

updateBoardState : Time -> Model -> Model
updateBoardState newTime model =
    case model.oldTime of 
        Just oldTime ->
            let newBullets = List.concat (List.map (\updater -> updateBulletPos (newTime - oldTime) updater) model.bullets)
                newPlayer = (updatePlayerLocation (updatePlayerVelocity model) (newTime - oldTime) )
                newEnemies = List.concat (List.map (\updater -> updateEnemyPos (newTime - oldTime) updater) model.enemies)
                newBackground = List.map (\(x, y, rad) -> (x, y + 10, rad)) model.background
            in
                {model | oldTime = Just newTime
                    , player = newPlayer
                    , bullets = newBullets
                    , background = newBackground
                    , enemies = newEnemies
                    }
        Nothing -> 
            {model | oldTime = Just newTime}
                                       
wallCheck : (HorizontalButtonState, VerticalButtonState) -> Model -> (Float, Float)
wallCheck (h, v) model =
    -- still fall off right side - no idea why
    -- let newBaseXVel = if (Debug.log "current x: " model.player.backX >= 900 && Debug.log "direction " h == Right) || (model.player.backX <= 0 && h == Left) then 0 else model.player.baseVelocityX
    let newBaseXVel = if (model.player.backX >= 900 && h == Right) || (model.player.backX <= 0 && h == Left) then 0 else model.player.baseVelocityX
        newBaseYVel = if (model.player.frontY <= 0 && v == Up) || (model.player.backY >= 650 && v == Down) then 0 else model.player.baseVelocityY
    in (newBaseXVel, newBaseYVel)

updatePlayerVelocity : Model -> Model
updatePlayerVelocity model = 
    let newPlayer = 
        let player = model.player
            (newBaseXVel, newBaseYVel) = wallCheck model.keypress model
            (h ,v) = model.keypress
            horizVel = 
                case h of 
                    Left -> -1 * newBaseXVel
                    Right -> newBaseYVel
                    HBoth -> 0
                    HNone -> 0
            vertVel = 
                case v of 
                    Up -> -1 * newBaseYVel
                    Down -> newBaseYVel
                    VBoth -> 0
                    VNone -> 0
        in {player | velocityX = horizVel, velocityY=vertVel}
    in {model | player = newPlayer}

updatePlayerLocation : Model -> Float -> Ship
updatePlayerLocation model timeDelta =
    let
        player = model.player
    in
        {player | frontX = player.frontX + (timeDelta / 1000) * player.velocityX
            , backX = player.backX + (timeDelta / 1000) * player.velocityX
            , frontY = player.frontY + (timeDelta / 1000) * player.velocityY
            , backY = player.backY + (timeDelta / 1000) * player.velocityY}

-- VIEW
view : Model -> Html Msg
view model =
    Html.div [] [
       -- Html.hr [] []
        svg 
            [viewBox "0 0 900 650", width "900px"] 
            (List.concat
                [
                    drawBackground model
                    , (ship model.player)
                    , List.map (\enemy -> drawEnemy enemy) model.enemies
                    , List.map (\updater -> drawBullet updater) model.bullets
                ]
            )
       -- , Html.hr [] []
    ]

drawBackground : Model -> List (Svg Msg)
drawBackground model = 
        (rect [x "0", y "0", width "900px", height "650px", fill "black"] [])
            :: List.map (\(x, y, radius) -> circle [ cx (toString x), cy (toString y), r (toString radius), fill "white"] []) model.background

drawBullet: BUpdater -> Svg Msg
drawBullet updater =
 case updater of
   BUpdater (x,y) _ -> circle [ cx (toString x), cy (toString y), r "5", fill "#0B79CE" ] []

drawEnemy : EnemyUpdater -> Svg Msg
drawEnemy updater =
    case updater of 
        EnemyUpdater (x, y, color) _ ->
            let (topX, topY) = (x,y)
                (leftX, leftY) = (x - 20 , y - 40)
                (rightX, rightY) = (x + 20, y - 40)
            in
                polygon [ points (String.concat [(toString topX) , "," , (toString topY) , " " 
                    , (toString leftX) , "," , (toString leftY) , " " 
                , (toString rightX) , "," , (toString rightY)]), fill color] []

ship : Ship -> List (Svg Msg)
ship s =  
    [
        circle [cx (toString s.backX), cy (toString s.backY), r (toString s.backR), fill "green"] []
       , circle [cx (toString s.frontX), cy (toString s.frontY), r (toString s.frontR), fill "green"] []
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