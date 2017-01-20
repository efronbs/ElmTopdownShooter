module SpaceShip exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MODEL

type alias ShipHitbox = {
    x : Int
    , y : Int
    , r : Int
}

type alias Ship = {
    back : ShipHitbox
    , front : ShipHitbox
}

-- VIEW
ship : Ship -> Svg Msg
ship s = 
    
        circle [cx (toString s.back.x), cy (toString s.back.y), r (toString s.back.r), fill "red"] []
       -- , circle [cx (toString s.front.x), cy (toString s.front.y), r (toString s.front.r), fill "red"] []
    

-- UPDATE 


type Msg 
    = None