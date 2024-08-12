module Point exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }


add : Point -> Point -> Point
add a b =
    Point
        (a.x + b.x)
        (a.y + b.y)


getMidpoint : Point -> Point -> Point
getMidpoint a b =
    { x = (a.x + b.x) / 2
    , y = (a.y + b.y) / 2
    }


scale : Float -> Point -> Point
scale c p =
    { x = p.x * c
    , y = p.y * c
    }
