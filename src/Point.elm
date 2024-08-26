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


sub : Point -> Point -> Point
sub a b =
    Point
        (a.x - b.x)
        (a.y - b.y)


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


interpolate : Float -> Point -> Point -> Point
interpolate f a b =
    { x = interpolateFloat f a.x b.x
    , y = interpolateFloat f a.y b.y
    }


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat f a b =
    (b - a) * f + a


manhattanDistance : Point -> Point -> Float
manhattanDistance a b =
    abs (a.x - b.x) + abs (a.y - b.y)
