> {-# LANGUAGE UnicodeSyntax #-}
> module Shapes
> where
> import Unicode

> data Shape
>   =  Circle Double
>   |  Square Double
>   |  Rectangle Double Double
>   deriving (Show)

> showShape ∷ Shape → String
> showShape (Circle r)      = "circle of radius " ++ show r
> showShape (Square l)      = "square of length " ++ show l
> showShape (Rectangle l w) = "rectangle of length " ++ show l
>                                   ++ " and width " ++ show w

> area ∷ Shape → Double
> area (Circle r)      = r^2 * pi
> area (Square l)      = l^2
> area (Rectangle l w) = l * w

> perimeter ∷ Shape → Double
> perimeter (Circle r)      = 2 * pi * r
> perimeter (Square l)      = 4 * l
> perimeter (Rectangle l w) = 2 * l + 2 * w

> center ∷ Shape → (Double, Double)
> center (Circle r)      = (r, r)
> center (Square l)      = (l / 2, l / 2)
> center (Rectangle l w) = (l / 2, w / 2)

> boundingBox ∷ Shape → (Double, Double)
> boundingBox (Circle r)      = (2 * r, 2 * r)
> boundingBox (Square l)      = (l, l)
> boundingBox (Rectangle l w) = (l, w)

