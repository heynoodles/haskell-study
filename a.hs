-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
module Shapes 
( Point(..)
	, Shape(..)
	, surface
	, nudge
	, baseCircle)
where
-- 定义typeclass
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r


baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

-- Record Syntax
data Person = Person {
	firstName :: String,
	lastName :: String,
	age :: Int
} deriving (Show)


-- Type parameters, 类似java中泛型
data Maybe a = Nothing | Just a
