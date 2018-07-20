module Elements
    exposing
        ( Circle(..)
        , Line(..)
        , Point(..)
        )

import Measurements exposing (Angle, Length, Ratio)
import Those exposing (That)


type Point
    = AngleDistance (That Point) Angle Length
    | DistanceDistance (That Point) Length Length
    | Between (That Point) (That Point) Ratio
    | Intersection Intersection


type Intersection
    = FirstCircleCircle (That Circle) (That Circle)
    | SecondCircleCircle (That Circle) (That Circle)
    | LineLine (That Line) (That Line)
    | FirstCircleLine (That Circle) (That Line)
    | SecondCircleLine (That Circle) (That Line)


type Line
    = ThroughOnePoint (That Point) Angle
    | ThroughTwoPoints (That Point) (That Point)


type Circle
    = CenteredAt (That Point) Length
