module Pattern
    exposing
        ( Geometry
        , Pattern
        , Problems
        , circles
        , empty
        , geometry
        , getCircle
        , getLine
        , getPoint
        , insertCircle
        , insertLine
        , insertPoint
        , lines
        , points
        , removeCircle
        , removeLine
        , removePoint
        , replaceCircle
        , replaceLine
        , replacePoint
        , variables
        )

{-|

@docs Pattern

@docs empty


# Read

@docs points, circles, lines, variables

@docs getPoint, getCircle, getLine

@docs geometry, Geometry, Problems


# Modify

@docs insertVariable, removeVariable, renameVariable

@docs insertPoint, replacePoint, removePoint

@docs insertCircle, replaceCircle, removeCircle

@docs insertLine, replaceLine, removeLine

-}

import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Elements exposing (Circle, Line, Point)
import Expr exposing (DoesNotCompute, Expr)
import Point2d exposing (Point2d)
import Those exposing (That, Those)


type Pattern
    = Pattern
        { points : Those Point
        , circles : Those Circle
        , lines : Those Line
        , variables : Dict String Expr
        }


empty : Pattern
empty =
    Pattern
        { points = Those.empty
        , circles = Those.empty
        , lines = Those.empty
        , variables = Dict.empty
        }


type alias Geometry =
    { points : List ( That Point, Maybe String, Point2d )
    , circles : List ( That Circle, Maybe String, Circle2d )
    , lines : List ( That Line, Maybe String, Axis2d )
    }


type alias Problems =
    { doNotCompute : List ( String, DoesNotCompute )
    , missingPoints : List (That Point)
    , missingCircles : List (That Circle)
    , missingLines : List (That Line)
    }


geometry : Pattern -> ( Geometry, Problems )
geometry =
    Debug.todo ""



---- VARIABLES


variables : Pattern -> List ( String, Expr, Float )
variables =
    Debug.todo ""


insertVariable : String -> Expr -> Pattern -> Pattern
insertVariable =
    Debug.todo ""


removeVariable : String -> Pattern -> Pattern
removeVariable =
    Debug.todo ""


renameVariable : String -> String -> Pattern -> Pattern
renameVariable =
    Debug.todo ""



---- POINTS


points : Pattern -> List ( That Point, Maybe String )
points =
    Debug.todo ""


getPoint : Pattern -> That Point -> Maybe Point
getPoint =
    Debug.todo ""


insertPoint : Point -> Pattern -> Pattern
insertPoint =
    Debug.todo ""


replacePoint : That Point -> Point -> Pattern -> Pattern
replacePoint =
    Debug.todo ""


removePoint : That Point -> Pattern -> Pattern
removePoint =
    Debug.todo ""



---- CIRCLES


circles : Pattern -> List ( That Circle, Maybe String )
circles =
    Debug.todo ""


getCircle : Pattern -> That Circle -> Maybe Circle
getCircle =
    Debug.todo ""


insertCircle : Circle -> Pattern -> Pattern
insertCircle =
    Debug.todo ""


replaceCircle : That Circle -> Circle -> Pattern -> Pattern
replaceCircle =
    Debug.todo ""


removeCircle : That Circle -> Pattern -> Pattern
removeCircle =
    Debug.todo ""



---- LINES


lines : Pattern -> List ( That Line, Maybe String )
lines =
    Debug.todo ""


getLine : Pattern -> That Line -> Maybe Line
getLine =
    Debug.todo ""


insertLine : Line -> Pattern -> Pattern
insertLine =
    Debug.todo ""


replaceLine : That Line -> Line -> Pattern -> Pattern
replaceLine =
    Debug.todo ""


removeLine : That Line -> Pattern -> Pattern
removeLine =
    Debug.todo ""
