module Ui.Atom.Marker exposing
    ( Orientation(..)
    , draw
    )

import Angle
import Geometry.Svg as Svg
import Length exposing (Meters)
import Pattern.Viewport exposing (Resolution)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes
import Vector2d



---- DRAW


type alias Config coordinates =
    { orientation : Orientation
    , index : Int
    , center : Point2d Meters coordinates
    , resolution : Resolution
    }


type Orientation
    = Horizontal
    | Vertical


draw : Config coordinates -> Svg msg
draw config =
    let
        bigCount =
            config.index // 4

        smallCount =
            modBy 4 config.index

        totalOffset =
            totalWidth / -2

        totalWidth =
            toFloat bigCount * (bigWidth + 1) + toFloat smallCount * (smallWidth + 1) - 1

        bigOffset i =
            Vector2d.millimeters (toFloat i * (bigWidth + 1)) 0

        smallOffset i =
            Vector2d.millimeters (toFloat i * (smallWidth + 1)) 0

        rotate =
            case config.orientation of
                Vertical ->
                    identity

                Horizontal ->
                    Rectangle2d.rotateAround Point2d.origin (Angle.degrees 90)
    in
    Svg.g [] <|
        List.map
            (Svg.rectangle2d
                [ Svg.Attributes.fill "gray"
                , Svg.Attributes.strokeWidth "0"
                ]
                << Rectangle2d.at config.resolution
                << Rectangle2d.translateBy (Vector2d.from Point2d.origin config.center)
                << rotate
                << Rectangle2d.translateBy (Vector2d.millimeters totalOffset 0)
                << Rectangle2d.translateBy (Vector2d.millimeters 0 (height / -2))
            )
            (List.concat
                [ List.repeat bigCount big
                    |> List.indexedMap (Rectangle2d.translateBy << bigOffset)
                , List.repeat smallCount small
                    |> List.indexedMap (Rectangle2d.translateBy << smallOffset)
                    |> List.map
                        (Rectangle2d.translateBy
                            (Vector2d.millimeters ((bigWidth + 1) * toFloat bigCount) 0)
                        )
                ]
            )


small : Rectangle2d Meters coordinates
small =
    Rectangle2d.with
        { x1 = Length.millimeters 0
        , y1 = Length.millimeters 0
        , x2 = Length.millimeters smallWidth
        , y2 = Length.millimeters height
        }


big : Rectangle2d Meters coordinates
big =
    Rectangle2d.with
        { x1 = Length.millimeters 0
        , y1 = Length.millimeters 0
        , x2 = Length.millimeters bigWidth
        , y2 = Length.millimeters height
        }


height : Float
height =
    64


smallWidth : Float
smallWidth =
    2


bigWidth : Float
bigWidth =
    6
