module Ui.Molecule.Pattern exposing
    ( State, init
    , Config, Viewport, view
    , Msg, update
    )

{-|

@docs State, init
@docs Config, Viewport, view
@docs Msg, update

-}

import Angle exposing (Angle)
import Direction2d
import Element exposing (Element)
import Element.Border as Border
import Geometry.Svg as Svg
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Length exposing (Meters)
import List.Extra as List
import Pattern exposing (A, Axis, Circle, Curve, Detail, Pattern, Point)
import Pattern.Draw exposing (Object(..))
import Point2d exposing (Point2d)
import Quantity
import State
import Svg
import Svg.Attributes
import Ui.Color
import Ui.Pattern exposing (Resolution)
import Vector2d



---- MODEL


type alias State =
    { hoveredObject : Maybe Object
    , focusedObject : Maybe Object
    , selectedObject : Maybe Object
    }


init : State
init =
    { hoveredObject = Nothing
    , focusedObject = Nothing
    , selectedObject = Nothing
    }



---- VIEW


type alias Config =
    { id : String
    }


type alias Viewport coordinates =
    { width : Float
    , height : Float
    , resolution : Resolution
    , center : Point2d Meters coordinates
    }


view : Config -> Viewport coordinates -> Pattern coordinates -> State -> Element Msg
view cfg viewport pattern state =
    Element.el
        [ Element.htmlAttribute (Html.Attributes.tabindex 0)
        , Element.htmlAttribute <|
            Html.Events.preventDefaultOn "keydown"
                (Decode.field "code" Decode.string
                    |> Decode.andThen
                        (\code ->
                            case code of
                                "ArrowLeft" ->
                                    Decode.succeed ( PressedArrowLeft, True )

                                "ArrowUp" ->
                                    Decode.succeed ( PressedArrowUp, True )

                                "ArrowRight" ->
                                    Decode.succeed ( PressedArrowRight, True )

                                "ArrowDown" ->
                                    Decode.succeed ( PressedArrowDown, True )

                                "Space" ->
                                    Decode.succeed ( PressedSpace, True )

                                _ ->
                                    Decode.fail "not handling that key here"
                        )
                )
        ]
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox <|
                    String.join " "
                        [ String.fromFloat (viewport.width / -2)
                        , String.fromFloat (viewport.height / -2)
                        , String.fromFloat viewport.width
                        , String.fromFloat viewport.height
                        ]
                , Html.Attributes.style "user-select" "none"
                , Html.Attributes.style "width" (String.fromFloat viewport.width ++ "px")
                , Html.Attributes.style "height" (String.fromFloat viewport.height ++ "px")
                ]
                [ Svg.translateBy (Vector2d.from (Point2d.at viewport.resolution viewport.center) Point2d.origin) <|
                    Pattern.Draw.draw
                        { onHover = HoveredObject
                        , onLeave = LeftObject
                        , onSelect = ClickedObject
                        }
                        pattern
                        viewport.resolution
                        state
                ]
        )



----UPDATE


type Msg
    = NoOp
    | FocusedPattern
      -- OBJECTS
    | HoveredObject Object
    | LeftObject Object
    | FocusedObject Object
    | ClickedObject Object
      -- KEYBOARD
    | PressedArrowDown
    | PressedArrowUp
    | PressedArrowLeft
    | PressedArrowRight
    | PressedSpace


type alias Position =
    { x : Float
    , y : Float
    }


update : Msg -> Pattern coordinates -> State -> State
update msg pattern state =
    case msg of
        NoOp ->
            state

        FocusedPattern ->
            let
                objects =
                    List.sortBy objectName <|
                        List.concat
                            [ List.map Point (Pattern.points pattern)
                            , List.map Axis (Pattern.axes pattern)
                            , List.map Circle (Pattern.circles pattern)
                            , List.map Curve (Pattern.curves pattern)
                            , List.map Detail (Pattern.details pattern)
                            ]
            in
            { state | focusedObject = List.head objects }

        -- OBJECTS
        HoveredObject object ->
            { state | hoveredObject = Just object }

        LeftObject _ ->
            { state | hoveredObject = Nothing }

        FocusedObject object ->
            { state | focusedObject = Just object }

        ClickedObject object ->
            { state
                | focusedObject = Just object
                , selectedObject = Just object
            }

        -- KEYBOARD
        PressedArrowDown ->
            let
                focusNextObject object =
                    case object of
                        Point aPoint ->
                            aPoint
                                |> nearestPointBetween pattern
                                    { lowerBound = Angle.degrees 45
                                    , upperBound = Angle.degrees 135
                                    }
                                |> Point

                        _ ->
                            object
            in
            { state | focusedObject = Maybe.map focusNextObject state.focusedObject }

        PressedArrowUp ->
            let
                focusNextObject object =
                    case object of
                        Point aPoint ->
                            aPoint
                                |> nearestPointBetween pattern
                                    { lowerBound = Angle.degrees -135
                                    , upperBound = Angle.degrees -45
                                    }
                                |> Point

                        _ ->
                            object
            in
            { state | focusedObject = Maybe.map focusNextObject state.focusedObject }

        PressedArrowLeft ->
            let
                focusNextObject object =
                    case object of
                        Point aPoint ->
                            aPoint
                                |> nearestPointOutOf pattern
                                    { lowerBound = Angle.degrees -135
                                    , upperBound = Angle.degrees 135
                                    }
                                |> Point

                        _ ->
                            object
            in
            { state | focusedObject = Maybe.map focusNextObject state.focusedObject }

        PressedArrowRight ->
            let
                focusNextObject object =
                    case object of
                        Point aPoint ->
                            aPoint
                                |> nearestPointBetween pattern
                                    { lowerBound = Angle.degrees -45
                                    , upperBound = Angle.degrees 45
                                    }
                                |> Point

                        _ ->
                            object
            in
            { state | focusedObject = Maybe.map focusNextObject state.focusedObject }

        PressedSpace ->
            { state | selectedObject = state.focusedObject }


objectName : Object -> String
objectName object =
    Maybe.withDefault "" <|
        case object of
            Point aPoint ->
                Pattern.name aPoint

            Axis aAxis ->
                Pattern.name aAxis

            Circle aCircle ->
                Pattern.name aCircle

            Curve aCurve ->
                Pattern.name aCurve

            Detail aDetail ->
                Pattern.name aDetail


nearestPointBetween :
    Pattern coordinates
    ->
        { lowerBound : Angle
        , upperBound : Angle
        }
    -> A Point
    -> A Point
nearestPointBetween pattern { lowerBound, upperBound } aPoint =
    case State.finalValue pattern (Pattern.point2d aPoint) of
        Err _ ->
            aPoint

        Ok point2d ->
            Pattern.points pattern
                |> List.filterMap
                    (\aOtherPoint ->
                        if aOtherPoint /= aPoint then
                            case State.finalValue pattern (Pattern.point2d aOtherPoint) of
                                Err _ ->
                                    Nothing

                                Ok otherPoint2d ->
                                    case
                                        Direction2d.from point2d otherPoint2d
                                            |> Maybe.map Direction2d.toAngle
                                    of
                                        Nothing ->
                                            Nothing

                                        Just angle ->
                                            if
                                                (angle |> Quantity.lessThanOrEqualTo upperBound)
                                                    && (angle |> Quantity.greaterThan lowerBound)
                                            then
                                                Just
                                                    ( Point2d.distanceFrom point2d otherPoint2d
                                                    , aOtherPoint
                                                    )

                                            else
                                                Nothing

                        else
                            Nothing
                    )
                |> List.minimumBy (Tuple.first >> Length.inMillimeters)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault aPoint


nearestPointOutOf :
    Pattern coordinates
    ->
        { lowerBound : Angle
        , upperBound : Angle
        }
    -> A Point
    -> A Point
nearestPointOutOf pattern { lowerBound, upperBound } aPoint =
    case State.finalValue pattern (Pattern.point2d aPoint) of
        Err _ ->
            aPoint

        Ok point2d ->
            Pattern.points pattern
                |> List.filterMap
                    (\aOtherPoint ->
                        if aOtherPoint /= aPoint then
                            case State.finalValue pattern (Pattern.point2d aOtherPoint) of
                                Err _ ->
                                    Nothing

                                Ok otherPoint2d ->
                                    case
                                        Direction2d.from point2d otherPoint2d
                                            |> Maybe.map Direction2d.toAngle
                                    of
                                        Nothing ->
                                            Nothing

                                        Just angle ->
                                            if
                                                (angle |> Quantity.greaterThan upperBound)
                                                    || (angle |> Quantity.lessThanOrEqualTo lowerBound)
                                            then
                                                Just
                                                    ( Point2d.distanceFrom point2d otherPoint2d
                                                    , aOtherPoint
                                                    )

                                            else
                                                Nothing

                        else
                            Nothing
                    )
                |> List.minimumBy (Tuple.first >> Length.inMillimeters)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault aPoint


nextAfter : a -> List a -> a
nextAfter a listA =
    case listA of
        [] ->
            a

        first :: rest ->
            if first == a then
                Maybe.withDefault first (List.head rest)

            else
                nextAfterHelp a first rest


nextAfterHelp : a -> a -> List a -> a
nextAfterHelp a first listA =
    case listA of
        [] ->
            first

        next :: rest ->
            if next == a then
                Maybe.withDefault first (List.head rest)

            else
                nextAfterHelp a first rest
