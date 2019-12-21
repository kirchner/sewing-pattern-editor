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
import Pattern exposing (A, Axis, Circle, Curve, Detail, Object(..), Pattern, Point)
import Pattern.Compute as Pattern
import Point2d exposing (Point2d)
import Quantity
import State
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy as Svg
import Ui.Atom.Object exposing (Resolution)
import Ui.Theme.Color
import Vector2d



---- MODEL


{-| -}
type alias State =
    { hoveredObject : Maybe Object
    , focusedObject : Maybe Object
    , selectedObject : Maybe Object
    }


{-| -}
init : State
init =
    { hoveredObject = Nothing
    , focusedObject = Nothing
    , selectedObject = Nothing
    }



---- VIEW


{-| -}
type alias Config =
    { id : String
    }


{-| -}
type alias Viewport coordinates =
    { width : Float
    , height : Float
    , resolution : Resolution
    , center : Point2d Meters coordinates
    }


{-| -}
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
                [ Svg.translateBy
                    (Vector2d.from (Point2d.at viewport.resolution viewport.center) Point2d.origin)
                    (Svg.lazy3 draw pattern viewport.resolution state)
                ]
        )


draw : Pattern coordinates -> Resolution -> State -> Svg Msg
draw pattern resolution { hoveredObject, focusedObject, selectedObject } =
    case State.finalValue pattern Pattern.compute of
        Err _ ->
            Svg.text ""

        Ok objects ->
            let
                objectLayers drawObject toObject objectFocused objectHovered objectSelected =
                    List.foldl
                        (\( aObject, object ) layers ->
                            let
                                { inactive, active, outline, events } =
                                    drawObject
                                        { onHover = HoveredObject (toObject aObject)
                                        , onLeave = LeftObject (toObject aObject)
                                        , onSelect = ClickedObject (toObject aObject)
                                        }
                                        (Maybe.withDefault "<no-name>" (Pattern.name aObject))
                                        object
                                        resolution
                                        (objectFocused aObject)
                                        (objectHovered aObject)
                                        (objectSelected aObject)
                            in
                            { inactiveList = inactive :: layers.inactiveList
                            , activeList = active :: layers.activeList
                            , outlineList = outline :: layers.outlineList
                            , eventsList = events :: layers.eventsList
                            }
                        )
                        { inactiveList = []
                        , activeList = []
                        , outlineList = []
                        , eventsList = []
                        }

                pointFocused aPoint =
                    case focusedObject of
                        Just (Point otherAPoint) ->
                            otherAPoint == aPoint

                        _ ->
                            False

                pointHovered aPoint =
                    case hoveredObject of
                        Just (Point otherAPoint) ->
                            otherAPoint == aPoint

                        _ ->
                            False

                pointSelected aPoint =
                    case selectedObject of
                        Just (Point otherAPoint) ->
                            otherAPoint == aPoint

                        _ ->
                            False

                pointLayers =
                    objectLayers Ui.Atom.Object.drawPoint Point pointFocused pointHovered pointSelected objects.points

                axisFocused aAxis =
                    case focusedObject of
                        Just (Axis otherAAxis) ->
                            otherAAxis == aAxis

                        _ ->
                            False

                axisHovered aAxis =
                    case hoveredObject of
                        Just (Axis otherAAxis) ->
                            otherAAxis == aAxis

                        _ ->
                            False

                axisSelected aAxis =
                    case selectedObject of
                        Just (Axis otherAAxis) ->
                            otherAAxis == aAxis

                        _ ->
                            False

                axisLayers =
                    objectLayers Ui.Atom.Object.drawAxis Axis axisFocused axisHovered axisSelected objects.axes

                circleFocused aCircle =
                    case focusedObject of
                        Just (Circle otherACircle) ->
                            otherACircle == aCircle

                        _ ->
                            False

                circleHovered aCircle =
                    case hoveredObject of
                        Just (Circle otherACircle) ->
                            otherACircle == aCircle

                        _ ->
                            False

                circleSelected aCircle =
                    case selectedObject of
                        Just (Circle otherACircle) ->
                            otherACircle == aCircle

                        _ ->
                            False

                circleLayers =
                    objectLayers Ui.Atom.Object.drawCircle Circle circleFocused circleHovered circleSelected objects.circles

                curveFocused aCurve =
                    case focusedObject of
                        Just (Curve otherACurve) ->
                            otherACurve == aCurve

                        _ ->
                            False

                curveHovered aCurve =
                    case hoveredObject of
                        Just (Curve otherACurve) ->
                            otherACurve == aCurve

                        _ ->
                            False

                curveSelected aCurve =
                    case selectedObject of
                        Just (Curve otherACurve) ->
                            otherACurve == aCurve

                        _ ->
                            False

                curveLayers =
                    objectLayers Ui.Atom.Object.drawCurve Curve curveFocused curveHovered curveSelected objects.curves

                detailFocused aDetail =
                    case focusedObject of
                        Just (Detail otherADetail) ->
                            otherADetail == aDetail

                        _ ->
                            False

                detailHovered aDetail =
                    case hoveredObject of
                        Just (Detail otherADetail) ->
                            otherADetail == aDetail

                        _ ->
                            False

                detailSelected aDetail =
                    case selectedObject of
                        Just (Detail otherADetail) ->
                            otherADetail == aDetail

                        _ ->
                            False

                detailLayers =
                    objectLayers Ui.Atom.Object.drawDetail Detail detailFocused detailHovered detailSelected objects.details
            in
            Svg.g [] <|
                List.concat
                    [ -- INACTIVE
                      detailLayers.inactiveList
                    , curveLayers.inactiveList
                    , circleLayers.inactiveList
                    , axisLayers.inactiveList
                    , pointLayers.inactiveList

                    -- ACTIVE
                    , detailLayers.activeList
                    , curveLayers.activeList
                    , circleLayers.activeList
                    , axisLayers.activeList
                    , pointLayers.activeList

                    -- OUTLINE
                    , detailLayers.outlineList
                    , curveLayers.outlineList
                    , circleLayers.outlineList
                    , axisLayers.outlineList
                    , pointLayers.outlineList

                    -- EVENTS
                    , List.reverse detailLayers.eventsList
                    , List.reverse curveLayers.eventsList
                    , List.reverse circleLayers.eventsList
                    , List.reverse axisLayers.eventsList
                    , List.reverse pointLayers.eventsList
                    ]



----UPDATE


{-| -}
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


{-| -}
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
