module Ui.Molecule.Pattern exposing
    ( State, init
    , Config, Viewport, view, viewStatic
    , Msg, update
    )

{-|

@docs State, init
@docs Config, Viewport, view, viewStatic
@docs Msg, update

-}

import Angle exposing (Angle)
import Direction2d
import Element exposing (Element)
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
import Vector2d



---- MODEL


{-| -}
type alias State =
    { hoveredObject : Maybe Object
    , focusedObject : Maybe Object
    , selectedObjects : List Object
    }


{-| -}
init : State
init =
    { hoveredObject = Nothing
    , focusedObject = Nothing
    , selectedObjects = []
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
view _ viewport pattern state =
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
                , Html.Events.onClick UserPressedBackground
                ]
                [ Svg.translateBy
                    (Vector2d.from (Point2d.at viewport.resolution viewport.center) Point2d.origin)
                    (Svg.lazy3 draw pattern viewport.resolution state)
                ]
        )


{-| -}
viewStatic : Config -> Viewport coordinates -> Pattern coordinates -> State -> Element msg
viewStatic _ viewport pattern state =
    Element.el []
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
                    (Svg.lazy3 drawStatic pattern viewport.resolution state)
                ]
        )


draw : Pattern coordinates -> Resolution -> State -> Svg Msg
draw pattern resolution { hoveredObject, focusedObject, selectedObjects } =
    case State.finalValue pattern Pattern.compute of
        Err _ ->
            Svg.text ""

        Ok objects ->
            let
                objectLayers drawObject objectEvents toObject objectFocused objectHovered objectSelected =
                    List.foldl
                        (\( aObject, object ) layers ->
                            let
                                { background, selected, active } =
                                    drawObject
                                        (Maybe.withDefault "<no-name>" (Pattern.name aObject))
                                        object
                                        resolution
                                        (objectFocused aObject)
                                        (objectHovered aObject)
                                        (List.any (objectSelected aObject) selectedObjects)

                                events =
                                    objectEvents
                                        { onHover = HoveredObject (toObject aObject)
                                        , onLeave = LeftObject (toObject aObject)
                                        , onSelect = ClickedObject (toObject aObject)
                                        }
                                        object
                                        resolution
                            in
                            { backgroundList = background :: layers.backgroundList
                            , selectedList = selected :: layers.selectedList
                            , activeList = active :: layers.activeList
                            , eventsList = events :: layers.eventsList
                            }
                        )
                        { backgroundList = []
                        , selectedList = []
                        , activeList = []
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

                pointSelected aPoint selectedObject =
                    case selectedObject of
                        Point otherAPoint ->
                            otherAPoint == aPoint

                        _ ->
                            False

                pointLayers =
                    objectLayers
                        Ui.Atom.Object.drawPoint
                        Ui.Atom.Object.pointEvents
                        Point
                        pointFocused
                        pointHovered
                        pointSelected
                        objects.points

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

                axisSelected aAxis selectedObject =
                    case selectedObject of
                        Axis otherAAxis ->
                            otherAAxis == aAxis

                        _ ->
                            False

                axisLayers =
                    objectLayers
                        Ui.Atom.Object.drawAxis
                        Ui.Atom.Object.axisEvents
                        Axis
                        axisFocused
                        axisHovered
                        axisSelected
                        objects.axes

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

                circleSelected aCircle selectedObject =
                    case selectedObject of
                        Circle otherACircle ->
                            otherACircle == aCircle

                        _ ->
                            False

                circleLayers =
                    objectLayers
                        Ui.Atom.Object.drawCircle
                        Ui.Atom.Object.circleEvents
                        Circle
                        circleFocused
                        circleHovered
                        circleSelected
                        objects.circles

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

                curveSelected aCurve selectedObject =
                    case selectedObject of
                        Curve otherACurve ->
                            otherACurve == aCurve

                        _ ->
                            False

                curveLayers =
                    objectLayers
                        Ui.Atom.Object.drawCurve
                        Ui.Atom.Object.curveEvents
                        Curve
                        curveFocused
                        curveHovered
                        curveSelected
                        objects.curves

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

                detailSelected aDetail selectedObject =
                    case selectedObject of
                        Detail otherADetail ->
                            otherADetail == aDetail

                        _ ->
                            False

                detailLayers =
                    objectLayers
                        Ui.Atom.Object.drawDetail
                        Ui.Atom.Object.detailEvents
                        Detail
                        detailFocused
                        detailHovered
                        detailSelected
                        objects.details
            in
            Svg.g [] <|
                List.concat
                    [ -- background
                      detailLayers.backgroundList
                    , curveLayers.backgroundList
                    , circleLayers.backgroundList
                    , axisLayers.backgroundList
                    , pointLayers.backgroundList

                    -- selected
                    , detailLayers.selectedList
                    , curveLayers.selectedList
                    , circleLayers.selectedList
                    , axisLayers.selectedList
                    , pointLayers.selectedList

                    -- active
                    , detailLayers.activeList
                    , curveLayers.activeList
                    , circleLayers.activeList
                    , axisLayers.activeList
                    , pointLayers.activeList

                    -- EVENTS
                    , List.reverse detailLayers.eventsList
                    , List.reverse curveLayers.eventsList
                    , List.reverse circleLayers.eventsList
                    , List.reverse axisLayers.eventsList
                    , List.reverse pointLayers.eventsList
                    ]


drawStatic : Pattern coordinates -> Resolution -> State -> Svg msg
drawStatic pattern resolution { hoveredObject, focusedObject, selectedObjects } =
    case State.finalValue pattern Pattern.compute of
        Err _ ->
            Svg.text ""

        Ok objects ->
            let
                objectLayers drawObject objectFocused objectHovered objectSelected =
                    List.foldl
                        (\( aObject, object ) layers ->
                            let
                                { background, selected, active } =
                                    drawObject
                                        (Maybe.withDefault "<no-name>" (Pattern.name aObject))
                                        object
                                        resolution
                                        (objectFocused aObject)
                                        (objectHovered aObject)
                                        (List.any (objectSelected aObject) selectedObjects)
                            in
                            { backgroundList = background :: layers.backgroundList
                            , selectedList = selected :: layers.selectedList
                            , activeList = active :: layers.activeList
                            }
                        )
                        { backgroundList = []
                        , selectedList = []
                        , activeList = []
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

                pointSelected aPoint selectedObject =
                    case selectedObject of
                        Point otherAPoint ->
                            otherAPoint == aPoint

                        _ ->
                            False

                pointLayers =
                    objectLayers
                        Ui.Atom.Object.drawPoint
                        pointFocused
                        pointHovered
                        pointSelected
                        objects.points

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

                axisSelected aAxis selectedObject =
                    case selectedObject of
                        Axis otherAAxis ->
                            otherAAxis == aAxis

                        _ ->
                            False

                axisLayers =
                    objectLayers
                        Ui.Atom.Object.drawAxis
                        axisFocused
                        axisHovered
                        axisSelected
                        objects.axes

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

                circleSelected aCircle selectedObject =
                    case selectedObject of
                        Circle otherACircle ->
                            otherACircle == aCircle

                        _ ->
                            False

                circleLayers =
                    objectLayers
                        Ui.Atom.Object.drawCircle
                        circleFocused
                        circleHovered
                        circleSelected
                        objects.circles

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

                curveSelected aCurve selectedObject =
                    case selectedObject of
                        Curve otherACurve ->
                            otherACurve == aCurve

                        _ ->
                            False

                curveLayers =
                    objectLayers
                        Ui.Atom.Object.drawCurve
                        curveFocused
                        curveHovered
                        curveSelected
                        objects.curves

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

                detailSelected aDetail selectedObject =
                    case selectedObject of
                        Detail otherADetail ->
                            otherADetail == aDetail

                        _ ->
                            False

                detailLayers =
                    objectLayers
                        Ui.Atom.Object.drawDetail
                        detailFocused
                        detailHovered
                        detailSelected
                        objects.details
            in
            Svg.g [] <|
                List.concat
                    [ -- background
                      detailLayers.backgroundList
                    , curveLayers.backgroundList
                    , circleLayers.backgroundList
                    , axisLayers.backgroundList
                    , pointLayers.backgroundList

                    -- selected
                    , detailLayers.selectedList
                    , curveLayers.selectedList
                    , circleLayers.selectedList
                    , axisLayers.selectedList
                    , pointLayers.selectedList

                    -- active
                    , detailLayers.activeList
                    , curveLayers.activeList
                    , circleLayers.activeList
                    , axisLayers.activeList
                    , pointLayers.activeList
                    ]



----UPDATE


{-| -}
type Msg
    = NoOp
    | FocusedPattern
    | UserPressedBackground
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


{-| -}
update : Msg -> Pattern coordinates -> Bool -> State -> State
update msg pattern dragging state =
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

        UserPressedBackground ->
            if dragging then
                state

            else
                { state
                    | focusedObject = Nothing
                    , hoveredObject = Nothing
                    , selectedObjects = []
                }

        -- OBJECTS
        HoveredObject object ->
            { state | hoveredObject = Just object }

        LeftObject _ ->
            { state | hoveredObject = Nothing }

        FocusedObject object ->
            { state | focusedObject = Just object }

        ClickedObject object ->
            if dragging then
                state

            else
                { state
                    | focusedObject = Just object
                    , selectedObjects =
                        if List.member object state.selectedObjects then
                            List.filter (\otherObject -> otherObject /= object) state.selectedObjects

                        else
                            state.selectedObjects ++ [ object ]
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
            case state.focusedObject of
                Nothing ->
                    state

                Just object ->
                    { state
                        | selectedObjects =
                            if List.member object state.selectedObjects then
                                List.filter (\otherObject -> otherObject /= object)
                                    state.selectedObjects

                            else
                                state.selectedObjects ++ [ object ]
                    }


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
