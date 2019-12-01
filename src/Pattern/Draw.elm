module Pattern.Draw exposing
    ( Object(..)
    , Config, draw
    )

{-|

@docs Object
@docs Config, draw

-}

{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

import Pattern exposing (A, Axis, Circle, Curve, Detail, Pattern, Point)
import Pattern.Compute as Pattern
import State exposing (State)
import Svg exposing (Svg)
import Svg.Attributes
import Ui.Pattern exposing (Resolution)


type alias State =
    { hoveredObject : Maybe Object
    , focusedObject : Maybe Object
    , selectedObject : Maybe Object
    }


{-| -}
type Object
    = Point (A Point)
    | Axis (A Axis)
    | Circle (A Circle)
    | Curve (A Curve)
    | Detail (A Detail)


{-| -}
type alias Config msg =
    { onHover : Object -> msg
    , onLeave : Object -> msg
    , onSelect : Object -> msg
    }


{-| -}
draw : Config msg -> Pattern coordinates -> Resolution -> State -> Svg msg
draw cfg pattern resolution { hoveredObject, focusedObject, selectedObject } =
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
                                        { onHover = cfg.onHover (toObject aObject)
                                        , onLeave = cfg.onLeave (toObject aObject)
                                        , onSelect = cfg.onSelect (toObject aObject)
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
                    objectLayers Ui.Pattern.drawPoint Point pointFocused pointHovered pointSelected objects.points

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
                    objectLayers Ui.Pattern.drawAxis Axis axisFocused axisHovered axisSelected objects.axes

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
                    objectLayers Ui.Pattern.drawCircle Circle circleFocused circleHovered circleSelected objects.circles

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
                    objectLayers Ui.Pattern.drawCurve Curve curveFocused curveHovered curveSelected objects.curves

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
                    objectLayers Ui.Pattern.drawDetail Detail detailFocused detailHovered detailSelected objects.details
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
