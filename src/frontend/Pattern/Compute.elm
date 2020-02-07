module Pattern.Compute exposing
    ( Objects, Result, compute
    , point, axis, circle, curve, detail
    )

{-|

@docs Objects, Result, compute
@docs point, axis, circle, curve, detail

-}

import Pattern exposing (A, Axis, Circle, ComputeHelp, Curve, Detail, Intersectable(..), Pattern, Point)
import StateResult exposing (StateResult, andThen, embed, get, map, map2, map3, map4, map5, ok, traverse)
import Ui.Atom.Object


{-| -}
type alias Result coordinates a =
    StateResult (Pattern coordinates) ComputeHelp a


{-| -}
type alias Objects coordinates =
    { points : List ( A Point, Ui.Atom.Object.Point coordinates )
    , axes : List ( A Axis, Ui.Atom.Object.Axis coordinates )
    , circles : List ( A Circle, Ui.Atom.Object.Circle coordinates )
    , curves : List ( A Curve, Ui.Atom.Object.Curve coordinates )
    , details : List ( A Detail, Ui.Atom.Object.Detail coordinates )
    }


{-| -}
compute : Result coordinates (Objects coordinates)
compute =
    let
        computeHelp pattern =
            map5 Objects
                (traverse (pairWithA point) (Pattern.points pattern))
                (traverse (pairWithA axis) (Pattern.axes pattern))
                (traverse (pairWithA circle) (Pattern.circles pattern))
                (traverse (pairWithA curve) (Pattern.curves pattern))
                (traverse (pairWithA detail) (Pattern.details pattern))

        pairWithA f aObject =
            map (Tuple.pair aObject) (f True aObject)
    in
    get
        |> andThen computeHelp



---- POINT


{-| -}
point : Bool -> A Pattern.Point -> Result coordinates (Ui.Atom.Object.Point coordinates)
point topLevel aPoint =
    if topLevel || Pattern.inlined aPoint then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Atom.Object.Point Nothing)
                            (Pattern.point2d aPoint)

                    Just info ->
                        map2 Ui.Atom.Object.Point
                            (pointInfo info)
                            (Pattern.point2d aPoint)
        in
        embed (Pattern.pointInfo aPoint)
            |> andThen computeInfo

    else
        map (Ui.Atom.Object.Point Nothing)
            (Pattern.point2d aPoint)


pointInfo : Pattern.PointInfo -> Result coordinates (Maybe (Ui.Atom.Object.PointInfo coordinates))
pointInfo info =
    case info of
        Pattern.Origin _ ->
            ok (Just Ui.Atom.Object.Origin)

        Pattern.FromOnePoint stuff ->
            let
                toPointInfo basePoint =
                    Just <|
                        Ui.Atom.Object.FromOnePoint
                            { basePoint = basePoint
                            , label = stuff.distance
                            }
            in
            map toPointInfo
                (point False stuff.basePoint)

        Pattern.BetweenRatio stuff ->
            let
                toPointInfo basePointA basePointB =
                    Just <|
                        Ui.Atom.Object.BetweenTwoPoints
                            { basePointA = basePointA
                            , basePointB = basePointB
                            , label = stuff.ratio
                            }
            in
            map2 toPointInfo
                (point False stuff.basePointA)
                (point False stuff.basePointB)

        Pattern.BetweenLength stuff ->
            let
                toPointInfo basePointA basePointB =
                    Just <|
                        Ui.Atom.Object.BetweenTwoPoints
                            { basePointA = basePointA
                            , basePointB = basePointB
                            , label = stuff.distance
                            }
            in
            map2 toPointInfo
                (point False stuff.basePointA)
                (point False stuff.basePointB)

        Pattern.Intersection stuff ->
            let
                toPointInfo intersectableA intersectableB =
                    Just <|
                        Ui.Atom.Object.Intersection
                            { intersectableA = intersectableA
                            , intersectableB = intersectableB
                            }
            in
            get
                |> andThen
                    (\_ ->
                        case ( stuff.intersectableA, stuff.intersectableB ) of
                            ( IntersectableAxis aAxisA, IntersectableAxis aAxisB ) ->
                                map2 toPointInfo
                                    (map Ui.Atom.Object.IntersectableAxis (axis False aAxisA))
                                    (map Ui.Atom.Object.IntersectableAxis (axis False aAxisB))

                            _ ->
                                ok Nothing
                    )

        Pattern.TransformedPoint _ ->
            ok Nothing



---- AXIS


{-| -}
axis : Bool -> A Pattern.Axis -> Result coordinates (Ui.Atom.Object.Axis coordinates)
axis topLevel aAxis =
    if topLevel || Pattern.inlined aAxis then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Atom.Object.Axis Nothing)
                            (Pattern.axis2d aAxis)

                    Just info ->
                        map2 Ui.Atom.Object.Axis
                            (axisInfo info)
                            (Pattern.axis2d aAxis)
        in
        embed (Pattern.axisInfo aAxis)
            |> andThen computeInfo

    else
        map (Ui.Atom.Object.Axis Nothing)
            (Pattern.axis2d aAxis)


axisInfo : Pattern.AxisInfo -> Result coordinates (Maybe (Ui.Atom.Object.AxisInfo coordinates))
axisInfo info =
    case info of
        Pattern.ThroughOnePoint stuff ->
            let
                toAxisInfo point_ =
                    Just <|
                        Ui.Atom.Object.ThroughOnePoint
                            { point = point_ }
            in
            map toAxisInfo
                (point False stuff.point)

        Pattern.ThroughTwoPoints stuff ->
            let
                toAxisInfo pointA pointB =
                    Just <|
                        Ui.Atom.Object.ThroughTwoPoints
                            { pointA = pointA
                            , pointB = pointB
                            }
            in
            map2 toAxisInfo
                (point False stuff.pointA)
                (point False stuff.pointB)

        Pattern.TransformedAxis _ ->
            ok Nothing



---- CIRCLE


{-| -}
circle : Bool -> A Pattern.Circle -> Result coordinates (Ui.Atom.Object.Circle coordinates)
circle topLevel aCircle =
    if topLevel || Pattern.inlined aCircle then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Atom.Object.Circle Nothing)
                            (Pattern.circle2d aCircle)

                    Just info ->
                        map2 Ui.Atom.Object.Circle
                            (circleInfo info)
                            (Pattern.circle2d aCircle)
        in
        embed (Pattern.circleInfo aCircle)
            |> andThen computeInfo

    else
        map (Ui.Atom.Object.Circle Nothing)
            (Pattern.circle2d aCircle)


circleInfo : Pattern.CircleInfo -> Result coordinates (Maybe (Ui.Atom.Object.CircleInfo coordinates))
circleInfo info =
    case info of
        Pattern.WithRadius stuff ->
            let
                toCircle centerPoint =
                    Just <|
                        Ui.Atom.Object.WithRadius
                            { centerPoint = centerPoint
                            , label = stuff.radius
                            }
            in
            map toCircle
                (point False stuff.centerPoint)

        Pattern.ThroughThreePoints stuff ->
            let
                toCircle pointA pointB pointC =
                    Just <|
                        Ui.Atom.Object.ThroughThreePoints
                            { pointA = pointA
                            , pointB = pointB
                            , pointC = pointC
                            }
            in
            map3 toCircle
                (point False stuff.pointA)
                (point False stuff.pointB)
                (point False stuff.pointC)

        Pattern.TransformedCircle _ ->
            ok Nothing



---- CURVE


{-| -}
curve : Bool -> A Pattern.Curve -> Result coordinates (Ui.Atom.Object.Curve coordinates)
curve topLevel aCurve =
    if topLevel || Pattern.inlined aCurve then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Atom.Object.Curve Nothing)
                            (Pattern.curve2d aCurve)

                    Just info ->
                        map2 Ui.Atom.Object.Curve
                            (curveInfo info)
                            (Pattern.curve2d aCurve)
        in
        embed (Pattern.curveInfo aCurve)
            |> andThen computeInfo

    else
        map (Ui.Atom.Object.Curve Nothing)
            (Pattern.curve2d aCurve)


curveInfo : Pattern.CurveInfo -> Result coordinates (Maybe (Ui.Atom.Object.CurveInfo coordinates))
curveInfo info =
    case info of
        Pattern.Straight stuff ->
            let
                toCurveInfo startPoint endPoint =
                    Just <|
                        Ui.Atom.Object.LineSegment
                            { startPoint = startPoint
                            , endPoint = endPoint
                            }
            in
            map2 toCurveInfo
                (point False stuff.startPoint)
                (point False stuff.endPoint)

        Pattern.Quadratic stuff ->
            let
                toCurveInfo firstControlPoint secondControlPoint thirdControlPoint =
                    Just <|
                        Ui.Atom.Object.QuadraticSpline
                            { firstControlPoint = firstControlPoint
                            , secondControlPoint = secondControlPoint
                            , thirdControlPoint = thirdControlPoint
                            }
            in
            map3 toCurveInfo
                (point False stuff.startPoint)
                (point False stuff.controlPoint)
                (point False stuff.endPoint)

        Pattern.Cubic stuff ->
            let
                toCurveInfo firstControlPoint secondControlPoint thirdControlPoint fourthControlPoint =
                    Just <|
                        Ui.Atom.Object.CubicSpline
                            { firstControlPoint = firstControlPoint
                            , secondControlPoint = secondControlPoint
                            , thirdControlPoint = thirdControlPoint
                            , fourthControlPoint = fourthControlPoint
                            }
            in
            map4 toCurveInfo
                (point False stuff.startPoint)
                (point False stuff.startControlPoint)
                (point False stuff.endControlPoint)
                (point False stuff.endPoint)

        Pattern.TransformedCurve _ ->
            ok Nothing



---- DETAIL


{-| -}
detail : Bool -> A Pattern.Detail -> Result coordinates (Ui.Atom.Object.Detail coordinates)
detail topLevel aDetail =
    if topLevel || Pattern.inlined aDetail then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Atom.Object.Detail [] [])
                            (Pattern.detail2d aDetail)

                    Just info ->
                        let
                            toDetail { points, curves } detail2d =
                                { points = points
                                , curves = curves
                                , detail2d = detail2d
                                }
                        in
                        map2 toDetail
                            (detailInfo info)
                            (Pattern.detail2d aDetail)
        in
        embed (Pattern.detailInfo aDetail)
            |> andThen computeInfo

    else
        map (Ui.Atom.Object.Detail [] [])
            (Pattern.detail2d aDetail)


type alias DetailInfo coordinates =
    { points : List (Ui.Atom.Object.Point coordinates)
    , curves : List (Ui.Atom.Object.Curve coordinates)
    }


detailInfo : Pattern.DetailInfo -> Result coordinates (DetailInfo coordinates)
detailInfo info =
    let
        toDetailInfo first nexts last =
            List.foldl
                (\next collected ->
                    { points = next.points ++ collected.points
                    , curves = next.curves ++ collected.curves
                    }
                )
                first
                (last :: nexts)
    in
    map3 toDetailInfo
        (firstCurveInfo info.firstCurve)
        (traverse nextCurveInfo info.nextCurves)
        (lastCurveInfo info.lastCurve)


firstCurveInfo : Pattern.FirstCurve -> Result coordinates (DetailInfo coordinates)
firstCurveInfo firstCurve =
    let
        fromPoints points =
            { points = points
            , curves = []
            }

        fromCurve curve_ =
            { points = []
            , curves = [ curve_ ]
            }
    in
    case firstCurve of
        Pattern.FirstStraight stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.startPoint
                    , point False stuff.endPoint
                    ]

        Pattern.FirstQuadratic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.startPoint
                    , point False stuff.controlPoint
                    , point False stuff.endPoint
                    ]

        Pattern.FirstCubic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.startPoint
                    , point False stuff.startControlPoint
                    , point False stuff.endControlPoint
                    , point False stuff.endPoint
                    ]

        Pattern.FirstReferencedCurve stuff ->
            map fromCurve
                (curve False stuff.curve)


nextCurveInfo : Pattern.NextCurve -> Result coordinates (DetailInfo coordinates)
nextCurveInfo nextCurve =
    let
        fromPoints points =
            { points = points
            , curves = []
            }

        fromCurve curve_ =
            { points = []
            , curves = [ curve_ ]
            }
    in
    case nextCurve of
        Pattern.NextStraight stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.endPoint ]

        Pattern.NextQuadratic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.controlPoint
                    , point False stuff.endPoint
                    ]

        Pattern.NextCubic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.startControlPoint
                    , point False stuff.endControlPoint
                    , point False stuff.endPoint
                    ]

        Pattern.NextReferencedCurve stuff ->
            map fromCurve
                (curve False stuff.curve)


lastCurveInfo : Pattern.LastCurve -> Result coordinates (DetailInfo coordinates)
lastCurveInfo lastCurve =
    let
        fromPoints points =
            { points = points
            , curves = []
            }

        fromCurve curve_ =
            { points = []
            , curves = [ curve_ ]
            }
    in
    case lastCurve of
        Pattern.LastStraight ->
            ok { points = [], curves = [] }

        Pattern.LastQuadratic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.controlPoint ]

        Pattern.LastCubic stuff ->
            map fromPoints <|
                traverse identity
                    [ point False stuff.startControlPoint
                    , point False stuff.endControlPoint
                    ]

        Pattern.LastReferencedCurve stuff ->
            map fromCurve
                (curve False stuff.curve)
