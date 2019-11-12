module Pattern.Compute exposing
    ( Objects, compute
    , point, axis, circle, curve, detail
    )

{-|

@docs Objects, compute
@docs point, axis, circle, curve, detail

-}

import Pattern exposing (A, Axis, Circle, ComputeHelp, Curve, Detail, Pattern, Point)
import StateResult exposing (StateResult, andThen, embed, get, map, map2, map3, map4, map5, ok, traverse)
import Ui.Pattern


type alias Result coordinates a =
    StateResult (Pattern coordinates) ComputeHelp a


type alias Objects coordinates =
    { points : List ( A Point, Ui.Pattern.Point coordinates )
    , axes : List ( A Axis, Ui.Pattern.Axis coordinates )
    , circles : List ( A Circle, Ui.Pattern.Circle coordinates )
    , curves : List ( A Curve, Ui.Pattern.Curve coordinates )
    , details : List ( A Detail, Ui.Pattern.Detail coordinates )
    }


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


point : Bool -> A Pattern.Point -> Result coordinates (Ui.Pattern.Point coordinates)
point topLevel aPoint =
    if topLevel || Pattern.inlined aPoint then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Point Nothing)
                            (Pattern.point2d aPoint)

                    Just info ->
                        map2 Ui.Pattern.Point
                            (pointInfo info)
                            (Pattern.point2d aPoint)
        in
        embed (Pattern.pointInfo aPoint)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Point Nothing)
            (Pattern.point2d aPoint)


pointInfo : Pattern.PointInfo -> Result coordinates (Maybe (Ui.Pattern.PointInfo coordinates))
pointInfo info =
    case info of
        Pattern.Origin stuff ->
            ok (Just Ui.Pattern.Origin)

        Pattern.FromOnePoint stuff ->
            let
                toPointInfo basePoint =
                    Just <|
                        Ui.Pattern.FromOnePoint
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
                        Ui.Pattern.BetweenTwoPoints
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
                        Ui.Pattern.BetweenTwoPoints
                            { basePointA = basePointA
                            , basePointB = basePointB
                            , label = stuff.distance
                            }
            in
            map2 toPointInfo
                (point False stuff.basePointA)
                (point False stuff.basePointB)

        Pattern.Intersection stuff ->
            ok Nothing

        Pattern.TransformedPoint stuff ->
            ok Nothing



---- AXIS


axis : Bool -> A Pattern.Axis -> Result coordinates (Ui.Pattern.Axis coordinates)
axis topLevel aAxis =
    if topLevel || Pattern.inlined aAxis then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Axis Nothing)
                            (Pattern.axis2d aAxis)

                    Just info ->
                        map2 Ui.Pattern.Axis
                            (axisInfo info)
                            (Pattern.axis2d aAxis)
        in
        embed (Pattern.axisInfo aAxis)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Axis Nothing)
            (Pattern.axis2d aAxis)


axisInfo : Pattern.AxisInfo -> Result coordinates (Maybe (Ui.Pattern.AxisInfo coordinates))
axisInfo info =
    case info of
        Pattern.ThroughOnePoint stuff ->
            let
                toAxisInfo point_ =
                    Just <|
                        Ui.Pattern.ThroughOnePoint
                            { point = point_ }
            in
            map toAxisInfo
                (point False stuff.point)

        Pattern.ThroughTwoPoints stuff ->
            let
                toAxisInfo pointA pointB =
                    Just <|
                        Ui.Pattern.ThroughTwoPoints
                            { pointA = pointA
                            , pointB = pointB
                            }
            in
            map2 toAxisInfo
                (point False stuff.pointA)
                (point False stuff.pointB)

        Pattern.TransformedAxis stuff ->
            ok Nothing



---- CIRCLE


circle : Bool -> A Pattern.Circle -> Result coordinates (Ui.Pattern.Circle coordinates)
circle topLevel aCircle =
    if topLevel || Pattern.inlined aCircle then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Circle Nothing)
                            (Pattern.circle2d aCircle)

                    Just info ->
                        map2 Ui.Pattern.Circle
                            (circleInfo info)
                            (Pattern.circle2d aCircle)
        in
        embed (Pattern.circleInfo aCircle)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Circle Nothing)
            (Pattern.circle2d aCircle)


circleInfo : Pattern.CircleInfo -> Result coordinates (Maybe (Ui.Pattern.CircleInfo coordinates))
circleInfo info =
    case info of
        Pattern.WithRadius stuff ->
            let
                toCircle centerPoint =
                    Just <|
                        Ui.Pattern.WithRadius
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
                        Ui.Pattern.ThroughThreePoints
                            { pointA = pointA
                            , pointB = pointB
                            , pointC = pointC
                            }
            in
            map3 toCircle
                (point False stuff.pointA)
                (point False stuff.pointB)
                (point False stuff.pointC)

        Pattern.TransformedCircle stuff ->
            ok Nothing



---- CURVE


curve : Bool -> A Pattern.Curve -> Result coordinates (Ui.Pattern.Curve coordinates)
curve topLevel aCurve =
    if topLevel || Pattern.inlined aCurve then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Curve Nothing)
                            (Pattern.curve2d aCurve)

                    Just info ->
                        map2 Ui.Pattern.Curve
                            (curveInfo info)
                            (Pattern.curve2d aCurve)
        in
        embed (Pattern.curveInfo aCurve)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Curve Nothing)
            (Pattern.curve2d aCurve)


curveInfo : Pattern.CurveInfo -> Result coordinates (Maybe (Ui.Pattern.CurveInfo coordinates))
curveInfo info =
    case info of
        Pattern.Straight stuff ->
            let
                toCurveInfo startPoint endPoint =
                    Just <|
                        Ui.Pattern.LineSegment
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
                        Ui.Pattern.QuadraticSpline
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
                        Ui.Pattern.CubicSpline
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

        Pattern.TransformedCurve stuff ->
            ok Nothing



---- DETAIL


detail : Bool -> A Pattern.Detail -> Result coordinates (Ui.Pattern.Detail coordinates)
detail topLevel aDetail =
    if topLevel || Pattern.inlined aDetail then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Detail [] [])
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
        map (Ui.Pattern.Detail [] [])
            (Pattern.detail2d aDetail)


type alias DetailInfo coordinates =
    { points : List (Ui.Pattern.Point coordinates)
    , curves : List (Ui.Pattern.Curve coordinates)
    }


detailInfo : Pattern.DetailInfo -> Result coordinates (DetailInfo coordinates)
detailInfo info =
    -- TODO
    ok
        { points = []
        , curves = []
        }
