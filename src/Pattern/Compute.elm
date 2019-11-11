module Pattern.Compute exposing
    ( Objects, compute
    , point, axis, circle, curve, detail
    )

{-|

@docs Objects, compute
@docs point, axis, circle, curve, detail

-}

import Pattern exposing (A, ComputeHelp, Pattern)
import StateResult exposing (StateResult, andThen, embed, get, map, map2, map3, map4, ok, traverse)
import Ui.Pattern


type alias Result coordinates a =
    StateResult (Pattern coordinates) ComputeHelp a


type alias Objects coordinates =
    { points : List (Ui.Pattern.Point coordinates)
    , axes : List (Ui.Pattern.Axis coordinates)
    , circles : List (Ui.Pattern.Circle coordinates)
    , details : List (Ui.Pattern.Detail coordinates)
    }


compute : Result coordinates (Objects coordinates)
compute =
    let
        computeHelp pattern =
            map4 Objects
                (traverse point (Pattern.points pattern))
                (traverse axis (Pattern.axes pattern))
                (traverse circle (Pattern.circles pattern))
                (traverse detail (Pattern.details pattern))
    in
    get
        |> andThen computeHelp



---- POINT


point : A Pattern.Point -> Result coordinates (Ui.Pattern.Point coordinates)
point aPoint =
    if Pattern.inlined aPoint then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Point Nothing)
                            (Pattern.point2d aPoint)

                    Just info ->
                        map2 (Ui.Pattern.Point << Just)
                            (pointInfo info)
                            (Pattern.point2d aPoint)
        in
        embed (Pattern.pointInfo aPoint)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Point Nothing)
            (Pattern.point2d aPoint)


pointInfo : Pattern.PointInfo -> Result coordinates (Ui.Pattern.PointInfo coordinates)
pointInfo info =
    case info of
        Pattern.Origin stuff ->
            ok Ui.Pattern.Origin

        Pattern.FromOnePoint stuff ->
            let
                toPointInfo basePoint =
                    Ui.Pattern.FromOnePoint
                        { basePoint = basePoint
                        , label = stuff.distance
                        }
            in
            map toPointInfo
                (point stuff.basePoint)

        Pattern.BetweenRatio stuff ->
            let
                toPointInfo basePointA basePointB =
                    Ui.Pattern.BetweenTwoPoints
                        { basePointA = basePointA
                        , basePointB = basePointB
                        , label = stuff.ratio
                        }
            in
            map2 toPointInfo
                (point stuff.basePointA)
                (point stuff.basePointB)

        Pattern.BetweenLength stuff ->
            let
                toPointInfo basePointA basePointB =
                    Ui.Pattern.BetweenTwoPoints
                        { basePointA = basePointA
                        , basePointB = basePointB
                        , label = stuff.distance
                        }
            in
            map2 toPointInfo
                (point stuff.basePointA)
                (point stuff.basePointB)

        Pattern.Intersection stuff ->
            Debug.todo "implement"

        Pattern.TransformedPoint stuff ->
            Debug.todo "implement"



---- AXIS


axis : A Pattern.Axis -> Result coordinates (Ui.Pattern.Axis coordinates)
axis aAxis =
    if Pattern.inlined aAxis then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Axis Nothing)
                            (Pattern.axis2d aAxis)

                    Just info ->
                        map2 (Ui.Pattern.Axis << Just)
                            (axisInfo info)
                            (Pattern.axis2d aAxis)
        in
        embed (Pattern.axisInfo aAxis)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Axis Nothing)
            (Pattern.axis2d aAxis)


axisInfo : Pattern.AxisInfo -> Result coordinates (Ui.Pattern.AxisInfo coordinates)
axisInfo info =
    case info of
        Pattern.ThroughOnePoint stuff ->
            let
                toAxisInfo point_ =
                    Ui.Pattern.ThroughOnePoint
                        { point = point_ }
            in
            map toAxisInfo
                (point stuff.point)

        Pattern.ThroughTwoPoints stuff ->
            let
                toAxisInfo pointA pointB =
                    Ui.Pattern.ThroughTwoPoints
                        { pointA = pointA
                        , pointB = pointB
                        }
            in
            map2 toAxisInfo
                (point stuff.pointA)
                (point stuff.pointB)

        Pattern.TransformedAxis stuff ->
            Debug.todo "implement"



---- CIRCLE


circle : A Pattern.Circle -> Result coordinates (Ui.Pattern.Circle coordinates)
circle aCircle =
    if Pattern.inlined aCircle then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Circle Nothing)
                            (Pattern.circle2d aCircle)

                    Just info ->
                        map2 (Ui.Pattern.Circle << Just)
                            (circleInfo info)
                            (Pattern.circle2d aCircle)
        in
        embed (Pattern.circleInfo aCircle)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Circle Nothing)
            (Pattern.circle2d aCircle)


circleInfo : Pattern.CircleInfo -> Result coordinates (Ui.Pattern.CircleInfo coordinates)
circleInfo info =
    case info of
        Pattern.WithRadius stuff ->
            let
                toCircle centerPoint =
                    Ui.Pattern.WithRadius
                        { centerPoint = centerPoint
                        , label = stuff.radius
                        }
            in
            map toCircle
                (point stuff.centerPoint)

        Pattern.ThroughThreePoints stuff ->
            let
                toCircle pointA pointB pointC =
                    Ui.Pattern.ThroughThreePoints
                        { pointA = pointA
                        , pointB = pointB
                        , pointC = pointC
                        }
            in
            map3 toCircle
                (point stuff.pointA)
                (point stuff.pointB)
                (point stuff.pointC)

        Pattern.TransformedCircle stuff ->
            Debug.todo "implement"



---- CURVE


curve : A Pattern.Curve -> Result coordinates (Ui.Pattern.Curve coordinates)
curve aCurve =
    if Pattern.inlined aCurve then
        let
            computeInfo maybeInfo =
                case maybeInfo of
                    Nothing ->
                        map (Ui.Pattern.Curve Nothing)
                            (Pattern.curve2d aCurve)

                    Just info ->
                        map2 (Ui.Pattern.Curve << Just)
                            (curveInfo info)
                            (Pattern.curve2d aCurve)
        in
        embed (Pattern.curveInfo aCurve)
            |> andThen computeInfo

    else
        map (Ui.Pattern.Curve Nothing)
            (Pattern.curve2d aCurve)


curveInfo : Pattern.CurveInfo -> Result coordinates (Ui.Pattern.CurveInfo coordinates)
curveInfo info =
    case info of
        Pattern.Straight stuff ->
            let
                toCurveInfo startPoint endPoint =
                    Ui.Pattern.LineSegment
                        { startPoint = startPoint
                        , endPoint = endPoint
                        }
            in
            map2 toCurveInfo
                (point stuff.startPoint)
                (point stuff.endPoint)

        Pattern.Quadratic stuff ->
            let
                toCurveInfo firstControlPoint secondControlPoint thirdControlPoint =
                    Ui.Pattern.QuadraticSpline
                        { firstControlPoint = firstControlPoint
                        , secondControlPoint = secondControlPoint
                        , thirdControlPoint = thirdControlPoint
                        }
            in
            map3 toCurveInfo
                (point stuff.startPoint)
                (point stuff.controlPoint)
                (point stuff.endPoint)

        Pattern.Cubic stuff ->
            let
                toCurveInfo firstControlPoint secondControlPoint thirdControlPoint fourthControlPoint =
                    Ui.Pattern.CubicSpline
                        { firstControlPoint = firstControlPoint
                        , secondControlPoint = secondControlPoint
                        , thirdControlPoint = thirdControlPoint
                        , fourthControlPoint = fourthControlPoint
                        }
            in
            map4 toCurveInfo
                (point stuff.startPoint)
                (point stuff.startControlPoint)
                (point stuff.endControlPoint)
                (point stuff.endPoint)

        Pattern.TransformedCurve stuff ->
            Debug.todo "implement"



---- DETAIL


detail : A Pattern.Detail -> Result coordinates (Ui.Pattern.Detail coordinates)
detail aDetail =
    if Pattern.inlined aDetail then
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
