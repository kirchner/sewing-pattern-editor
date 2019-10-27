module Dialog exposing
    ( Create, Edit
    , ObjectReferences, ThatObject(..), Objects
    , createSelection, createPreview, createHovered, createFocused
    , editSelection, editPreview, editHovered, editFocused
    , createPoint, createAxis, createCircle, createCurve, createDetail
    , editPoint, editAxis, editCircle, editCurve, editDetail
    , createView, editView
    , CreateMsg, CreateResult(..), createUpdate
    , EditMsg, EditResult(..), editUpdate
    , createSubscriptions, editSubscriptions
    )

{-|


# Model

@docs Create, Edit

@docs ObjectReferences, ThatObject, Objects

@docs createSelection, createPreview, createHovered, createFocused

@docs editSelection, editPreview, editHovered, editFocused


# Init

@docs createPoint, createAxis, createCircle, createCurve, createDetail

@docs editPoint, editAxis, editCircle, editCurve, editDetail


# View

@docs createView, editView


# Update

@docs CreateMsg, CreateResult, createUpdate

@docs EditMsg, EditResult, editUpdate


# Subscriptions

@docs createSubscriptions, editSubscriptions

-}

import Design
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Pattern
    exposing
        ( A
        , Axis
        , AxisInfo(..)
        , Circle
        , CircleInfo(..)
        , ComputeHelp(..)
        , Curve
        , CurveInfo(..)
        , Detail
        , Direction(..)
        , ExprHelp(..)
        , FirstCurve(..)
        , InsertHelp(..)
        , Intersectable(..)
        , IntersectableTag(..)
        , LastCurve(..)
        , NextCurve(..)
        , OneInTwo(..)
        , Orientation(..)
        , Pattern
        , Point
        , PointInfo(..)
        , ReplaceHelp(..)
        )
import Result.Extra as Result
import State exposing (State)
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)
import View.Icon
import View.Input



---- MODEL


{-| -}
type Create
    = Create
        { name : String
        , nameHelp : Maybe String
        , dialog : Dialog
        }


type Dialog
    = DialogPoint PointForm
    | DialogAxis AxisForm
    | DialogCircle CircleForm
    | DialogCurve CurveForm
    | DialogDetail DetailForm


{-| -}
type Edit
    = EditPoint
        { aPoint : A Point
        , objects : Pattern.Objects
        , form : PointForm
        }
    | EditAxis
        { aAxis : A Axis
        , objects : Pattern.Objects
        , form : AxisForm
        }
    | EditCircle
        { aCircle : A Circle
        , objects : Pattern.Objects
        , form : CircleForm
        }
    | EditCurve
        { aCurve : A Curve
        , objects : Pattern.Objects
        , form : CurveForm
        }
    | EditDetail
        { aDetail : A Detail
        , objects : Pattern.Objects
        , form : DetailForm
        }


{-| -}
type alias ObjectReferences =
    { points : List (A Pattern.Point)
    , axes : List (A Pattern.Point)
    , circles : List (A Pattern.Point)
    , curves : List (A Pattern.Point)
    , details : List (A Pattern.Point)
    }


{-| -}
type ThatObject
    = ThatPoint (A Pattern.Point)
    | ThatAxis (A Pattern.Axis)
    | ThatCircle (A Pattern.Circle)
    | ThatCurve (A Pattern.Curve)
    | ThatDetail (A Pattern.Detail)


{-| -}
type alias Objects =
    { points : List Pattern.Point
    , axes : List Pattern.Axis
    , circles : List Pattern.Circle
    , curves : List Pattern.Curve
    , details : List Pattern.Detail
    }


{-| -}
createSelection : Create -> ObjectReferences
createSelection model =
    { points = []
    , axes = []
    , circles = []
    , curves = []
    , details = []
    }


{-| -}
createPreview : Create -> Objects
createPreview model =
    { points = []
    , axes = []
    , circles = []
    , curves = []
    , details = []
    }


{-| -}
createHovered : Create -> Maybe ThatObject
createHovered model =
    Nothing


{-| -}
createFocused : Create -> Maybe ThatObject
createFocused model =
    Nothing


{-| -}
editSelection : Edit -> ObjectReferences
editSelection model =
    { points = []
    , axes = []
    , circles = []
    , curves = []
    , details = []
    }


{-| -}
editPreview : Edit -> Objects
editPreview model =
    { points = []
    , axes = []
    , circles = []
    , curves = []
    , details = []
    }


{-| -}
editHovered : Edit -> Maybe ThatObject
editHovered model =
    Nothing


{-| -}
editFocused : Edit -> Maybe ThatObject
editFocused model =
    Nothing



---- TOP LEVEL


{-| -}
type PointForm
    = FromOnePointForm
        { basePoint : OtherPointForm
        , direction : Direction
        , directionHelp : Maybe String
        , distance : String
        , distanceHelp : Maybe String
        }
    | FromTwoPointsForm
        { basePointA : OtherPointForm
        , basePointB : OtherPointForm
        , pointsHelp : Maybe String
        , twoPointsPosition : TwoPointsPosition
        }
    | IntersectionForm
        { objectA : OtherIntersectableForm
        , objectB : OtherIntersectableForm
        , objectsHelp : Maybe String
        , which : Int
        , whichHelp : Maybe String
        }


type TwoPointsPosition
    = TwoPointsPositionRatio
        { ratio : String
        , ratioHelp : Maybe String
        }
    | TwoPointsPositionFromA
        { distance : String
        , distanceHelp : Maybe String
        }
    | TwoPointsPositionFromB
        { distance : String
        , distanceHelp : Maybe String
        }


{-| -}
type AxisForm
    = ThroughOnePointForm
        { point : OtherPointForm
        , orientation : Orientation
        , orientationHelp : Maybe String
        }
    | ThroughTwoPointsForm
        { pointA : OtherPointForm
        , pointB : OtherPointForm
        , pointsHelp : Maybe String
        }


{-| -}
type CircleForm
    = WithRadiusForm
        { centerPoint : OtherPointForm
        , radius : String
        , radiusHelp : Maybe String
        }
    | ThroughThreePointsForm
        { pointA : OtherPointForm
        , pointB : OtherPointForm
        , pointC : OtherPointForm
        , pointsHelp : Maybe String
        }


{-| -}
type CurveForm
    = StraightForm
        { startPoint : OtherPointForm
        , endPoint : OtherPointForm
        , pointsHelp : Maybe String
        }
    | QuadraticForm
        { startPoint : OtherPointForm
        , controlPoint : OtherPointForm
        , endPoint : OtherPointForm
        , pointsHelp : Maybe String
        }
    | CubicForm
        { startPoint : OtherPointForm
        , startControlPoint : OtherPointForm
        , endControlPoint : OtherPointForm
        , endPoint : OtherPointForm
        , pointsHelp : Maybe String
        }


{-| -}
type alias DetailForm =
    { firstCurve : ( FirstCurveForm, ActionMenu )
    , nextCurves : List ( NextCurveForm, ActionMenu )
    , lastCurve : ( LastCurveForm, ActionMenu )
    }


type FirstCurveForm
    = FirstStraightForm
        { startPoint : OtherPointForm
        , endPoint : OtherPointForm
        }
    | FirstQuadraticForm
        { startPoint : OtherPointForm
        , controlPoint : OtherPointForm
        , endPoint : OtherPointForm
        }
    | FirstCubicForm
        { startPoint : OtherPointForm
        , startControlPoint : OtherPointForm
        , endControlPoint : OtherPointForm
        , endPoint : OtherPointForm
        }
    | FirstReferencedCurveForm
        { curve : OtherCurveForm
        , reversed : Bool
        }


type NextCurveForm
    = NextStraightForm
        { endPoint : OtherPointForm
        }
    | NextQuadraticForm
        { controlPoint : OtherPointForm
        , endPoint : OtherPointForm
        }
    | NextCubicForm
        { startControlPoint : OtherPointForm
        , endControlPoint : OtherPointForm
        , endPoint : OtherPointForm
        }
    | NextReferencedCurveForm
        { curve : OtherCurveForm
        , reversed : Bool
        }


type LastCurveForm
    = LastStraightForm
    | LastQuadraticForm
        { controlPoint : OtherPointForm
        }
    | LastCubicForm
        { startControlPoint : OtherPointForm
        , endControlPoint : OtherPointForm
        }
    | LastReferencedCurveForm
        { curve : OtherCurveForm
        , reversed : Bool
        }


type alias OtherCurveForm =
    { dropdown : Dropdown
    , maybeACurve : Maybe (A Curve)
    }


type ActionMenu
    = Closed
    | MoveUp
    | MoveDown
    | InsertCurveBefore
    | InsertCurveAfter
    | Remove



---- NESTINGS


type OtherPointForm
    = ReferencedPointForm
        { dropdown : Dropdown
        , maybeAPoint : Maybe (A Point)
        , help : Maybe String
        }
    | InlinedPointForm
        { expanded : Bool
        , point : PointForm
        }


type OtherIntersectableForm
    = ReferencedIntersectableForm
        { dropdown : Dropdown
        , maybeAIntersectable : Maybe (A Intersectable)
        , help : Maybe String
        }
    | InlinedAxisForm
        { expanded : Bool
        , axis : AxisForm
        }
    | InlinedCircleForm
        { expanded : Bool
        , circle : CircleForm
        }
    | InlinedCurveForm
        { expanded : Bool
        , curve : CurveForm
        }


otherIntersectableFormExpanded : OtherIntersectableForm -> Bool
otherIntersectableFormExpanded form =
    case form of
        ReferencedIntersectableForm _ ->
            True

        InlinedAxisForm { expanded } ->
            expanded

        InlinedCircleForm { expanded } ->
            expanded

        InlinedCurveForm { expanded } ->
            expanded



---- INIT


{-| -}
createPoint : Create
createPoint =
    Create
        { name = ""
        , nameHelp = Nothing
        , dialog = DialogPoint initFromOnePointForm
        }


{-| -}
createAxis : Create
createAxis =
    Create
        { name = ""
        , nameHelp = Nothing
        , dialog = DialogAxis initThroughOnePointForm
        }


{-| -}
createCircle : Create
createCircle =
    Create
        { name = ""
        , nameHelp = Nothing
        , dialog = DialogCircle initWithRadiusForm
        }


{-| -}
createCurve : Create
createCurve =
    Create
        { name = ""
        , nameHelp = Nothing
        , dialog = DialogCurve initStraightForm
        }


{-| -}
createDetail : Create
createDetail =
    Create
        { name = ""
        , nameHelp = Nothing
        , dialog = DialogDetail initDetailForm
        }


{-| -}
editPoint : Pattern -> A Pattern.Point -> Maybe Edit
editPoint pattern aPoint =
    let
        objects =
            Pattern.objectsNotDependingOnPoint pattern aPoint
    in
    Maybe.map
        (\form ->
            EditPoint
                { aPoint = aPoint
                , objects = objects
                , form = form
                }
        )
        (initPointFormWith pattern aPoint)


initPointFormWith : Pattern -> A Point -> Maybe PointForm
initPointFormWith pattern aPoint =
    case Pattern.pointInfo aPoint pattern of
        Nothing ->
            Nothing

        Just info ->
            case info of
                Origin stuff ->
                    Nothing

                FromOnePoint stuff ->
                    let
                        toForm basePoint =
                            FromOnePointForm
                                { basePoint = basePoint
                                , direction = stuff.direction
                                , directionHelp = Nothing
                                , distance = stuff.distance
                                , distanceHelp = Nothing
                                }
                    in
                    Maybe.map toForm (initOtherPointFormWith pattern stuff.basePoint)

                BetweenRatio stuff ->
                    let
                        toForm basePointA basePointB =
                            FromTwoPointsForm
                                { basePointA = basePointA
                                , basePointB = basePointB
                                , pointsHelp = Nothing
                                , twoPointsPosition =
                                    TwoPointsPositionRatio
                                        { ratio = stuff.ratio
                                        , ratioHelp = Nothing
                                        }
                                }
                    in
                    Maybe.map2 toForm
                        (initOtherPointFormWith pattern stuff.basePointA)
                        (initOtherPointFormWith pattern stuff.basePointB)

                BetweenLength stuff ->
                    let
                        toForm basePointA basePointB =
                            FromTwoPointsForm
                                { basePointA = basePointA
                                , basePointB = basePointB
                                , pointsHelp = Nothing
                                , twoPointsPosition =
                                    case stuff.from of
                                        FirstInTwo ->
                                            TwoPointsPositionFromA
                                                { distance = stuff.distance
                                                , distanceHelp = Nothing
                                                }

                                        SecondInTwo ->
                                            TwoPointsPositionFromA
                                                { distance = stuff.distance
                                                , distanceHelp = Nothing
                                                }
                                }
                    in
                    Maybe.map2 toForm
                        (initOtherPointFormWith pattern stuff.basePointA)
                        (initOtherPointFormWith pattern stuff.basePointB)

                Intersection stuff ->
                    let
                        toForm objectA objectB =
                            IntersectionForm
                                { objectA = objectA
                                , objectB = objectB
                                , objectsHelp = Nothing
                                , which = stuff.which
                                , whichHelp = Nothing
                                }
                    in
                    Maybe.map2 toForm
                        (initOtherIntersectableFormWith pattern stuff.objectA)
                        (initOtherIntersectableFormWith pattern stuff.objectB)

                TransformedPoint stuff ->
                    Nothing


{-| -}
editAxis : Pattern -> A Pattern.Axis -> Maybe Edit
editAxis pattern aAxis =
    let
        objects =
            Pattern.objectsNotDependingOnAxis pattern aAxis
    in
    Maybe.map
        (\form ->
            EditAxis
                { aAxis = aAxis
                , objects = objects
                , form = form
                }
        )
        (initAxisFormWith pattern aAxis)


initAxisFormWith : Pattern -> A Axis -> Maybe AxisForm
initAxisFormWith pattern aAxis =
    case Pattern.axisInfo aAxis pattern of
        Nothing ->
            Nothing

        Just (ThroughOnePoint stuff) ->
            let
                toForm point =
                    ThroughOnePointForm
                        { point = point
                        , orientation = stuff.orientation
                        , orientationHelp = Nothing
                        }
            in
            Maybe.map toForm (initOtherPointFormWith pattern stuff.point)

        Just (ThroughTwoPoints stuff) ->
            let
                toForm pointA pointB =
                    ThroughTwoPointsForm
                        { pointA = pointA
                        , pointB = pointB
                        , pointsHelp = Nothing
                        }
            in
            Maybe.map2 toForm
                (initOtherPointFormWith pattern stuff.pointA)
                (initOtherPointFormWith pattern stuff.pointB)

        Just (TransformedAxis stuff) ->
            Nothing


{-| -}
editCircle : Pattern -> A Pattern.Circle -> Maybe Edit
editCircle pattern aCircle =
    let
        objects =
            Pattern.objectsNotDependingOnCircle pattern aCircle
    in
    Maybe.map
        (\form ->
            EditCircle
                { aCircle = aCircle
                , objects = objects
                , form = form
                }
        )
        (initCircleFormWith pattern aCircle)


initCircleFormWith : Pattern -> A Circle -> Maybe CircleForm
initCircleFormWith pattern aCircle =
    case Pattern.circleInfo aCircle pattern of
        Nothing ->
            Nothing

        Just (WithRadius stuff) ->
            let
                toForm centerPoint =
                    WithRadiusForm
                        { centerPoint = centerPoint
                        , radius = stuff.radius
                        , radiusHelp = Nothing
                        }
            in
            Maybe.map toForm (initOtherPointFormWith pattern stuff.centerPoint)

        Just (ThroughThreePoints stuff) ->
            let
                toForm pointA pointB pointC =
                    ThroughThreePointsForm
                        { pointA = pointA
                        , pointB = pointB
                        , pointC = pointC
                        , pointsHelp = Nothing
                        }
            in
            Maybe.map3 toForm
                (initOtherPointFormWith pattern stuff.pointA)
                (initOtherPointFormWith pattern stuff.pointB)
                (initOtherPointFormWith pattern stuff.pointC)

        Just (TransformedCircle stuff) ->
            Nothing


{-| -}
editCurve : Pattern -> A Pattern.Curve -> Maybe Edit
editCurve pattern aCurve =
    let
        objects =
            Pattern.objectsNotDependingOnCurve pattern aCurve
    in
    Maybe.map
        (\form ->
            EditCurve
                { aCurve = aCurve
                , objects = objects
                , form = form
                }
        )
        (initCurveFormWith pattern aCurve)


initCurveFormWith : Pattern -> A Curve -> Maybe CurveForm
initCurveFormWith pattern aCurve =
    case Pattern.curveInfo aCurve pattern of
        Nothing ->
            Nothing

        Just info ->
            case info of
                Straight stuff ->
                    let
                        toForm startPoint endPoint =
                            StraightForm
                                { startPoint = startPoint
                                , endPoint = endPoint
                                , pointsHelp = Nothing
                                }
                    in
                    Maybe.map2 toForm
                        (initOtherPointFormWith pattern stuff.startPoint)
                        (initOtherPointFormWith pattern stuff.endPoint)

                Quadratic stuff ->
                    let
                        toForm startPoint controlPoint endPoint =
                            QuadraticForm
                                { startPoint = startPoint
                                , controlPoint = controlPoint
                                , endPoint = endPoint
                                , pointsHelp = Nothing
                                }
                    in
                    Maybe.map3 toForm
                        (initOtherPointFormWith pattern stuff.startPoint)
                        (initOtherPointFormWith pattern stuff.controlPoint)
                        (initOtherPointFormWith pattern stuff.endPoint)

                Cubic stuff ->
                    let
                        toForm startPoint startControlPoint endControlPoint endPoint =
                            CubicForm
                                { startPoint = startPoint
                                , startControlPoint = startControlPoint
                                , endControlPoint = endControlPoint
                                , endPoint = endPoint
                                , pointsHelp = Nothing
                                }
                    in
                    Maybe.map4 toForm
                        (initOtherPointFormWith pattern stuff.startPoint)
                        (initOtherPointFormWith pattern stuff.startControlPoint)
                        (initOtherPointFormWith pattern stuff.endControlPoint)
                        (initOtherPointFormWith pattern stuff.endPoint)

                TransformedCurve stuff ->
                    Nothing


{-| -}
editDetail : Pattern -> A Pattern.Detail -> Maybe Edit
editDetail pattern aDetail =
    case Pattern.detailInfo aDetail pattern of
        Nothing ->
            Nothing

        Just info ->
            let
                toForm firstCurve nextCurves lastCurve =
                    EditDetail
                        { aDetail = aDetail
                        , form =
                            { firstCurve = ( firstCurve, Closed )
                            , nextCurves = List.map close nextCurves
                            , lastCurve = ( lastCurve, Closed )
                            }
                        , objects = objects
                        }

                close nextCurve =
                    ( nextCurve, Closed )

                objects =
                    Pattern.objectsNotDependingOnDetail pattern aDetail
            in
            Maybe.map3 toForm
                (initFirstCurveFormWith pattern info.firstCurve)
                (initNextCurvesFormWith pattern info.nextCurves)
                (initLastCurveFormWith pattern info.lastCurve)


initFirstCurveFormWith : Pattern -> FirstCurve -> Maybe FirstCurveForm
initFirstCurveFormWith pattern firstCurve =
    case firstCurve of
        FirstStraight stuff ->
            let
                toForm startPoint endPoint =
                    FirstStraightForm
                        { startPoint = startPoint
                        , endPoint = endPoint
                        }
            in
            Maybe.map2 toForm
                (initOtherPointFormWith pattern stuff.startPoint)
                (initOtherPointFormWith pattern stuff.endPoint)

        FirstQuadratic stuff ->
            let
                toForm startPoint controlPoint endPoint =
                    FirstQuadraticForm
                        { startPoint = startPoint
                        , controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            Maybe.map3 toForm
                (initOtherPointFormWith pattern stuff.startPoint)
                (initOtherPointFormWith pattern stuff.controlPoint)
                (initOtherPointFormWith pattern stuff.endPoint)

        FirstCubic stuff ->
            let
                toForm startPoint startControlPoint endControlPoint endPoint =
                    FirstCubicForm
                        { startPoint = startPoint
                        , startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            Maybe.map4 toForm
                (initOtherPointFormWith pattern stuff.startPoint)
                (initOtherPointFormWith pattern stuff.startControlPoint)
                (initOtherPointFormWith pattern stuff.endControlPoint)
                (initOtherPointFormWith pattern stuff.endPoint)

        FirstReferencedCurve stuff ->
            Just <|
                FirstReferencedCurveForm
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }


initNextCurvesFormWith : Pattern -> List NextCurve -> Maybe (List NextCurveForm)
initNextCurvesFormWith pattern nextCurves =
    initNextCurvesFormWithHelp pattern nextCurves []


initNextCurvesFormWithHelp :
    Pattern
    -> List NextCurve
    -> List NextCurveForm
    -> Maybe (List NextCurveForm)
initNextCurvesFormWithHelp pattern nextCurves collected =
    case nextCurves of
        [] ->
            Just (List.reverse collected)

        nextCurve :: rest ->
            case nextCurveFormWith pattern nextCurve of
                Nothing ->
                    Nothing

                Just nextCurveForm ->
                    initNextCurvesFormWithHelp pattern rest (nextCurveForm :: collected)


nextCurveFormWith : Pattern -> NextCurve -> Maybe NextCurveForm
nextCurveFormWith pattern nextCurve =
    case nextCurve of
        NextStraight stuff ->
            let
                toForm endPoint =
                    NextStraightForm
                        { endPoint = endPoint }
            in
            Maybe.map toForm
                (initOtherPointFormWith pattern stuff.endPoint)

        NextQuadratic stuff ->
            let
                toForm controlPoint endPoint =
                    NextQuadraticForm
                        { controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            Maybe.map2 toForm
                (initOtherPointFormWith pattern stuff.controlPoint)
                (initOtherPointFormWith pattern stuff.endPoint)

        NextCubic stuff ->
            let
                toForm startControlPoint endControlPoint endPoint =
                    NextCubicForm
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            Maybe.map3 toForm
                (initOtherPointFormWith pattern stuff.startControlPoint)
                (initOtherPointFormWith pattern stuff.endControlPoint)
                (initOtherPointFormWith pattern stuff.endPoint)

        NextReferencedCurve stuff ->
            Just <|
                NextReferencedCurveForm
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }


initLastCurveFormWith : Pattern -> LastCurve -> Maybe LastCurveForm
initLastCurveFormWith pattern lastCurve =
    case lastCurve of
        LastStraight ->
            Just LastStraightForm

        LastQuadratic stuff ->
            let
                toForm controlPoint =
                    LastQuadraticForm { controlPoint = controlPoint }
            in
            Maybe.map toForm
                (initOtherPointFormWith pattern stuff.controlPoint)

        LastCubic stuff ->
            let
                toForm startControlPoint endControlPoint =
                    LastCubicForm
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        }
            in
            Maybe.map2 toForm
                (initOtherPointFormWith pattern stuff.startControlPoint)
                (initOtherPointFormWith pattern stuff.endControlPoint)

        LastReferencedCurve stuff ->
            Just <|
                LastReferencedCurveForm
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }


initOtherPointFormWith : Pattern -> A Point -> Maybe OtherPointForm
initOtherPointFormWith pattern aPoint =
    if Pattern.inlined aPoint then
        let
            toForm point =
                InlinedPointForm
                    { expanded = False
                    , point = point
                    }
        in
        Maybe.map toForm (initPointFormWith pattern aPoint)

    else
        Just <|
            ReferencedPointForm
                { dropdown = Ui.Atom.Dropdown.init
                , maybeAPoint = Just aPoint
                , help = Nothing
                }


initOtherIntersectableFormWith :
    Pattern
    -> A Intersectable
    -> Maybe OtherIntersectableForm
initOtherIntersectableFormWith pattern aIntersectable =
    if Pattern.inlined aIntersectable then
        case Pattern.tagFromIntersectable pattern aIntersectable of
            Just IntersectableAxisTag ->
                let
                    toForm axis =
                        InlinedAxisForm
                            { expanded = False
                            , axis = axis
                            }
                in
                Pattern.axisFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initAxisFormWith pattern)
                    |> Maybe.map toForm

            Just IntersectableCircleTag ->
                let
                    toForm circle =
                        InlinedCircleForm
                            { expanded = False
                            , circle = circle
                            }
                in
                Pattern.circleFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initCircleFormWith pattern)
                    |> Maybe.map toForm

            Just IntersectableCurveTag ->
                let
                    toForm curve =
                        InlinedCurveForm
                            { expanded = False
                            , curve = curve
                            }
                in
                Pattern.curveFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initCurveFormWith pattern)
                    |> Maybe.map toForm

            Nothing ->
                Nothing

    else
        Just <|
            ReferencedIntersectableForm
                { dropdown = Ui.Atom.Dropdown.init
                , maybeAIntersectable = Just aIntersectable
                , help = Nothing
                }



-- POINT FORM


initFromOnePointForm : PointForm
initFromOnePointForm =
    FromOnePointForm
        { basePoint = initReferencedPointForm
        , direction = DirectionAngle ""
        , directionHelp = Nothing
        , distance = ""
        , distanceHelp = Nothing
        }


initFromTwoPointsForm : PointForm
initFromTwoPointsForm =
    FromTwoPointsForm
        { basePointA = initReferencedPointForm
        , basePointB = initReferencedPointForm
        , pointsHelp = Nothing
        , twoPointsPosition =
            TwoPointsPositionRatio
                { ratio = ""
                , ratioHelp = Nothing
                }
        }


initIntersectionForm : PointForm
initIntersectionForm =
    IntersectionForm
        { objectA = initReferencedIntersectableForm
        , objectB = initReferencedIntersectableForm
        , objectsHelp = Nothing
        , which = 1
        , whichHelp = Nothing
        }



-- AXIS FORM


initThroughOnePointForm : AxisForm
initThroughOnePointForm =
    ThroughOnePointForm
        { point = initReferencedPointForm
        , orientation = OrientationAngle ""
        , orientationHelp = Nothing
        }


initThroughTwoPointsForm : AxisForm
initThroughTwoPointsForm =
    ThroughTwoPointsForm
        { pointA = initReferencedPointForm
        , pointB = initReferencedPointForm
        , pointsHelp = Nothing
        }



-- CIRCLE FORM


initWithRadiusForm : CircleForm
initWithRadiusForm =
    WithRadiusForm
        { centerPoint = initReferencedPointForm
        , radius = ""
        , radiusHelp = Nothing
        }


initThroughThreePointsForm : CircleForm
initThroughThreePointsForm =
    ThroughThreePointsForm
        { pointA = initReferencedPointForm
        , pointB = initReferencedPointForm
        , pointC = initReferencedPointForm
        , pointsHelp = Nothing
        }



-- CURVE FORM


initStraightForm : CurveForm
initStraightForm =
    StraightForm
        { startPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        , pointsHelp = Nothing
        }


initQuadraticForm : CurveForm
initQuadraticForm =
    QuadraticForm
        { startPoint = initReferencedPointForm
        , controlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        , pointsHelp = Nothing
        }


initCubicForm : CurveForm
initCubicForm =
    CubicForm
        { startPoint = initReferencedPointForm
        , startControlPoint = initReferencedPointForm
        , endControlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        , pointsHelp = Nothing
        }



-- DETAIL FORM


initDetailForm : DetailForm
initDetailForm =
    { firstCurve =
        ( initFirstStraightForm
        , Closed
        )
    , nextCurves = []
    , lastCurve =
        ( LastStraightForm
        , Closed
        )
    }


initFirstStraightForm : FirstCurveForm
initFirstStraightForm =
    FirstStraightForm
        { startPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        }


initFirstQuadraticForm : FirstCurveForm
initFirstQuadraticForm =
    FirstQuadraticForm
        { startPoint = initReferencedPointForm
        , controlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        }


initFirstCubicForm : FirstCurveForm
initFirstCubicForm =
    FirstCubicForm
        { startPoint = initReferencedPointForm
        , startControlPoint = initReferencedPointForm
        , endControlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        }


initFirstReferencedCurveForm : FirstCurveForm
initFirstReferencedCurveForm =
    FirstReferencedCurveForm
        { curve = initOtherCurveForm
        , reversed = False
        }


initNextStraightForm : NextCurveForm
initNextStraightForm =
    NextStraightForm
        { endPoint = initReferencedPointForm }


initNextQuadraticForm : NextCurveForm
initNextQuadraticForm =
    NextQuadraticForm
        { controlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        }


initNextCubicForm : NextCurveForm
initNextCubicForm =
    NextCubicForm
        { startControlPoint = initReferencedPointForm
        , endControlPoint = initReferencedPointForm
        , endPoint = initReferencedPointForm
        }


initNextReferencedCurveForm : NextCurveForm
initNextReferencedCurveForm =
    NextReferencedCurveForm
        { curve = initOtherCurveForm
        , reversed = False
        }


initLastStraightForm : LastCurveForm
initLastStraightForm =
    LastStraightForm


initLastQuadraticForm : LastCurveForm
initLastQuadraticForm =
    LastQuadraticForm
        { controlPoint = initReferencedPointForm }


initLastCubicForm : LastCurveForm
initLastCubicForm =
    LastCubicForm
        { startControlPoint = initReferencedPointForm
        , endControlPoint = initReferencedPointForm
        }


initLastReferencedCurveForm : LastCurveForm
initLastReferencedCurveForm =
    LastReferencedCurveForm
        { curve = initOtherCurveForm
        , reversed = False
        }


initOtherCurveForm : OtherCurveForm
initOtherCurveForm =
    { dropdown = Ui.Atom.Dropdown.init
    , maybeACurve = Nothing
    }



-- OTHER FORM


initReferencedPointForm : OtherPointForm
initReferencedPointForm =
    ReferencedPointForm
        { dropdown = Ui.Atom.Dropdown.init
        , maybeAPoint = Nothing
        , help = Nothing
        }


initInlinedPointForm : OtherPointForm
initInlinedPointForm =
    InlinedPointForm
        { expanded = True
        , point = initFromOnePointForm
        }


initReferencedIntersectableForm : OtherIntersectableForm
initReferencedIntersectableForm =
    ReferencedIntersectableForm
        { dropdown = Ui.Atom.Dropdown.init
        , maybeAIntersectable = Nothing
        , help = Nothing
        }


initInlinedAxisForm : OtherIntersectableForm
initInlinedAxisForm =
    InlinedAxisForm
        { expanded = True
        , axis = initThroughOnePointForm
        }


initInlinedCircleForm : OtherIntersectableForm
initInlinedCircleForm =
    InlinedCircleForm
        { expanded = True
        , circle = initWithRadiusForm
        }


initInlinedCurveForm : OtherIntersectableForm
initInlinedCurveForm =
    InlinedCurveForm
        { expanded = True
        , curve = initStraightForm
        }



---- VIEW


{-| -}
createView :
    { pattern : Pattern, hoveredInCanvas : Maybe ThatObject }
    -> Create
    -> Element CreateMsg
createView { pattern } (Create { name, nameHelp, dialog }) =
    let
        allObjects =
            Pattern.objects pattern
    in
    case dialog of
        DialogPoint point ->
            viewPointForm pattern allObjects name nameHelp point

        DialogAxis axis ->
            viewAxisForm pattern allObjects name nameHelp axis

        DialogCircle circle ->
            viewCircleForm pattern allObjects name nameHelp circle

        DialogCurve curve ->
            viewCurveForm pattern allObjects name nameHelp curve

        DialogDetail detail ->
            viewDetailForm pattern allObjects name nameHelp detail


viewActions : String -> Maybe String -> Element CreateMsg
viewActions name nameHelp =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.small
        ]
        [ View.Input.text "name-input"
            { onChange = NameChanged
            , text = name
            , label = "Pick a name"
            , help = nameHelp
            }
        , Element.row
            [ Element.width Element.fill
            , Element.spacing Design.xxSmall
            ]
            [ Element.el [ Element.alignLeft ] <|
                View.Input.btnPrimary
                    { onPress = Just CreatePressed
                    , label = "Create"
                    }
            , Element.el [ Element.alignLeft ] <|
                View.Input.btnSecondary "preview"
                    { onPress = Just CreatePreviewPressed
                    , label = "Preview"
                    }
            , Element.el [ Element.alignRight ] <|
                View.Input.btnCancel
                    { onPress = Just CreateCancelPressed
                    , label = "Cancel"
                    }
            ]
        ]



---- VIEW CREATE POINT DIALOG


viewPointForm :
    Pattern
    -> Pattern.Objects
    -> String
    -> Maybe String
    -> PointForm
    -> Element CreateMsg
viewPointForm pattern objects name nameHelp form =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ Element.map CreatePointMsg <|
            elCreateANew "point" <|
                viewPointFormHelp pattern
                    objects
                    { point = form
                    , id = "new-point"
                    }
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Design.small
            ]
            [ viewActions name nameHelp
            , Element.paragraph
                [ Design.fontNormal
                , Font.color Design.black
                , Font.size 14
                ]
                [ Element.text "or use this point as a base point "
                , Input.button
                    [ Font.underline
                    , Font.color Design.primary
                    , Element.mouseOver
                        [ Font.color Design.primaryDark ]
                    , Element.htmlAttribute <|
                        Attributes.style "transition" "color 0.2s ease-in-out 0s"
                    ]
                    { onPress = Just PointUseInPointPressed
                    , label = Element.text "for another point"
                    }
                , Element.text " or "
                , Input.button
                    [ Font.underline
                    , Font.color Design.primary
                    , Element.mouseOver
                        [ Font.color Design.primaryDark ]
                    , Element.htmlAttribute <|
                        Attributes.style "transition" "color 0.2s ease-in-out 0s"
                    ]
                    { onPress = Just PointUseInAxisPressed
                    , label = Element.text "for an axis"
                    }
                , Element.text ", or as the center point "
                , Input.button
                    [ Font.underline
                    , Font.color Design.primary
                    , Element.mouseOver
                        [ Font.color Design.primaryDark ]
                    , Element.htmlAttribute <|
                        Attributes.style "transition" "color 0.2s ease-in-out 0s"
                    ]
                    { onPress = Just PointUseInCirclePressed
                    , label = Element.text "of a circle"
                    }
                ]
            ]
        ]


viewPointFormHelp :
    Pattern
    -> Pattern.Objects
    ->
        { point : PointForm
        , id : String
        }
    -> Element PointMsg
viewPointFormHelp pattern objects { point, id } =
    let
        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Design.small ]
    in
    Ui.Atom.segmentControl
        { id = id
        , label = Nothing
        , help =
            case point of
                FromOnePointForm _ ->
                    Nothing

                FromTwoPointsForm stuff ->
                    stuff.pointsHelp

                IntersectionForm _ ->
                    Nothing
        , onChange = PointTypeChanged
        , options = pointTags
        , selected = tagFromPointForm point
        , child =
            nested <|
                case point of
                    FromOnePointForm stuff ->
                        [ Element.map FromOnePoint_BasePointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.basePoint
                                , id = id ++ "__from-one-point--base-point"
                                , label = "Base point"
                                }
                        , Element.map FromOnePoint_DirectionMsg <|
                            viewDirection
                                { direction = stuff.direction
                                , id = id ++ "__from-one-point--direction"
                                , help = stuff.directionHelp
                                }
                        , View.Input.formula "distance"
                            { onChange = FromOnePoint_DistanceChanged
                            , text = stuff.distance
                            , label = "Distance"
                            , help = stuff.distanceHelp
                            }
                        ]

                    FromTwoPointsForm stuff ->
                        [ Element.map FromTwoPoints_BasePointAMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.basePointA
                                , id = id ++ "__from-two-points--base-point-a"
                                , label = "1st base point"
                                }
                        , Element.map FromTwoPoints_BasePointBMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.basePointB
                                , id = id ++ "__from-two-points--base-point-b"
                                , label = "2nd base point"
                                }
                        , Element.map FromTwoPoints_TwoPointsPositionMsg <|
                            viewTwoPointsPosition
                                { twoPointsPosition = stuff.twoPointsPosition
                                , id = id ++ "__from-two-points--two-points-position"
                                }
                        ]

                    IntersectionForm stuff ->
                        let
                            whichSize =
                                Maybe.withDefault 1 <|
                                    Maybe.map2 Pattern.whichSize
                                        (intersectableFrom stuff.objectA)
                                        (intersectableFrom stuff.objectB)

                            intersectableFrom object =
                                case object of
                                    ReferencedIntersectableForm { maybeAIntersectable } ->
                                        Maybe.andThen (Pattern.tagFromIntersectable pattern)
                                            maybeAIntersectable

                                    InlinedAxisForm _ ->
                                        Just IntersectableAxisTag

                                    InlinedCircleForm _ ->
                                        Just IntersectableCircleTag

                                    InlinedCurveForm _ ->
                                        Just IntersectableCurveTag
                        in
                        [ Element.map Intersection_ObjectAMsg <|
                            viewOtherIntersectableForm pattern
                                objects
                                { otherIntersectable = stuff.objectA
                                , id = "__intersection--object-a"
                                , label = "1st object"
                                }
                        , Element.map Intersection_ObjectBMsg <|
                            viewOtherIntersectableForm pattern
                                objects
                                { otherIntersectable = stuff.objectB
                                , id = "__intersection--object-b"
                                , label = "2nd object"
                                }
                        , if whichSize > 1 then
                            Ui.Atom.segmentControl
                                { id = id ++ "__which"
                                , label = Just "Which intersection?"
                                , help = Nothing
                                , onChange = Intersection_WhichChanged
                                , options =
                                    List.range 1 whichSize
                                        |> List.map
                                            (\index ->
                                                ( index, "Intersection #" ++ String.fromInt index )
                                            )
                                , selected = stuff.which
                                , child = Nothing
                                }

                          else
                            Element.none
                        ]
        }



---- VIEW CREATE AXIS DIALOG


viewAxisForm :
    Pattern
    -> Pattern.Objects
    -> String
    -> Maybe String
    -> AxisForm
    -> Element CreateMsg
viewAxisForm pattern objects name nameHelp form =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ Element.map CreateAxisMsg <|
            elCreateANew "axis" <|
                viewAxisFormHelp pattern
                    objects
                    { axis = form
                    , id = "new-axis"
                    }
        , viewActions name nameHelp
        ]


viewAxisFormHelp :
    Pattern
    -> Pattern.Objects
    -> { axis : AxisForm, id : String }
    -> Element AxisMsg
viewAxisFormHelp pattern objects { axis, id } =
    let
        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Design.small ]
    in
    Ui.Atom.segmentControl
        { id = id
        , label = Nothing
        , help = Nothing
        , onChange = AxisTypeChanged
        , options = axisTags
        , selected = tagFromAxisForm axis
        , child =
            nested <|
                case axis of
                    ThroughOnePointForm stuff ->
                        [ Element.map ThroughOnePoint_PointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.point
                                , id = id ++ "__through-one-point--point"
                                , label = "Point"
                                }
                        , Element.map ThroughOnePoint_OrientationMsg <|
                            viewOrientation
                                { orientation = stuff.orientation
                                , id = id ++ "__through-one-point--orientation"
                                }
                        ]

                    ThroughTwoPointsForm stuff ->
                        [ Element.map ThroughTwoPoints_PointAMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.pointA
                                , id = id ++ "__through-two-points--point-a"
                                , label = "1st point"
                                }
                        , Element.map ThroughTwoPoints_PointBMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.pointB
                                , id = id ++ "__through-two-points--point-b"
                                , label = "2nd point"
                                }
                        ]
        }



---- VIEW CREATE CIRCLE DIALOG


viewCircleForm :
    Pattern
    -> Pattern.Objects
    -> String
    -> Maybe String
    -> CircleForm
    -> Element CreateMsg
viewCircleForm pattern objects name nameHelp form =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ Element.map CreateCircleMsg <|
            elCreateANew "circle" <|
                viewCircleFormHelp pattern
                    objects
                    { circle = form
                    , id = "new-circle"
                    }
        , viewActions name nameHelp
        ]


viewCircleFormHelp :
    Pattern
    -> Pattern.Objects
    -> { circle : CircleForm, id : String }
    -> Element CircleMsg
viewCircleFormHelp pattern objects { circle, id } =
    let
        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Design.small ]
    in
    Ui.Atom.segmentControl
        { id = id
        , label = Nothing
        , help = Nothing
        , onChange = CircleTypeChanged
        , options = circleTags
        , selected = tagFromCircleForm circle
        , child =
            nested <|
                case circle of
                    WithRadiusForm stuff ->
                        [ Element.map WithRadius_CenterPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.centerPoint
                                , id = id ++ "__with-radius--center-point"
                                , label = "Center point"
                                }
                        , View.Input.formula "radius"
                            { onChange = WithRadius_RadiusChanged
                            , text = stuff.radius
                            , label = "Radius"
                            , help = stuff.radiusHelp
                            }
                        ]

                    ThroughThreePointsForm stuff ->
                        [ Element.map ThroughThreePoints_PointAMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.pointA
                                , id = "__through-three-points--point-a"
                                , label = "1st point"
                                }
                        , Element.map ThroughThreePoints_PointBMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.pointB
                                , id = "__through-three-points--point-b"
                                , label = "2nd point"
                                }
                        , Element.map ThroughThreePoints_PointCMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.pointC
                                , id = "__through-three-points--point-c"
                                , label = "3rd point"
                                }
                        ]
        }



---- VIEW CREATE CURVE DIALOG


viewCurveForm :
    Pattern
    -> Pattern.Objects
    -> String
    -> Maybe String
    -> CurveForm
    -> Element CreateMsg
viewCurveForm pattern objects name nameHelp form =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ Element.map CreateCurveMsg <|
            elCreateANew "curve" <|
                viewCurveFormHelp pattern
                    objects
                    { curve = form
                    , id = "new-curve"
                    }
        , viewActions name nameHelp
        ]


viewCurveFormHelp :
    Pattern
    -> Pattern.Objects
    -> { curve : CurveForm, id : String }
    -> Element CurveMsg
viewCurveFormHelp pattern objects { curve, id } =
    let
        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Design.small ]
    in
    Ui.Atom.segmentControl
        { id = id
        , label = Nothing
        , help = Nothing
        , onChange = CurveTypeChanged
        , options = curveTags
        , selected = tagFromCurveForm curve
        , child =
            nested <|
                case curve of
                    StraightForm stuff ->
                        [ Element.map StartPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.startPoint
                                , id = "__straight--start-point"
                                , label = "Start point"
                                }
                        , Element.map EndPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.endPoint
                                , id = "__straight--end-point"
                                , label = "End point"
                                }
                        ]

                    QuadraticForm stuff ->
                        [ Element.map StartPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.startPoint
                                , id = "__quadratic--start-point"
                                , label = "Start point"
                                }
                        , Element.map ControlPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.controlPoint
                                , id = "__quadratic--control-point"
                                , label = "Control point"
                                }
                        , Element.map EndPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.endPoint
                                , id = "__quadratic--end-point"
                                , label = "End point"
                                }
                        ]

                    CubicForm stuff ->
                        [ Element.map StartPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.startPoint
                                , id = "__cubic--start-point"
                                , label = "Start point"
                                }
                        , Element.map StartControlPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.startControlPoint
                                , id = "__cubic--start-control-point"
                                , label = "Start control point"
                                }
                        , Element.map EndControlPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.endControlPoint
                                , id = "__cubic--end-control-point"
                                , label = "End control point"
                                }
                        , Element.map EndPointMsg <|
                            viewOtherPointForm pattern
                                objects
                                { otherPoint = stuff.endPoint
                                , id = "__cubic--end-point"
                                , label = "End point"
                                }
                        ]
        }



---- VIEW CREATE DETAIL DIALOG


viewDetailForm :
    Pattern
    -> Pattern.Objects
    -> String
    -> Maybe String
    -> DetailForm
    -> Element CreateMsg
viewDetailForm pattern objects name nameHelp detail =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ Element.map CreateDetailMsg <|
            elCreateANew "detail" <|
                viewDetailFormHelp pattern
                    objects
                    { detail = detail
                    , id = "new-detail"
                    }
        , viewActions name nameHelp
        ]


viewDetailFormHelp :
    Pattern
    -> Pattern.Objects
    ->
        { detail : DetailForm
        , id : String
        }
    -> Element DetailMsg
viewDetailFormHelp pattern objects { detail, id } =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.small
        ]
        (List.concat
            [ [ Ui.Atom.segmentControl
                    { id = id ++ "__first-curve-label"
                    , label = Just "1st curve"
                    , help = Nothing
                    , onChange = FirstCurveTypeChanged
                    , options = firstCurveTags
                    , selected = tagFromFirstCurveForm (Tuple.first detail.firstCurve)
                    , child =
                        case Tuple.first detail.firstCurve of
                            FirstStraightForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.column
                                            [ Element.width Element.fill
                                            , Element.spacing Design.small
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-straight--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.endPoint
                                                    , id = id ++ "__first-straight--end-point"
                                                    , label = "End point"
                                                    }
                                            ]

                            FirstQuadraticForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.column
                                            [ Element.width Element.fill
                                            , Element.spacing Design.small
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-quadratic--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveControlPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.controlPoint
                                                    , id = id ++ "__first-quadratic--control-point"
                                                    , label = "Control point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.endPoint
                                                    , id = id ++ "__first-quadratic--end-point"
                                                    , label = "End point"
                                                    }
                                            ]

                            FirstCubicForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.column
                                            [ Element.width Element.fill
                                            , Element.spacing Design.small
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-cubic--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveStartControlPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.startControlPoint
                                                    , id =
                                                        id ++ "__first-cubic--start-control-point"
                                                    , label = "Start control point"
                                                    }
                                            , Element.map FirstCurveEndControlPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.endControlPoint
                                                    , id = id ++ "__first-cubic--end-control-point"
                                                    , label = "End control point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.endPoint
                                                    , id = id ++ "__first-cubic--end-point"
                                                    , label = "End point"
                                                    }
                                            ]

                            FirstReferencedCurveForm { curve } ->
                                Just <|
                                    Ui.Atom.Dropdown.viewAppended
                                        { entryToString = objectName
                                        , entryToHash = Pattern.hash
                                        }
                                        { id = id ++ "__first-referenced-curve"
                                        , lift = FirstCurveDropdownMsg
                                        , label = "First curve"
                                        }
                                        objects.curves
                                        curve.dropdown
                                        curve.maybeACurve
                    }
              , case Tuple.first detail.firstCurve of
                    FirstReferencedCurveForm { reversed } ->
                        View.Input.checkbox
                            { onChange = FirstCurveReverseChanged
                            , checked = reversed
                            , label = "Reverse curve"
                            }

                    _ ->
                        Element.none
              ]
            , List.indexedMap (viewNextCurve pattern objects id) detail.nextCurves
            , [ View.Input.btnSecondary "add-curve-button"
                    { onPress = Just AddCurvePressed
                    , label = "Add Curve"
                    }
              , Ui.Atom.segmentControl
                    { id = id ++ "__last-curve-label"
                    , label = Just "Closing curve"
                    , help = Nothing
                    , onChange = LastCurveTypeChanged
                    , options = lastCurveTags
                    , selected = tagFromLastCurveForm (Tuple.first detail.lastCurve)
                    , child =
                        case Tuple.first detail.lastCurve of
                            LastStraightForm ->
                                Nothing

                            LastQuadraticForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.map LastCurveControlPointMsg <|
                                            viewOtherPointForm pattern
                                                objects
                                                { otherPoint = stuff.controlPoint
                                                , id = id ++ "__last-quadratic--control-point"
                                                , label = "Control point"
                                                }

                            LastCubicForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.column
                                            [ Element.width Element.fill
                                            , Element.spacing Design.small
                                            ]
                                            [ Element.map LastCurveStartControlPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.startControlPoint
                                                    , id = id ++ "__last-cubic--start-control-point"
                                                    , label = "Start control point"
                                                    }
                                            , Element.map LastCurveEndControlPointMsg <|
                                                viewOtherPointForm pattern
                                                    objects
                                                    { otherPoint = stuff.endControlPoint
                                                    , id = id ++ "__last-cubic--end-control-point"
                                                    , label = "End control point"
                                                    }
                                            ]

                            LastReferencedCurveForm { curve } ->
                                Just <|
                                    Ui.Atom.Dropdown.viewAppended
                                        { entryToString = objectName
                                        , entryToHash = Pattern.hash
                                        }
                                        { id = id ++ "__last-referenced-curve"
                                        , lift = LastCurveDropdownMsg
                                        , label = "Last curve"
                                        }
                                        objects.curves
                                        curve.dropdown
                                        curve.maybeACurve
                    }
              , case Tuple.first detail.lastCurve of
                    LastReferencedCurveForm { reversed } ->
                        View.Input.checkbox
                            { onChange = LastCurveReverseChanged
                            , checked = reversed
                            , label = "Reverse curve"
                            }

                    _ ->
                        Element.none
              ]
            ]
        )


viewNextCurve :
    Pattern
    -> Pattern.Objects
    -> String
    -> Int
    -> ( NextCurveForm, ActionMenu )
    -> Element DetailMsg
viewNextCurve pattern objects id index ( form, actionMenu ) =
    let
        actualId =
            id ++ "__next-" ++ String.fromInt index
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.small
        ]
        [ Ui.Atom.segmentControl
            { id = actualId
            , label = Just (ordinalFromInt (index + 2) ++ " Curve")
            , help = Nothing
            , onChange = NextCurveTypeChanged index
            , options = nextCurveTags
            , selected = tagFromNextCurveForm form
            , child =
                case form of
                    NextStraightForm stuff ->
                        Just <|
                            Ui.Atom.nested <|
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing Design.small
                                    ]
                                    [ Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.endPoint
                                            , id = actualId ++ "__next-straight--end-point"
                                            , label = "End point"
                                            }
                                    ]

                    NextQuadraticForm stuff ->
                        Just <|
                            Ui.Atom.nested <|
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing Design.small
                                    ]
                                    [ Element.map (NextCurveControlPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.controlPoint
                                            , id = actualId ++ "__next-quadratic--control-point"
                                            , label = "Control point"
                                            }
                                    , Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.endPoint
                                            , id = actualId ++ "__next-quadratic--end-point"
                                            , label = "End point"
                                            }
                                    ]

                    NextCubicForm stuff ->
                        Just <|
                            Ui.Atom.nested <|
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing Design.small
                                    ]
                                    [ Element.map (NextCurveStartControlPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.startControlPoint
                                            , id = actualId ++ "__next-cubic--start-control-point"
                                            , label = "Start control point"
                                            }
                                    , Element.map (NextCurveEndControlPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.endControlPoint
                                            , id = actualId ++ "__next-cubic--end-control-point"
                                            , label = "End control point"
                                            }
                                    , Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm pattern
                                            objects
                                            { otherPoint = stuff.endPoint
                                            , id = actualId ++ "__next-cubic--end-point"
                                            , label = "End point"
                                            }
                                    ]

                    NextReferencedCurveForm { curve } ->
                        Just <|
                            Ui.Atom.Dropdown.viewAppended
                                { entryToString = objectName
                                , entryToHash = Pattern.hash
                                }
                                { id = actualId ++ "__referenced-curve"
                                , lift = NextCurveDropdownMsg index
                                , label = ordinalFromInt (index + 2) ++ " Curve"
                                }
                                objects.curves
                                curve.dropdown
                                curve.maybeACurve
            }
        , case form of
            NextReferencedCurveForm { reversed } ->
                View.Input.checkbox
                    { onChange = NextCurveReverseChanged index
                    , checked = reversed
                    , label = "Reverse curve"
                    }

            _ ->
                Element.none
        ]


viewActionMenu : ActionMenu -> Element ActionMenuMsg
viewActionMenu actionMenu =
    Element.row
        (if actionMenu == Closed then
            [ Element.spacing Design.xxSmall
            , Element.alignRight
            ]

         else
            [ Element.spacing Design.xxSmall
            , Element.alignRight
            , Element.htmlAttribute <|
                Attributes.style "z-index" "1"
            ]
        )
        [ Input.button
            [ Element.paddingEach
                { left = Design.xxSmall
                , right = Design.xxSmall
                , top = Design.xxSmall
                , bottom = Design.xxSmall - 2
                }
            , Font.size 10
            , Font.color Design.black
            , Border.widthEach
                { left = 0
                , right = 0
                , top = 0
                , bottom = 2
                }
            , Border.color Design.secondary
            , Background.color Design.secondary
            , Element.mouseOver
                [ Background.color Design.secondaryDark
                , Border.color Design.black
                ]
            , Element.focused
                [ Border.color Design.black ]
            , Element.htmlAttribute <|
                Attributes.style "transition" <|
                    String.join "; "
                        [ "background-color 0.2s ease-in-out 0s"
                        , "border-color 0.2s ease-in-out 0s"
                        ]
            , Element.below <|
                case actionMenu of
                    Closed ->
                        Element.none

                    _ ->
                        let
                            viewAction msg label =
                                Element.el
                                    [ Element.paddingXY 8 7
                                    , Element.width Element.fill
                                    , Background.color Design.secondary
                                    , Element.mouseOver
                                        [ Background.color Design.secondaryDark ]
                                    , Element.htmlAttribute <|
                                        Attributes.tabindex -1
                                    , Element.htmlAttribute <|
                                        Html.Events.stopPropagationOn "click" <|
                                            Decode.succeed
                                                ( msg, True )
                                    ]
                                    (Element.text label)
                        in
                        Element.column
                            [ Events.onMouseDown MouseDown
                            , Events.onMouseUp MouseUp
                            , Element.moveDown 2
                            , Font.size 14
                            , Font.color Design.black
                            ]
                            [ viewAction MoveDownPressed "Move down"
                            , viewAction MoveUpPressed "Move up"
                            , viewAction InsertCurveBeforePressed
                                "Insert curve before"
                            , viewAction InsertCurveAfterPressed
                                "Insert curve after"
                            , viewAction RemovePressed "Remove"
                            ]
            , Events.onLoseFocus LostFocus
            ]
            { onPress = Just Pressed
            , label =
                Element.row
                    [ Element.spacing Design.xxSmall ]
                    [ Element.text "Actions"
                    , View.Icon.fa "angle-down"
                    ]
            }
        ]


{-| -}
editView :
    { pattern : Pattern
    , name : String
    , hoveredInCanvas : Maybe ThatObject
    }
    -> Edit
    -> Element EditMsg
editView { pattern, name } edit =
    let
        actions =
            Element.row
                [ Element.width Element.fill
                , Element.spacing Design.xxSmall
                ]
                [ Element.el [ Element.alignLeft ] <|
                    View.Input.btnPrimary
                        { onPress = Just UpdatePressed
                        , label = "Update"
                        }
                , Element.el [ Element.alignLeft ] <|
                    View.Input.btnSecondary "preview"
                        { onPress = Just EditPreviewPressed
                        , label = "Preview"
                        }
                , Element.el [ Element.alignRight ] <|
                    View.Input.btnCancel
                        { onPress = Just EditCancelPressed
                        , label = "Cancel"
                        }
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.large
        , Element.padding Design.small
        ]
        [ case edit of
            EditPoint { objects, form } ->
                Element.map EditPointMsg <|
                    elEditThe { thing = "point", name = name } <|
                        viewPointFormHelp pattern
                            objects
                            { point = form
                            , id = "edit-point"
                            }

            EditAxis { objects, form } ->
                Element.map EditAxisMsg <|
                    elEditThe { thing = "axis", name = name } <|
                        viewAxisFormHelp pattern
                            objects
                            { axis = form
                            , id = "edit-axis"
                            }

            EditCircle { objects, form } ->
                Element.map EditCircleMsg <|
                    elEditThe { thing = "circle", name = name } <|
                        viewCircleFormHelp pattern
                            objects
                            { circle = form
                            , id = "edit-circle"
                            }

            EditCurve { objects, form } ->
                Element.map EditCurveMsg <|
                    elEditThe { thing = "curve", name = name } <|
                        viewCurveFormHelp pattern
                            objects
                            { curve = form
                            , id = "edit-curve"
                            }

            EditDetail { objects, form } ->
                Element.map EditDetailMsg <|
                    elEditThe { thing = "detail", name = name } <|
                        viewDetailFormHelp pattern
                            objects
                            { detail = form
                            , id = "edit-detail"
                            }
        , actions
        ]



---- SHARED VIEW FUNCTIONS


elCreateANew : String -> Element msg -> Element msg
elCreateANew thing element =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.small
        ]
        [ title ("Create a new " ++ thing)
        , element
        ]


elEditThe :
    { thing : String
    , name : String
    }
    -> Element msg
    -> Element msg
elEditThe { thing, name } element =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Design.small
        ]
        [ title ("Edit the " ++ thing ++ " ‘" ++ name ++ "’")
        , element
        ]


title : String -> Element msg
title text =
    Element.el
        [ Design.fontNormal
        , Font.color Design.black
        , Font.bold
        ]
        (Element.text text)


viewOtherPointForm :
    Pattern
    -> Pattern.Objects
    ->
        { otherPoint : OtherPointForm
        , id : String
        , label : String
        }
    -> Element OtherPointMsg
viewOtherPointForm pattern objects { otherPoint, id, label } =
    Ui.Atom.segmentControl
        { id = id
        , label = Just label
        , help =
            case otherPoint of
                ReferencedPointForm { help } ->
                    help

                _ ->
                    Nothing
        , onChange = OtherPointTypeChanged
        , options = otherPointTags
        , selected = tagFromOtherPointForm otherPoint
        , child =
            case otherPoint of
                ReferencedPointForm { dropdown, maybeAPoint, help } ->
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = objectName
                            , entryToHash = Pattern.hash
                            }
                            { id = id ++ "__referenced--point"
                            , lift = ReferencedPointDropdownMsg
                            , label = label
                            }
                            objects.points
                            dropdown
                            maybeAPoint

                InlinedPointForm { expanded, point } ->
                    Just <|
                        Ui.Atom.nestedHideable
                            { show = expanded
                            , onPress = InlinedPointExpandToggled
                            , shown =
                                Element.map InlinedPointMsg <|
                                    viewPointFormHelp pattern
                                        objects
                                        { point = point
                                        , id = id ++ "__inlined--point"
                                        }
                            , hidden = Element.none
                            }
        }


viewOtherIntersectableForm :
    Pattern
    -> Pattern.Objects
    ->
        { otherIntersectable : OtherIntersectableForm
        , id : String
        , label : String
        }
    -> Element OtherIntersectableMsg
viewOtherIntersectableForm pattern objects { otherIntersectable, id, label } =
    let
        selectedTag =
            tagFromOtherIntersectableForm otherIntersectable

        expanded =
            otherIntersectableFormExpanded otherIntersectable
    in
    Ui.Atom.segmentControl
        { id = id
        , label = Just label
        , help = Nothing
        , onChange = OtherIntersectableTypeChanged
        , options = otherIntersectableTags
        , selected = selectedTag
        , child =
            case otherIntersectable of
                ReferencedIntersectableForm { dropdown, maybeAIntersectable, help } ->
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = objectName
                            , entryToHash = Pattern.hash
                            }
                            { id = id ++ "__other-intersectable-object"
                            , lift = ReferencedIntersectableDropdownMsg
                            , label = label
                            }
                            (List.concat
                                [ List.map Pattern.intersectableAxis objects.axes
                                , List.map Pattern.intersectableCircle objects.circles
                                , List.map Pattern.intersectableCurve objects.curves
                                ]
                            )
                            dropdown
                            maybeAIntersectable

                InlinedAxisForm { axis } ->
                    Just <|
                        Ui.Atom.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedAxisMsg <|
                                    viewAxisFormHelp pattern
                                        objects
                                        { axis = axis
                                        , id = id ++ "__inlined--axis"
                                        }
                            , hidden = Element.none
                            }

                InlinedCircleForm { circle } ->
                    Just <|
                        Ui.Atom.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedCircleMsg <|
                                    viewCircleFormHelp pattern
                                        objects
                                        { circle = circle
                                        , id = id ++ "__inlined--circle"
                                        }
                            , hidden = Element.none
                            }

                InlinedCurveForm { curve } ->
                    Just <|
                        Ui.Atom.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedCurveMsg <|
                                    viewCurveFormHelp pattern
                                        objects
                                        { curve = curve
                                        , id = "__inlined--curve"
                                        }
                            , hidden = Element.none
                            }
        }


viewDirection :
    { direction : Direction
    , id : String
    , help : Maybe String
    }
    -> Element DirectionMsg
viewDirection { direction, id, help } =
    Ui.Atom.segmentControl
        { id = id ++ "__direction"
        , label = Just "Direction"
        , help = help
        , onChange = DirectionTypeChanged
        , options = directionTags
        , selected = tagFromDirection direction
        , child =
            case direction of
                DirectionAngle custom ->
                    Just <|
                        Ui.Atom.inputFormulaAppended
                            (id ++ "__direction--angle")
                            { onChange = CustomChanged
                            , text = custom
                            , label = "Angle"
                            }

                _ ->
                    Nothing
        }


viewOrientation :
    { orientation : Orientation
    , id : String
    }
    -> Element OrientationMsg
viewOrientation { orientation, id } =
    Ui.Atom.segmentControl
        { id = id ++ "__orientation"
        , label = Just "Orientation"
        , help = Nothing
        , onChange = OrientationTypeChanged
        , options = orientationTags
        , selected = tagFromOrientation orientation
        , child =
            case orientation of
                OrientationAngle custom ->
                    Just <|
                        Ui.Atom.inputFormulaAppended
                            (id ++ "__orientation--angle")
                            { onChange = CustomOrientationChanged
                            , text = custom
                            , label = "Angle"
                            }

                _ ->
                    Nothing
        }


viewTwoPointsPosition :
    { twoPointsPosition : TwoPointsPosition
    , id : String
    }
    -> Element TwoPointsPositionMsg
viewTwoPointsPosition { twoPointsPosition, id } =
    Ui.Atom.segmentControl
        { id = id ++ "__two-points-position"
        , label = Just "Position"
        , help =
            case twoPointsPosition of
                TwoPointsPositionRatio { ratioHelp } ->
                    ratioHelp

                TwoPointsPositionFromA { distanceHelp } ->
                    distanceHelp

                TwoPointsPositionFromB { distanceHelp } ->
                    distanceHelp
        , onChange = TwoPointsPosition_TypeChanged
        , options = twoPointsPositionTags
        , selected = tagFromTwoPointsPosition twoPointsPosition
        , child =
            Just <|
                case twoPointsPosition of
                    TwoPointsPositionRatio { ratio } ->
                        Ui.Atom.inputFormulaAppended
                            (id ++ "__two-point-distance-ratio-input")
                            { onChange = TwoPointsPosition_RatioChanged
                            , text = ratio
                            , label = "Ratio"
                            }

                    TwoPointsPositionFromA { distance } ->
                        Ui.Atom.inputFormulaAppended
                            (id ++ "__two-point-distance-distance-from-a-input")
                            { onChange = TwoPointsPosition_FromAChanged
                            , text = distance
                            , label = "Distance from 1st base point"
                            }

                    TwoPointsPositionFromB { distance } ->
                        Ui.Atom.inputFormulaAppended
                            (id ++ "__two-point-distance-distance-from-a-input")
                            { onChange = TwoPointsPosition_FromBChanged
                            , text = distance
                            , label = "Distance from 2nd base point"
                            }
        }



---- MSG


{-| -}
type CreateMsg
    = NameChanged String
    | CreatePressed
    | CreatePreviewPressed
    | CreateCancelPressed
    | CreatePointMsg PointMsg
    | CreateAxisMsg AxisMsg
    | CreateCircleMsg CircleMsg
    | CreateCurveMsg CurveMsg
    | CreateDetailMsg DetailMsg
      -- USE
    | PointUseInPointPressed
    | PointUseInAxisPressed
    | PointUseInCirclePressed


{-| -}
type EditMsg
    = UpdatePressed
    | EditPreviewPressed
    | EditCancelPressed
    | EditPointMsg PointMsg
    | EditAxisMsg AxisMsg
    | EditCircleMsg CircleMsg
    | EditCurveMsg CurveMsg
    | EditDetailMsg DetailMsg


type PointMsg
    = PointTypeChanged PointTag
      -- FROM ONE POINT
    | FromOnePoint_BasePointMsg OtherPointMsg
    | FromOnePoint_DirectionMsg DirectionMsg
    | FromOnePoint_DistanceChanged String
      -- FROM TWO POINTS
    | FromTwoPoints_BasePointAMsg OtherPointMsg
    | FromTwoPoints_BasePointBMsg OtherPointMsg
    | FromTwoPoints_TwoPointsPositionMsg TwoPointsPositionMsg
      -- INTERSECTION
    | Intersection_ObjectAMsg OtherIntersectableMsg
    | Intersection_ObjectBMsg OtherIntersectableMsg
    | Intersection_WhichChanged Int


type TwoPointsPositionMsg
    = TwoPointsPosition_TypeChanged TwoPointsPositionTag
    | TwoPointsPosition_RatioChanged String
    | TwoPointsPosition_FromAChanged String
    | TwoPointsPosition_FromBChanged String


type AxisMsg
    = AxisUseInPointPressed
    | AxisTypeChanged AxisTag
      -- THROUGH ONE POINT
    | ThroughOnePoint_PointMsg OtherPointMsg
    | ThroughOnePoint_OrientationMsg OrientationMsg
      -- THROUGH TWO POINTS
    | ThroughTwoPoints_PointAMsg OtherPointMsg
    | ThroughTwoPoints_PointBMsg OtherPointMsg


type CircleMsg
    = CircleUseInPointPressed
    | CircleTypeChanged CircleTag
      -- WITH RADIUS
    | WithRadius_CenterPointMsg OtherPointMsg
    | WithRadius_RadiusChanged String
      -- THROUGH THREE POINTS
    | ThroughThreePoints_PointAMsg OtherPointMsg
    | ThroughThreePoints_PointBMsg OtherPointMsg
    | ThroughThreePoints_PointCMsg OtherPointMsg


type CurveMsg
    = CurveTypeChanged CurveTag
      -- STRAIGHT & QUADRATIC & CUBIC
    | StartPointMsg OtherPointMsg
    | ControlPointMsg OtherPointMsg
    | StartControlPointMsg OtherPointMsg
    | EndControlPointMsg OtherPointMsg
    | EndPointMsg OtherPointMsg


type DetailMsg
    = AddCurvePressed
      -- FIRST CURVE
    | FirstCurveTypeChanged FirstCurveTag
    | FirstCurveStartPointMsg OtherPointMsg
    | FirstCurveStartControlPointMsg OtherPointMsg
    | FirstCurveControlPointMsg OtherPointMsg
    | FirstCurveEndControlPointMsg OtherPointMsg
    | FirstCurveEndPointMsg OtherPointMsg
    | FirstCurveDropdownMsg (Ui.Atom.Dropdown.Msg (A Curve))
    | FirstCurveReverseChanged Bool
    | FirstCurveActionMenuMsg ActionMenuMsg
      -- NEXT CURVE
    | NextCurveTypeChanged Int NextCurveTag
    | NextCurveStartControlPointMsg Int OtherPointMsg
    | NextCurveControlPointMsg Int OtherPointMsg
    | NextCurveEndControlPointMsg Int OtherPointMsg
    | NextCurveEndPointMsg Int OtherPointMsg
    | NextCurveDropdownMsg Int (Ui.Atom.Dropdown.Msg (A Curve))
    | NextCurveReverseChanged Int Bool
    | NextCurveActionMenuMsg Int ActionMenuMsg
      -- LAST CURVE
    | LastCurveTypeChanged LastCurveTag
    | LastCurveStartControlPointMsg OtherPointMsg
    | LastCurveControlPointMsg OtherPointMsg
    | LastCurveEndControlPointMsg OtherPointMsg
    | LastCurveDropdownMsg (Ui.Atom.Dropdown.Msg (A Curve))
    | LastCurveReverseChanged Bool
    | LastCurveActionMenuMsg ActionMenuMsg



-- ACTION MENUS


type ActionMenuMsg
    = MouseDown
    | MouseUp
    | LostFocus
    | Pressed
    | MoveDownPressed
    | MoveUpPressed
    | InsertCurveBeforePressed
    | InsertCurveAfterPressed
    | RemovePressed



-- NESTINGS


type OtherPointMsg
    = OtherPointTypeChanged OtherPointTag
    | ReferencedPointDropdownMsg (Ui.Atom.Dropdown.Msg (A Pattern.Point))
    | InlinedPointMsg PointMsg
    | InlinedPointExpandToggled


type OtherIntersectableMsg
    = OtherIntersectableTypeChanged OtherIntersectableTag
    | ReferencedIntersectableDropdownMsg (Ui.Atom.Dropdown.Msg (A Intersectable))
    | InlinedAxisMsg AxisMsg
    | InlinedCircleMsg CircleMsg
    | InlinedCurveMsg CurveMsg
    | InlinedIntersectableExpandToggled



-- SHARED


type DirectionMsg
    = DirectionTypeChanged DirectionTag
    | CustomChanged String


type OrientationMsg
    = OrientationTypeChanged OrientationTag
    | CustomOrientationChanged String



---- UPDATE


{-| -}
type CreateResult
    = CreateOpen ( Create, Cmd CreateMsg )
    | CreateSucceeded Pattern
    | CreateCanceled


{-| -}
createUpdate : Pattern -> CreateMsg -> Create -> CreateResult
createUpdate pattern msg ((Create stuff) as create) =
    case msg of
        NameChanged newName ->
            CreateOpen
                ( Create { stuff | name = newName }
                , Cmd.none
                )

        CreatePressed ->
            let
                addHelpWith dialogObject newForm =
                    CreateOpen
                        ( Create
                            { stuff
                                | dialog = dialogObject newForm
                                , nameHelp =
                                    if stuff.name == "" then
                                        Just "Pick a name"

                                    else
                                        stuff.nameHelp
                            }
                        , Cmd.none
                        )

                insertWith insertObject newObject =
                    if stuff.name == "" then
                        CreateOpen
                            ( Create
                                { stuff | nameHelp = Just "Pick a name" }
                            , Cmd.none
                            )

                    else
                        case insertObject stuff.name newObject pattern of
                            Err insertHelp ->
                                case insertHelp of
                                    NameTaken ->
                                        CreateOpen
                                            ( Create
                                                { stuff
                                                    | nameHelp = Just "Name already taken"
                                                }
                                            , Cmd.none
                                            )

                                    BadObject _ ->
                                        CreateOpen ( Create stuff, Cmd.none )

                                    NotImplementedYet ->
                                        CreateOpen ( Create stuff, Cmd.none )

                            Ok newPattern ->
                                CreateSucceeded newPattern
            in
            case clearHelp stuff.dialog of
                DialogPoint form ->
                    newPointFrom form pattern
                        |> Result.mapError (addHelpWith DialogPoint)
                        |> Result.map (insertWith Pattern.insertPoint)
                        |> Result.resolve

                DialogAxis form ->
                    newAxisFrom form pattern
                        |> Result.mapError (addHelpWith DialogAxis)
                        |> Result.map (insertWith Pattern.insertAxis)
                        |> Result.resolve

                DialogCircle form ->
                    newCircleFrom form pattern
                        |> Result.mapError (addHelpWith DialogCircle)
                        |> Result.map (insertWith Pattern.insertCircle)
                        |> Result.resolve

                DialogCurve form ->
                    newCurveFrom form pattern
                        |> Result.mapError (addHelpWith DialogCurve)
                        |> Result.map (insertWith Pattern.insertCurve)
                        |> Result.resolve

                DialogDetail form ->
                    newDetailFrom form pattern
                        |> Result.mapError (addHelpWith DialogDetail)
                        |> Result.map (insertWith Pattern.insertDetail)
                        |> Result.resolve

        CreatePreviewPressed ->
            CreateOpen ( create, Cmd.none )

        CreateCancelPressed ->
            CreateCanceled

        CreatePointMsg pointMsg ->
            case stuff.dialog of
                DialogPoint point ->
                    let
                        ( newPoint, pointCmd ) =
                            updatePointForm pattern (Pattern.objects pattern) pointMsg point
                    in
                    CreateOpen
                        ( Create { stuff | dialog = DialogPoint newPoint }
                        , Cmd.map CreatePointMsg pointCmd
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        CreateAxisMsg axisMsg ->
            case stuff.dialog of
                DialogAxis axis ->
                    let
                        ( newAxis, axisCmd ) =
                            updateAxisForm pattern (Pattern.objects pattern) axisMsg axis
                    in
                    CreateOpen
                        ( Create { stuff | dialog = DialogAxis newAxis }
                        , Cmd.map CreateAxisMsg axisCmd
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        CreateCircleMsg circleMsg ->
            case stuff.dialog of
                DialogCircle circle ->
                    let
                        ( newCircle, circleCmd ) =
                            updateCircleForm pattern (Pattern.objects pattern) circleMsg circle
                    in
                    CreateOpen
                        ( Create { stuff | dialog = DialogCircle newCircle }
                        , Cmd.map CreateCircleMsg circleCmd
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        CreateCurveMsg curveMsg ->
            case stuff.dialog of
                DialogCurve curve ->
                    let
                        ( newCurve, curveCmd ) =
                            updateCurveForm pattern (Pattern.objects pattern) curveMsg curve
                    in
                    CreateOpen
                        ( Create { stuff | dialog = DialogCurve newCurve }
                        , Cmd.map CreateCurveMsg curveCmd
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        CreateDetailMsg detailMsg ->
            case stuff.dialog of
                DialogDetail detail ->
                    let
                        ( newDetail, detailCmd ) =
                            updateDetailForm pattern (Pattern.objects pattern) detailMsg detail
                    in
                    CreateOpen
                        ( Create { stuff | dialog = DialogDetail newDetail }
                        , Cmd.map CreateDetailMsg detailCmd
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        -- USE
        PointUseInPointPressed ->
            case stuff.dialog of
                DialogPoint point ->
                    CreateOpen
                        ( Create
                            { stuff
                                | dialog =
                                    DialogPoint <|
                                        FromOnePointForm
                                            { basePoint =
                                                InlinedPointForm
                                                    { expanded = False
                                                    , point = point
                                                    }
                                            , direction = DirectionAngle ""
                                            , directionHelp = Nothing
                                            , distance = ""
                                            , distanceHelp = Nothing
                                            }
                            }
                        , Cmd.none
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        PointUseInAxisPressed ->
            case stuff.dialog of
                DialogPoint point ->
                    CreateOpen
                        ( Create
                            { stuff
                                | dialog =
                                    DialogAxis <|
                                        ThroughOnePointForm
                                            { point =
                                                InlinedPointForm
                                                    { expanded = False
                                                    , point = point
                                                    }
                                            , orientation = OrientationAngle ""
                                            , orientationHelp = Nothing
                                            }
                            }
                        , Cmd.none
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )

        PointUseInCirclePressed ->
            case stuff.dialog of
                DialogPoint point ->
                    CreateOpen
                        ( Create
                            { stuff
                                | dialog =
                                    DialogCircle <|
                                        WithRadiusForm
                                            { centerPoint =
                                                InlinedPointForm
                                                    { expanded = False
                                                    , point = point
                                                    }
                                            , radius = ""
                                            , radiusHelp = Nothing
                                            }
                            }
                        , Cmd.none
                        )

                _ ->
                    CreateOpen ( create, Cmd.none )


updatePointForm :
    Pattern
    -> Pattern.Objects
    -> PointMsg
    -> PointForm
    -> ( PointForm, Cmd PointMsg )
updatePointForm pattern objects pointMsg form =
    case ( pointMsg, form ) of
        ( PointTypeChanged pointTag, _ ) ->
            ( if tagFromPointForm form == pointTag then
                form

              else
                case pointTag of
                    FromOnePointTag ->
                        initFromOnePointForm

                    FromTwoPointsTag ->
                        initFromTwoPointsForm

                    IntersectionTag ->
                        initIntersectionForm
            , Cmd.none
            )

        -- FROM ONE POINT
        ( FromOnePoint_BasePointMsg subMsg, FromOnePointForm stuff ) ->
            let
                ( newBasePoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.basePoint
            in
            ( FromOnePointForm { stuff | basePoint = newBasePoint }
            , Cmd.map FromOnePoint_BasePointMsg subCmd
            )

        ( FromOnePoint_DirectionMsg subMsg, FromOnePointForm stuff ) ->
            ( FromOnePointForm
                { stuff | direction = updateDirection subMsg stuff.direction }
            , Cmd.none
            )

        ( FromOnePoint_DistanceChanged newDistance, FromOnePointForm stuff ) ->
            ( FromOnePointForm
                { stuff | distance = newDistance }
            , Cmd.none
            )

        -- FROM TWO POINTS
        ( FromTwoPoints_BasePointAMsg subMsg, FromTwoPointsForm stuff ) ->
            let
                ( newBasePointA, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.basePointA
            in
            ( FromTwoPointsForm { stuff | basePointA = newBasePointA }
            , Cmd.map FromTwoPoints_BasePointAMsg subCmd
            )

        ( FromTwoPoints_BasePointBMsg subMsg, FromTwoPointsForm stuff ) ->
            let
                ( newBasePointB, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.basePointB
            in
            ( FromTwoPointsForm { stuff | basePointB = newBasePointB }
            , Cmd.map FromTwoPoints_BasePointBMsg subCmd
            )

        ( FromTwoPoints_TwoPointsPositionMsg subMsg, FromTwoPointsForm stuff ) ->
            case subMsg of
                TwoPointsPosition_TypeChanged twoPointDistanceTag ->
                    let
                        newTwoPointsPosition =
                            case twoPointDistanceTag of
                                TwoPointsPositionRatioTag ->
                                    TwoPointsPositionRatio
                                        { ratio = ""
                                        , ratioHelp = Nothing
                                        }

                                TwoPointsPositionFromATag ->
                                    TwoPointsPositionFromA
                                        { distance = ""
                                        , distanceHelp = Nothing
                                        }

                                TwoPointsPositionFromBTag ->
                                    TwoPointsPositionFromB
                                        { distance = ""
                                        , distanceHelp = Nothing
                                        }
                    in
                    ( FromTwoPointsForm
                        { stuff | twoPointsPosition = newTwoPointsPosition }
                    , Cmd.none
                    )

                TwoPointsPosition_RatioChanged newRatio ->
                    case stuff.twoPointsPosition of
                        TwoPointsPositionRatio stuff_ ->
                            ( FromTwoPointsForm
                                { stuff
                                    | twoPointsPosition =
                                        TwoPointsPositionRatio
                                            { stuff_ | ratio = newRatio }
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( form, Cmd.none )

                TwoPointsPosition_FromAChanged newDistance ->
                    case stuff.twoPointsPosition of
                        TwoPointsPositionFromA stuff_ ->
                            ( FromTwoPointsForm
                                { stuff
                                    | twoPointsPosition =
                                        TwoPointsPositionFromA
                                            { stuff_ | distance = newDistance }
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( form, Cmd.none )

                TwoPointsPosition_FromBChanged newDistance ->
                    case stuff.twoPointsPosition of
                        TwoPointsPositionFromB stuff_ ->
                            ( FromTwoPointsForm
                                { stuff
                                    | twoPointsPosition =
                                        TwoPointsPositionFromB
                                            { stuff_ | distance = newDistance }
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( form, Cmd.none )

        -- INTERSECTION
        ( Intersection_ObjectAMsg subMsg, IntersectionForm stuff ) ->
            let
                ( newObjectA, subCmd ) =
                    updateOtherIntersectableForm pattern objects subMsg stuff.objectA
            in
            ( IntersectionForm { stuff | objectA = newObjectA }
            , Cmd.map Intersection_ObjectAMsg subCmd
            )

        ( Intersection_ObjectBMsg subMsg, IntersectionForm stuff ) ->
            let
                ( newObjectB, subCmd ) =
                    updateOtherIntersectableForm pattern objects subMsg stuff.objectB
            in
            ( IntersectionForm { stuff | objectB = newObjectB }
            , Cmd.map Intersection_ObjectBMsg subCmd
            )

        ( Intersection_WhichChanged newWhich, IntersectionForm stuff ) ->
            ( IntersectionForm { stuff | which = newWhich }
            , Cmd.none
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )


updateAxisForm :
    Pattern
    -> Pattern.Objects
    -> AxisMsg
    -> AxisForm
    -> ( AxisForm, Cmd AxisMsg )
updateAxisForm pattern objects axisMsg form =
    case ( axisMsg, form ) of
        ( AxisTypeChanged axisTag, _ ) ->
            ( if tagFromAxisForm form == axisTag then
                form

              else
                case axisTag of
                    ThroughOnePointTag ->
                        initThroughOnePointForm

                    ThroughTwoPointsTag ->
                        initThroughTwoPointsForm
            , Cmd.none
            )

        -- THROUGH ONE POINT
        ( ThroughOnePoint_PointMsg subMsg, ThroughOnePointForm stuff ) ->
            let
                ( newPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.point
            in
            ( ThroughOnePointForm { stuff | point = newPoint }
            , Cmd.map ThroughOnePoint_PointMsg subCmd
            )

        ( ThroughOnePoint_OrientationMsg subMsg, ThroughOnePointForm stuff ) ->
            ( ThroughOnePointForm
                { stuff | orientation = updateOrientation subMsg stuff.orientation }
            , Cmd.none
            )

        -- THROUGH TWO POINTS
        ( ThroughTwoPoints_PointAMsg subMsg, ThroughTwoPointsForm stuff ) ->
            let
                ( newPointA, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.pointA
            in
            ( ThroughTwoPointsForm { stuff | pointA = newPointA }
            , Cmd.map ThroughTwoPoints_PointAMsg subCmd
            )

        ( ThroughTwoPoints_PointBMsg subMsg, ThroughTwoPointsForm stuff ) ->
            let
                ( newPointB, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.pointB
            in
            ( ThroughTwoPointsForm { stuff | pointB = newPointB }
            , Cmd.map ThroughTwoPoints_PointBMsg subCmd
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )


updateCircleForm :
    Pattern
    -> Pattern.Objects
    -> CircleMsg
    -> CircleForm
    -> ( CircleForm, Cmd CircleMsg )
updateCircleForm pattern objects circleMsg form =
    case ( circleMsg, form ) of
        ( CircleTypeChanged circleTag, _ ) ->
            ( if tagFromCircleForm form == circleTag then
                form

              else
                case circleTag of
                    WithRadiusTag ->
                        initWithRadiusForm

                    ThroughThreePointsTag ->
                        initThroughThreePointsForm
            , Cmd.none
            )

        -- WITH RADIUS
        ( WithRadius_CenterPointMsg subMsg, WithRadiusForm stuff ) ->
            let
                ( newCenterPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.centerPoint
            in
            ( WithRadiusForm { stuff | centerPoint = newCenterPoint }
            , Cmd.map WithRadius_CenterPointMsg subCmd
            )

        ( WithRadius_RadiusChanged newRadius, WithRadiusForm stuff ) ->
            ( WithRadiusForm { stuff | radius = newRadius }
            , Cmd.none
            )

        -- THROUGH THREE POINTS
        ( ThroughThreePoints_PointAMsg subMsg, ThroughThreePointsForm stuff ) ->
            let
                ( newPointA, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.pointA
            in
            ( ThroughThreePointsForm { stuff | pointA = newPointA }
            , Cmd.map ThroughThreePoints_PointAMsg subCmd
            )

        ( ThroughThreePoints_PointBMsg subMsg, ThroughThreePointsForm stuff ) ->
            let
                ( newPointB, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.pointB
            in
            ( ThroughThreePointsForm { stuff | pointB = newPointB }
            , Cmd.map ThroughThreePoints_PointBMsg subCmd
            )

        ( ThroughThreePoints_PointCMsg subMsg, ThroughThreePointsForm stuff ) ->
            let
                ( newPointC, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.pointC
            in
            ( ThroughThreePointsForm { stuff | pointC = newPointC }
            , Cmd.map ThroughThreePoints_PointCMsg subCmd
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )


updateCurveForm :
    Pattern
    -> Pattern.Objects
    -> CurveMsg
    -> CurveForm
    -> ( CurveForm, Cmd CurveMsg )
updateCurveForm pattern objects curveMsg form =
    case ( curveMsg, form ) of
        ( CurveTypeChanged curveTag, _ ) ->
            ( if tagFromCurveForm form == curveTag then
                form

              else
                case curveTag of
                    StraightTag ->
                        initStraightForm

                    QuadraticTag ->
                        initQuadraticForm

                    CubicTag ->
                        initCubicForm
            , Cmd.none
            )

        -- STRAIGHT
        ( StartPointMsg subMsg, StraightForm stuff ) ->
            let
                ( newStartPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.startPoint
            in
            ( StraightForm { stuff | startPoint = newStartPoint }
            , Cmd.map StartPointMsg subCmd
            )

        ( EndPointMsg subMsg, StraightForm stuff ) ->
            let
                ( newEndPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.endPoint
            in
            ( StraightForm { stuff | endPoint = newEndPoint }
            , Cmd.map EndPointMsg subCmd
            )

        -- QUADRATIC
        ( StartPointMsg subMsg, QuadraticForm stuff ) ->
            let
                ( newStartPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.startPoint
            in
            ( QuadraticForm { stuff | startPoint = newStartPoint }
            , Cmd.map StartPointMsg subCmd
            )

        ( ControlPointMsg subMsg, QuadraticForm stuff ) ->
            let
                ( newControlPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.controlPoint
            in
            ( QuadraticForm { stuff | controlPoint = newControlPoint }
            , Cmd.map ControlPointMsg subCmd
            )

        ( EndPointMsg subMsg, QuadraticForm stuff ) ->
            let
                ( newEndPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.endPoint
            in
            ( QuadraticForm { stuff | endPoint = newEndPoint }
            , Cmd.map EndPointMsg subCmd
            )

        -- CUBIC
        ( StartPointMsg subMsg, CubicForm stuff ) ->
            let
                ( newStartPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.startPoint
            in
            ( CubicForm { stuff | startPoint = newStartPoint }
            , Cmd.map StartPointMsg subCmd
            )

        ( StartControlPointMsg subMsg, CubicForm stuff ) ->
            let
                ( newStartControlPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.startControlPoint
            in
            ( CubicForm { stuff | startControlPoint = newStartControlPoint }
            , Cmd.map StartControlPointMsg subCmd
            )

        ( EndControlPointMsg subMsg, CubicForm stuff ) ->
            let
                ( newEndControlPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.endControlPoint
            in
            ( CubicForm { stuff | endControlPoint = newEndControlPoint }
            , Cmd.map EndControlPointMsg subCmd
            )

        ( EndPointMsg subMsg, CubicForm stuff ) ->
            let
                ( newEndPoint, subCmd ) =
                    updateOtherPointForm pattern objects subMsg stuff.endPoint
            in
            ( CubicForm { stuff | endPoint = newEndPoint }
            , Cmd.map EndPointMsg subCmd
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )


updateDetailForm :
    Pattern
    -> Pattern.Objects
    -> DetailMsg
    -> DetailForm
    -> ( DetailForm, Cmd DetailMsg )
updateDetailForm pattern objects detailMsg detail =
    case detailMsg of
        AddCurvePressed ->
            ( { detail
                | nextCurves =
                    detail.nextCurves
                        ++ [ ( initNextStraightForm, Closed ) ]
              }
            , Cmd.none
            )

        -- FIRST CURVE
        FirstCurveTypeChanged firstCurveTag ->
            ( if
                tagFromFirstCurveForm (Tuple.first detail.firstCurve)
                    == firstCurveTag
              then
                detail

              else
                case firstCurveTag of
                    FirstStraightTag ->
                        { detail | firstCurve = ( initFirstStraightForm, Closed ) }

                    FirstQuadraticTag ->
                        { detail | firstCurve = ( initFirstQuadraticForm, Closed ) }

                    FirstCubicTag ->
                        { detail | firstCurve = ( initFirstCubicForm, Closed ) }

                    FirstReferencedCurveTag ->
                        { detail | firstCurve = ( initFirstReferencedCurveForm, Closed ) }
            , Cmd.none
            )

        FirstCurveStartPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.startPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstStraightForm { stuff | startPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartPointMsg subCmd
                    )

                FirstQuadraticForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.startPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstQuadraticForm { stuff | startPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartPointMsg subCmd
                    )

                FirstCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.startPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstCubicForm { stuff | startPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartPointMsg subCmd
                    )

                FirstReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        FirstCurveStartControlPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    ( detail, Cmd.none )

                FirstQuadraticForm stuff ->
                    ( detail, Cmd.none )

                FirstCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstCubicForm { stuff | startControlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartControlPointMsg subCmd
                    )

                FirstReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        FirstCurveControlPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    ( detail, Cmd.none )

                FirstQuadraticForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.controlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstQuadraticForm { stuff | controlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveControlPointMsg subCmd
                    )

                FirstCubicForm stuff ->
                    ( detail, Cmd.none )

                FirstReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        FirstCurveEndControlPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    ( detail, Cmd.none )

                FirstQuadraticForm stuff ->
                    ( detail, Cmd.none )

                FirstCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstCubicForm { stuff | endControlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndControlPointMsg subCmd
                    )

                FirstReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        FirstCurveEndPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstStraightForm { stuff | endPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndPointMsg subCmd
                    )

                FirstQuadraticForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstQuadraticForm { stuff | endPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndPointMsg subCmd
                    )

                FirstCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstCubicForm { stuff | endPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndPointMsg subCmd
                    )

                FirstReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        FirstCurveDropdownMsg dropdownMsg ->
            case Tuple.first detail.firstCurve of
                FirstReferencedCurveForm stuff ->
                    let
                        ( newDropdown, dropdownCmd, newMaybeACurve ) =
                            Ui.Atom.Dropdown.update
                                { entryToHash = Pattern.hash }
                                objects.curves
                                dropdownMsg
                                stuff.curve.dropdown
                                stuff.curve.maybeACurve
                    in
                    ( { detail
                        | firstCurve =
                            ( FirstReferencedCurveForm
                                { stuff
                                    | curve =
                                        { dropdown = newDropdown
                                        , maybeACurve = newMaybeACurve
                                        }
                                }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveDropdownMsg dropdownCmd
                    )

                _ ->
                    ( detail, Cmd.none )

        FirstCurveReverseChanged newValue ->
            case Tuple.first detail.firstCurve of
                FirstReferencedCurveForm stuff ->
                    ( { detail
                        | firstCurve =
                            ( FirstReferencedCurveForm { stuff | reversed = newValue }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( detail, Cmd.none )

        FirstCurveActionMenuMsg actionMenuMsg ->
            ( detail, Cmd.none )

        -- NEXT CURVE
        NextCurveTypeChanged index nextCurveTag ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( nextCurve, actionMenu ) ->
                    ( if tagFromNextCurveForm nextCurve == nextCurveTag then
                        detail

                      else
                        case nextCurveTag of
                            NextStraightTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextStraightForm, Closed ))
                                            detail.nextCurves
                                }

                            NextQuadraticTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextQuadraticForm, Closed ))
                                            detail.nextCurves
                                }

                            NextCubicTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextCubicForm, Closed ))
                                            detail.nextCurves
                                }

                            NextReferencedCurveTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextReferencedCurveForm, Closed ))
                                            detail.nextCurves
                                }
                    , Cmd.none
                    )

        NextCurveStartControlPointMsg index otherPointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NextStraightForm _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextQuadraticForm stuff, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextCubicForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern
                                objects
                                otherPointMsg
                                stuff.startControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextCubicForm
                                        { stuff | startControlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveStartControlPointMsg index) subCmd
                    )

                Just ( NextReferencedCurveForm _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveControlPointMsg index otherPointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NextStraightForm _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextQuadraticForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.controlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextQuadraticForm
                                        { stuff | controlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveControlPointMsg index) subCmd
                    )

                Just ( NextCubicForm _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextReferencedCurveForm _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveEndControlPointMsg index otherPointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NextStraightForm _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextQuadraticForm _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NextCubicForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextCubicForm
                                        { stuff | endControlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndControlPointMsg index) subCmd
                    )

                Just ( NextReferencedCurveForm _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveEndPointMsg index otherPointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NextStraightForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextStraightForm
                                        { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NextQuadraticForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextQuadraticForm
                                        { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NextCubicForm stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextCubicForm { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NextReferencedCurveForm _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveDropdownMsg index dropdownMsg ->
            case List.getAt index detail.nextCurves of
                Just ( NextReferencedCurveForm stuff, actionMenu ) ->
                    let
                        ( newDropdown, dropdownCmd, newMaybeACurve ) =
                            Ui.Atom.Dropdown.update
                                { entryToHash = Pattern.hash }
                                objects.curves
                                dropdownMsg
                                stuff.curve.dropdown
                                stuff.curve.maybeACurve
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextReferencedCurveForm
                                        { stuff
                                            | curve =
                                                { dropdown = newDropdown
                                                , maybeACurve = newMaybeACurve
                                                }
                                        }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveDropdownMsg index) dropdownCmd
                    )

                _ ->
                    ( detail, Cmd.none )

        NextCurveReverseChanged index newValue ->
            case List.getAt index detail.nextCurves of
                Just ( NextReferencedCurveForm stuff, actionMenu ) ->
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextReferencedCurveForm { stuff | reversed = newValue }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.none
                    )

                _ ->
                    ( detail, Cmd.none )

        NextCurveActionMenuMsg index actionMenuMsg ->
            ( detail, Cmd.none )

        -- LAST CURVE
        LastCurveTypeChanged lastCurveTag ->
            ( if
                tagFromLastCurveForm (Tuple.first detail.lastCurve)
                    == lastCurveTag
              then
                detail

              else
                case lastCurveTag of
                    LastStraightTag ->
                        { detail | lastCurve = ( initLastStraightForm, Closed ) }

                    LastQuadraticTag ->
                        { detail | lastCurve = ( initLastQuadraticForm, Closed ) }

                    LastCubicTag ->
                        { detail | lastCurve = ( initLastCubicForm, Closed ) }

                    LastReferencedCurveTag ->
                        { detail | lastCurve = ( initLastReferencedCurveForm, Closed ) }
            , Cmd.none
            )

        LastCurveStartControlPointMsg otherPointMsg ->
            case Tuple.first detail.lastCurve of
                LastStraightForm ->
                    ( detail, Cmd.none )

                LastQuadraticForm stuff ->
                    ( detail, Cmd.none )

                LastCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LastCubicForm { stuff | startControlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveStartControlPointMsg subCmd
                    )

                LastReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        LastCurveControlPointMsg otherPointMsg ->
            case Tuple.first detail.lastCurve of
                LastStraightForm ->
                    ( detail, Cmd.none )

                LastQuadraticForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.controlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LastQuadraticForm { stuff | controlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveControlPointMsg subCmd
                    )

                LastCubicForm stuff ->
                    ( detail, Cmd.none )

                LastReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        LastCurveEndControlPointMsg otherPointMsg ->
            case Tuple.first detail.lastCurve of
                LastStraightForm ->
                    ( detail, Cmd.none )

                LastQuadraticForm stuff ->
                    ( detail, Cmd.none )

                LastCubicForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            updateOtherPointForm pattern objects otherPointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LastCubicForm { stuff | endControlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveEndControlPointMsg subCmd
                    )

                LastReferencedCurveForm stuff ->
                    ( detail, Cmd.none )

        LastCurveDropdownMsg dropdownMsg ->
            case Tuple.first detail.lastCurve of
                LastReferencedCurveForm stuff ->
                    let
                        ( newDropdown, dropdownCmd, newMaybeACurve ) =
                            Ui.Atom.Dropdown.update
                                { entryToHash = Pattern.hash }
                                objects.curves
                                dropdownMsg
                                stuff.curve.dropdown
                                stuff.curve.maybeACurve
                    in
                    ( { detail
                        | lastCurve =
                            ( LastReferencedCurveForm
                                { stuff
                                    | curve =
                                        { dropdown = newDropdown
                                        , maybeACurve = newMaybeACurve
                                        }
                                }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map LastCurveDropdownMsg dropdownCmd
                    )

                _ ->
                    ( detail, Cmd.none )

        LastCurveReverseChanged newValue ->
            case Tuple.first detail.lastCurve of
                LastReferencedCurveForm stuff ->
                    ( { detail
                        | lastCurve =
                            ( LastReferencedCurveForm { stuff | reversed = newValue }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( detail, Cmd.none )

        LastCurveActionMenuMsg actionMenuMsg ->
            ( detail, Cmd.none )


updateOtherPointForm :
    Pattern
    -> Pattern.Objects
    -> OtherPointMsg
    -> OtherPointForm
    -> ( OtherPointForm, Cmd OtherPointMsg )
updateOtherPointForm pattern objects msg form =
    case msg of
        OtherPointTypeChanged otherPointTag ->
            case otherPointTag of
                ReferencedPointTag ->
                    ( initReferencedPointForm
                    , Cmd.none
                    )

                InlinedPointTag ->
                    ( initInlinedPointForm
                    , Cmd.none
                    )

        ReferencedPointDropdownMsg dropdownMsg ->
            case form of
                ReferencedPointForm stuff ->
                    let
                        ( newDropdown, dropdownCmd, newMaybeAPoint ) =
                            Ui.Atom.Dropdown.update
                                { entryToHash = Pattern.hash }
                                objects.points
                                dropdownMsg
                                stuff.dropdown
                                stuff.maybeAPoint
                    in
                    ( ReferencedPointForm
                        { stuff
                            | dropdown = newDropdown
                            , maybeAPoint = newMaybeAPoint
                        }
                    , Cmd.map ReferencedPointDropdownMsg dropdownCmd
                    )

                _ ->
                    ( form, Cmd.none )

        InlinedPointMsg subMsg ->
            case form of
                InlinedPointForm stuff ->
                    let
                        ( newPoint, subCmd ) =
                            updatePointForm pattern objects subMsg stuff.point
                    in
                    ( InlinedPointForm { stuff | point = newPoint }
                    , Cmd.map InlinedPointMsg subCmd
                    )

                _ ->
                    ( form, Cmd.none )

        InlinedPointExpandToggled ->
            case form of
                InlinedPointForm stuff ->
                    ( InlinedPointForm { stuff | expanded = not stuff.expanded }
                    , Cmd.none
                    )

                _ ->
                    ( form, Cmd.none )


updateOtherIntersectableForm :
    Pattern
    -> Pattern.Objects
    -> OtherIntersectableMsg
    -> OtherIntersectableForm
    -> ( OtherIntersectableForm, Cmd OtherIntersectableMsg )
updateOtherIntersectableForm pattern objects msg form =
    case ( msg, form ) of
        ( OtherIntersectableTypeChanged intersectableTag, _ ) ->
            if tagFromOtherIntersectableForm form == intersectableTag then
                ( form, Cmd.none )

            else
                ( case intersectableTag of
                    ReferencedIntersectableTag ->
                        initReferencedIntersectableForm

                    InlinedAxisTag ->
                        initInlinedAxisForm

                    InlinedCircleTag ->
                        initInlinedCircleForm

                    InlinedCurveTag ->
                        initInlinedCurveForm
                , Cmd.none
                )

        ( ReferencedIntersectableDropdownMsg dropdownMsg, ReferencedIntersectableForm stuff ) ->
            let
                ( newDropdown, dropdownCmd, newMaybeAIntersectable ) =
                    Ui.Atom.Dropdown.update
                        { entryToHash = Pattern.hash }
                        (List.concat
                            [ List.map Pattern.intersectableAxis objects.axes
                            , List.map Pattern.intersectableCircle objects.circles
                            , List.map Pattern.intersectableCurve objects.curves
                            ]
                        )
                        dropdownMsg
                        stuff.dropdown
                        stuff.maybeAIntersectable
            in
            ( ReferencedIntersectableForm
                { stuff
                    | dropdown = newDropdown
                    , maybeAIntersectable = newMaybeAIntersectable
                }
            , Cmd.map ReferencedIntersectableDropdownMsg dropdownCmd
            )

        ( InlinedAxisMsg axisMsg, InlinedAxisForm stuff ) ->
            let
                ( newAxis, axisCmd ) =
                    updateAxisForm pattern objects axisMsg stuff.axis
            in
            ( InlinedAxisForm { stuff | axis = newAxis }
            , Cmd.map InlinedAxisMsg axisCmd
            )

        ( InlinedCircleMsg circleMsg, InlinedCircleForm stuff ) ->
            let
                ( newCircle, circleCmd ) =
                    updateCircleForm pattern objects circleMsg stuff.circle
            in
            ( InlinedCircleForm { stuff | circle = newCircle }
            , Cmd.map InlinedCircleMsg circleCmd
            )

        ( InlinedCurveMsg curveMsg, InlinedCurveForm stuff ) ->
            let
                ( newCurve, curveCmd ) =
                    updateCurveForm pattern objects curveMsg stuff.curve
            in
            ( InlinedCurveForm { stuff | curve = newCurve }
            , Cmd.map InlinedCurveMsg curveCmd
            )

        ( InlinedIntersectableExpandToggled, InlinedAxisForm stuff ) ->
            ( InlinedAxisForm { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        ( InlinedIntersectableExpandToggled, InlinedCircleForm stuff ) ->
            ( InlinedCircleForm { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        ( InlinedIntersectableExpandToggled, InlinedCurveForm stuff ) ->
            ( InlinedCurveForm { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )


updateDirection : DirectionMsg -> Direction -> Direction
updateDirection directionMsg direction =
    case directionMsg of
        DirectionTypeChanged directionTag ->
            case directionTag of
                LeftwardTag ->
                    Leftward

                RightwardTag ->
                    Rightward

                UpTag ->
                    Up

                DownTag ->
                    Down

                CustomTag ->
                    DirectionAngle ""

        CustomChanged newCustom ->
            case direction of
                DirectionAngle _ ->
                    DirectionAngle newCustom

                _ ->
                    direction


updateOrientation : OrientationMsg -> Orientation -> Orientation
updateOrientation orientationMsg orientation =
    case orientationMsg of
        OrientationTypeChanged orientationTag ->
            if tagFromOrientation orientation == orientationTag then
                orientation

            else
                case orientationTag of
                    HorizontalTag ->
                        Horizontal

                    VerticalTag ->
                        Vertical

                    CustomOrientationTag ->
                        OrientationAngle ""

        CustomOrientationChanged newCustom ->
            case orientation of
                OrientationAngle _ ->
                    OrientationAngle newCustom

                _ ->
                    orientation



---- NEW


newPointFrom : PointForm -> Pattern -> Result PointForm Point
newPointFrom form pattern =
    case form of
        FromOnePointForm stuff ->
            let
                getBasePoint =
                    newOtherPointFrom stuff.basePoint pattern
                        |> Result.mapError
                            (\basePointWithHelp ->
                                { stuff
                                    | basePoint = basePointWithHelp
                                    , directionHelp = checkDirection pattern stuff.direction
                                    , distanceHelp = checkExpr pattern stuff.distance
                                }
                            )
                        |> Result.andThen createNewPoint

                createNewPoint aPoint =
                    Pattern.fromOnePoint aPoint stuff.direction stuff.distance pattern
                        |> Result.mapError addHelp

                addHelp help =
                    { stuff
                        | directionHelp = Maybe.map printComputeHelp help.computeDirection
                        , distanceHelp = Maybe.map printComputeHelp help.computeDistance
                    }
            in
            getBasePoint
                |> Result.mapError FromOnePointForm

        FromTwoPointsForm stuff ->
            let
                getBasePointA =
                    newOtherPointFrom stuff.basePointA pattern
                        |> Result.mapError
                            (\basePointAWithHelp ->
                                { stuff
                                    | basePointA = basePointAWithHelp
                                    , basePointB =
                                        checkOtherPoint pattern stuff.basePointB
                                    , twoPointsPosition =
                                        checkTwoPointsPosition pattern stuff.twoPointsPosition
                                }
                            )
                        |> Result.andThen getBasePointB

                getBasePointB aPointA =
                    newOtherPointFrom stuff.basePointB pattern
                        |> Result.mapError
                            (\basePointBWithHelp ->
                                { stuff
                                    | basePointB = basePointBWithHelp
                                    , twoPointsPosition =
                                        checkTwoPointsPosition pattern stuff.twoPointsPosition
                                }
                            )
                        |> Result.andThen (createNewPoint aPointA)

                createNewPoint aPointA aPointB =
                    case stuff.twoPointsPosition of
                        TwoPointsPositionRatio stuff_ ->
                            Pattern.betweenRatio aPointA aPointB stuff_.ratio pattern
                                |> Result.mapError addRatioHelp

                        TwoPointsPositionFromA stuff_ ->
                            Pattern.betweenLength aPointA aPointB stuff_.distance FirstInTwo pattern
                                |> Result.mapError addFromAHelp

                        TwoPointsPositionFromB stuff_ ->
                            Pattern.betweenLength aPointA aPointB stuff_.distance SecondInTwo pattern
                                |> Result.mapError addFromBHelp

                addRatioHelp help =
                    stuff
                        |> addTwoPointsPositionRatioHelp help
                        |> addBasePointsCoincideHelp help

                addFromAHelp help =
                    stuff
                        |> addTwoPointsPositionFromAHelp help
                        |> addBasePointsCoincideHelp help

                addFromBHelp help =
                    stuff
                        |> addTwoPointsPositionFromBHelp help
                        |> addBasePointsCoincideHelp help
            in
            getBasePointA
                |> Result.mapError FromTwoPointsForm

        IntersectionForm stuff ->
            let
                getObjectA =
                    newOtherIntersectableFrom stuff.objectA pattern
                        |> Result.mapError
                            (\objectAWithHelp ->
                                { stuff
                                    | objectA = objectAWithHelp
                                    , objectB =
                                        checkOtherIntersectableObject pattern
                                            stuff.objectB
                                }
                            )
                        |> Result.andThen getObjectB

                getObjectB aIntersectableA =
                    newOtherIntersectableFrom stuff.objectB pattern
                        |> Result.mapError
                            (\objectBWithHelp ->
                                { stuff | objectB = objectBWithHelp }
                            )
                        |> Result.andThen (createNewPoint aIntersectableA)

                createNewPoint aIntersectableA aIntersectableB =
                    Pattern.intersection
                        aIntersectableA
                        aIntersectableB
                        stuff.which
                        pattern
                        |> Result.mapError
                            (\intersectionHelp ->
                                { stuff
                                    | whichHelp =
                                        if intersectionHelp.whichOutOfBound then
                                            Just "Make a different choice."

                                        else
                                            Nothing
                                }
                            )
            in
            getObjectA
                |> Result.mapError IntersectionForm


newAxisFrom : AxisForm -> Pattern -> Result AxisForm Axis
newAxisFrom form pattern =
    case form of
        ThroughOnePointForm stuff ->
            let
                getPoint =
                    newOtherPointFrom stuff.point pattern
                        |> Result.mapError
                            (\pointWithHelp ->
                                { stuff
                                    | point = pointWithHelp
                                    , orientationHelp = checkOrientation pattern stuff.orientation
                                }
                            )
                        |> Result.andThen createNewAxis

                createNewAxis aPoint =
                    Pattern.throughOnePoint aPoint stuff.orientation pattern
                        |> Result.mapError addHelp

                addHelp help =
                    { stuff | orientationHelp = Maybe.map printComputeHelp help.computeAngle }
            in
            getPoint
                |> Result.mapError ThroughOnePointForm

        ThroughTwoPointsForm stuff ->
            let
                getPointA =
                    newOtherPointFrom stuff.pointA pattern
                        |> Result.mapError
                            (\pointAWithHelp ->
                                { stuff
                                    | pointA = pointAWithHelp
                                    , pointB = checkOtherPoint pattern stuff.pointB
                                }
                            )
                        |> Result.andThen getPointB

                getPointB aPointA =
                    newOtherPointFrom stuff.pointB pattern
                        |> Result.mapError
                            (\pointBWithHelp ->
                                { stuff | pointB = pointBWithHelp }
                            )
                        |> Result.andThen (createNewAxis aPointA)

                createNewAxis aPointA aPointB =
                    Pattern.throughTwoPoints aPointA aPointB pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addBasePointsCoincideHelp help
            in
            getPointA
                |> Result.mapError ThroughTwoPointsForm


newCircleFrom : CircleForm -> Pattern -> Result CircleForm Circle
newCircleFrom form pattern =
    case form of
        WithRadiusForm stuff ->
            let
                getCenterPoint =
                    newOtherPointFrom stuff.centerPoint pattern
                        |> Result.mapError
                            (\centerPointWithHelp ->
                                { stuff
                                    | centerPoint = centerPointWithHelp
                                    , radiusHelp = checkExpr pattern stuff.radius
                                }
                            )
                        |> Result.andThen createNewCircle

                createNewCircle aCenterPoint =
                    Pattern.withRadius stuff.radius aCenterPoint pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addRadiusHelp help
            in
            getCenterPoint
                |> Result.mapError WithRadiusForm

        ThroughThreePointsForm stuff ->
            let
                getPointA =
                    newOtherPointFrom stuff.pointA pattern
                        |> Result.mapError
                            (\pointAWithHelp ->
                                { stuff
                                    | pointA = pointAWithHelp
                                    , pointB = checkOtherPoint pattern stuff.pointB
                                    , pointC = checkOtherPoint pattern stuff.pointC
                                }
                            )
                        |> Result.andThen getPointB

                getPointB aPointA =
                    newOtherPointFrom stuff.pointB pattern
                        |> Result.mapError
                            (\pointBWithHelp ->
                                { stuff
                                    | pointB = pointBWithHelp
                                    , pointC = checkOtherPoint pattern stuff.pointC
                                }
                            )
                        |> Result.andThen (getPointC aPointA)

                getPointC aPointA aPointB =
                    newOtherPointFrom stuff.pointC pattern
                        |> Result.mapError
                            (\pointCWithHelp ->
                                { stuff | pointC = pointCWithHelp }
                            )
                        |> Result.andThen (createNewCircle aPointA aPointB)

                createNewCircle aPointA aPointB aPointC =
                    Pattern.throughThreePoints aPointA aPointB aPointC pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addPointsCoincideHelp help
            in
            getPointA
                |> Result.mapError ThroughThreePointsForm


newCurveFrom : CurveForm -> Pattern -> Result CurveForm Curve
newCurveFrom form pattern =
    case form of
        StraightForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint aStartPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff
                                    | endPoint = endPointWithHelp
                                }
                            )
                        |> Result.andThen (createNewCurve aStartPoint)

                createNewCurve aStartPoint aEndPoint =
                    Pattern.straight aStartPoint aEndPoint pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addPointsCoincideHelp help
            in
            Result.mapError StraightForm getStartPoint

        QuadraticForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , controlPoint =
                                        checkOtherPoint pattern stuff.controlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getControlPoint

                getControlPoint aStartPoint =
                    newOtherPointFrom stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint aStartPoint)

                getEndPoint aStartPoint aControlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff
                                    | endPoint = endPointWithHelp
                                }
                            )
                        |> Result.andThen
                            (createNewCurve aStartPoint aControlPoint)

                createNewCurve aStartPoint aControlPoint aEndPoint =
                    Pattern.quadratic aStartPoint aControlPoint aEndPoint pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addPointsCoincideHelp help
            in
            Result.mapError QuadraticForm getStartPoint

        CubicForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , startControlPoint =
                                        checkOtherPoint pattern stuff.startControlPoint
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getStartControlPoint

                getStartControlPoint aStartPoint =
                    newOtherPointFrom stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndControlPoint aStartPoint)

                getEndControlPoint aStartPoint aStartControlPoint =
                    newOtherPointFrom stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen
                            (getEndPoint aStartPoint aStartControlPoint)

                getEndPoint aStartPoint aStartControlPoint aEndControlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff
                                    | endPoint = endPointWithHelp
                                }
                            )
                        |> Result.andThen
                            (createNewCurve
                                aStartPoint
                                aStartControlPoint
                                aEndControlPoint
                            )

                createNewCurve aStartPoint aStartControlPoint aEndControlPoint aEndPoint =
                    Pattern.cubic
                        aStartPoint
                        aStartControlPoint
                        aEndControlPoint
                        aEndPoint
                        pattern
                        |> Result.mapError addHelp

                addHelp help =
                    stuff
                        |> addPointsCoincideHelp help
            in
            Result.mapError CubicForm getStartPoint


newDetailFrom : DetailForm -> Pattern -> Result DetailForm Detail
newDetailFrom form pattern =
    let
        getFirstCurve =
            newFirstCurveFrom (Tuple.first form.firstCurve) pattern
                |> Result.mapError
                    (\firstCurveWithHelp ->
                        { form
                            | firstCurve =
                                ( firstCurveWithHelp
                                , Tuple.second form.firstCurve
                                )
                            , nextCurves =
                                List.map (Tuple.mapFirst (checkNextCurve pattern))
                                    form.nextCurves
                            , lastCurve =
                                ( checkLastCurve pattern (Tuple.first form.lastCurve)
                                , Tuple.second form.lastCurve
                                )
                        }
                    )
                |> Result.andThen getNextCurves

        getNextCurves firstCurve =
            form.nextCurves
                |> List.foldl getNextCurve (Ok ( [], [] ))
                |> Result.map (Tuple.first >> List.reverse)
                |> Result.mapError
                    (\nextCurvesWithHelp ->
                        { form
                            | nextCurves = nextCurvesWithHelp
                            , lastCurve =
                                ( checkLastCurve pattern (Tuple.first form.lastCurve)
                                , Tuple.second form.lastCurve
                                )
                        }
                    )
                |> Result.andThen (getLastCurve firstCurve)

        getNextCurve ( nextCurveForm, actionMenu ) result =
            case result of
                Ok ( nextCurves, nextCurveForms ) ->
                    case newNextCurveFrom nextCurveForm pattern of
                        Err newCurveWithHelp ->
                            Err <|
                                ( newCurveWithHelp, actionMenu )
                                    :: nextCurveForms

                        Ok nextCurve ->
                            Ok
                                ( nextCurve :: nextCurves
                                , ( nextCurveForm, actionMenu ) :: nextCurveForms
                                )

                Err nextCurveForms ->
                    Err <|
                        ( checkNextCurve pattern nextCurveForm, actionMenu )
                            :: nextCurveForms

        getLastCurve firstCurve nextCurves =
            newLastCurveFrom (Tuple.first form.lastCurve) pattern
                |> Result.mapError
                    (\lastCurveWithHelp ->
                        { form
                            | lastCurve =
                                ( lastCurveWithHelp
                                , Tuple.second form.lastCurve
                                )
                        }
                    )
                |> Result.andThen (toDetail firstCurve nextCurves)

        toDetail firstCurve nextCurves lastCurve =
            Pattern.detail firstCurve nextCurves lastCurve pattern
                |> Result.mapError (\detailHelp -> form)
    in
    getFirstCurve


newFirstCurveFrom : FirstCurveForm -> Pattern -> Result FirstCurveForm FirstCurve
newFirstCurveFrom form pattern =
    case form of
        FirstStraightForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint startPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map (toCurve startPoint)

                toCurve startPoint endPoint =
                    FirstStraight
                        { startPoint = startPoint
                        , endPoint = endPoint
                        }
            in
            Result.mapError FirstStraightForm getStartPoint

        FirstQuadraticForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , controlPoint =
                                        checkOtherPoint pattern stuff.controlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getControlPoint

                getControlPoint startPoint =
                    newOtherPointFrom stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint)

                getEndPoint startPoint controlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map (toCurve startPoint controlPoint)

                toCurve startPoint controlPoint endPoint =
                    FirstQuadratic
                        { startPoint = startPoint
                        , controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            Result.mapError FirstQuadraticForm getStartPoint

        FirstCubicForm stuff ->
            let
                getStartPoint =
                    newOtherPointFrom stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , startControlPoint =
                                        checkOtherPoint pattern stuff.startControlPoint
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getStartControlPoint

                getStartControlPoint startPoint =
                    newOtherPointFrom stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndControlPoint startPoint)

                getEndControlPoint startPoint startControlPoint =
                    newOtherPointFrom stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint startControlPoint)

                getEndPoint startPoint startControlPoint endControlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map
                            (toCurve startPoint startControlPoint endControlPoint)

                toCurve startPoint startControlPoint endControlPoint endPoint =
                    FirstCubic
                        { startPoint = startPoint
                        , startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            Result.mapError FirstCubicForm getStartPoint

        FirstReferencedCurveForm stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        FirstReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


newNextCurveFrom : NextCurveForm -> Pattern -> Result NextCurveForm NextCurve
newNextCurveFrom form pattern =
    case form of
        NextStraightForm stuff ->
            let
                getEndPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map toCurve

                toCurve endPoint =
                    NextStraight { endPoint = endPoint }
            in
            Result.mapError NextStraightForm getEndPoint

        NextQuadraticForm stuff ->
            let
                getControlPoint =
                    newOtherPointFrom stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint controlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map (toCurve controlPoint)

                toCurve controlPoint endPoint =
                    NextQuadratic
                        { controlPoint = controlPoint
                        , endPoint = endPoint
                        }
            in
            Result.mapError NextQuadraticForm getControlPoint

        NextCubicForm stuff ->
            let
                getStartControlPoint =
                    newOtherPointFrom stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newOtherPointFrom stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkOtherPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startControlPoint)

                getEndPoint startControlPoint endControlPoint =
                    newOtherPointFrom stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map
                            (toCurve startControlPoint endControlPoint)

                toCurve startControlPoint endControlPoint endPoint =
                    NextCubic
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        , endPoint = endPoint
                        }
            in
            Result.mapError NextCubicForm getStartControlPoint

        NextReferencedCurveForm stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        NextReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


checkNextCurve : Pattern -> NextCurveForm -> NextCurveForm
checkNextCurve pattern form =
    case form of
        NextStraightForm stuff ->
            NextStraightForm
                { stuff | endPoint = checkOtherPoint pattern stuff.endPoint }

        NextQuadraticForm stuff ->
            NextQuadraticForm
                { stuff
                    | controlPoint = checkOtherPoint pattern stuff.controlPoint
                    , endPoint = checkOtherPoint pattern stuff.endPoint
                }

        NextCubicForm stuff ->
            NextCubicForm
                { stuff
                    | startControlPoint = checkOtherPoint pattern stuff.startControlPoint
                    , endControlPoint = checkOtherPoint pattern stuff.endControlPoint
                    , endPoint = checkOtherPoint pattern stuff.endPoint
                }

        NextReferencedCurveForm stuff ->
            form


newLastCurveFrom : LastCurveForm -> Pattern -> Result LastCurveForm LastCurve
newLastCurveFrom form pattern =
    case form of
        LastStraightForm ->
            Ok LastStraight

        LastQuadraticForm stuff ->
            let
                getControlPoint =
                    newOtherPointFrom stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff | controlPoint = controlPointWithHelp }
                            )
                        |> Result.map toCurve

                toCurve controlPoint =
                    LastQuadratic { controlPoint = controlPoint }
            in
            Result.mapError LastQuadraticForm getControlPoint

        LastCubicForm stuff ->
            let
                getStartControlPoint =
                    newOtherPointFrom stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint =
                                        checkOtherPoint pattern stuff.endControlPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newOtherPointFrom stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff | endControlPoint = endControlPointWithHelp }
                            )
                        |> Result.map (toCurve startControlPoint)

                toCurve startControlPoint endControlPoint =
                    LastCubic
                        { startControlPoint = startControlPoint
                        , endControlPoint = endControlPoint
                        }
            in
            Result.mapError LastCubicForm getStartControlPoint

        LastReferencedCurveForm stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        LastReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


checkLastCurve : Pattern -> LastCurveForm -> LastCurveForm
checkLastCurve pattern form =
    case form of
        LastStraightForm ->
            LastStraightForm

        LastQuadraticForm stuff ->
            LastQuadraticForm
                { stuff | controlPoint = checkOtherPoint pattern stuff.controlPoint }

        LastCubicForm stuff ->
            LastCubicForm
                { stuff
                    | startControlPoint = checkOtherPoint pattern stuff.startControlPoint
                    , endControlPoint = checkOtherPoint pattern stuff.endControlPoint
                }

        LastReferencedCurveForm stuff ->
            form


newOtherPointFrom : OtherPointForm -> Pattern -> Result OtherPointForm (A Point)
newOtherPointFrom form pattern =
    case form of
        ReferencedPointForm stuff ->
            case stuff.maybeAPoint of
                Nothing ->
                    Err (ReferencedPointForm { stuff | help = Just "Pick a point" })

                Just aPoint ->
                    Ok aPoint

        InlinedPointForm stuff ->
            newPointFrom stuff.point pattern
                |> Result.mapError
                    (\pointFormWithHelp ->
                        InlinedPointForm { stuff | point = pointFormWithHelp }
                    )
                |> Result.map Pattern.this


newOtherIntersectableFrom :
    OtherIntersectableForm
    -> Pattern
    -> Result OtherIntersectableForm (A Intersectable)
newOtherIntersectableFrom form pattern =
    case form of
        ReferencedIntersectableForm stuff ->
            case stuff.maybeAIntersectable of
                Nothing ->
                    Err <|
                        ReferencedIntersectableForm
                            { stuff | help = Just "Pick an object" }

                Just aIntersectable ->
                    Ok aIntersectable

        InlinedAxisForm stuff ->
            newAxisFrom stuff.axis pattern
                |> Result.mapError
                    (\axisFormWithHelp ->
                        InlinedAxisForm { stuff | axis = axisFormWithHelp }
                    )
                |> Result.map (Pattern.this >> Pattern.intersectableAxis)

        InlinedCircleForm stuff ->
            newCircleFrom stuff.circle pattern
                |> Result.mapError
                    (\circleFormWithHelp ->
                        InlinedCircleForm { stuff | circle = circleFormWithHelp }
                    )
                |> Result.map (Pattern.this >> Pattern.intersectableCircle)

        InlinedCurveForm stuff ->
            newCurveFrom stuff.curve pattern
                |> Result.mapError
                    (\curveFormWithHelp ->
                        InlinedCurveForm { stuff | curve = curveFormWithHelp }
                    )
                |> Result.map (Pattern.this >> Pattern.intersectableCurve)



-- ADDING HELP


checkOtherPoint : Pattern -> OtherPointForm -> OtherPointForm
checkOtherPoint pattern otherPoint =
    case newOtherPointFrom otherPoint pattern of
        Err otherPointWithHelp ->
            otherPointWithHelp

        Ok _ ->
            otherPoint


checkOtherIntersectableObject : Pattern -> OtherIntersectableForm -> OtherIntersectableForm
checkOtherIntersectableObject pattern otherIntersectable =
    case newOtherIntersectableFrom otherIntersectable pattern of
        Err otherIntersectableWithHelp ->
            otherIntersectableWithHelp

        Ok _ ->
            otherIntersectable


checkTwoPointsPosition : Pattern -> TwoPointsPosition -> TwoPointsPosition
checkTwoPointsPosition pattern twoPointsPosition =
    case twoPointsPosition of
        TwoPointsPositionRatio stuff_ ->
            case Pattern.checkExpr pattern stuff_.ratio of
                Nothing ->
                    twoPointsPosition

                Just help ->
                    TwoPointsPositionRatio
                        { stuff_ | ratioHelp = Just (printComputeHelp help) }

        TwoPointsPositionFromA stuff_ ->
            case Pattern.checkExpr pattern stuff_.distance of
                Nothing ->
                    twoPointsPosition

                Just help ->
                    TwoPointsPositionFromA
                        { stuff_ | distanceHelp = Just (printComputeHelp help) }

        TwoPointsPositionFromB stuff_ ->
            case Pattern.checkExpr pattern stuff_.distance of
                Nothing ->
                    twoPointsPosition

                Just help ->
                    TwoPointsPositionFromB
                        { stuff_ | distanceHelp = Just (printComputeHelp help) }


checkDirection : Pattern -> Direction -> Maybe String
checkDirection pattern direction =
    case direction of
        DirectionAngle angle ->
            Pattern.checkExpr pattern angle
                |> Maybe.map printComputeHelp

        _ ->
            Nothing


checkExpr : Pattern -> String -> Maybe String
checkExpr pattern =
    Pattern.checkExpr pattern
        >> Maybe.map printComputeHelp


checkOrientation : Pattern -> Orientation -> Maybe String
checkOrientation pattern orientation =
    case orientation of
        Horizontal ->
            Nothing

        Vertical ->
            Nothing

        OrientationAngle angle ->
            Pattern.checkExpr pattern angle
                |> Maybe.map printComputeHelp


printComputeHelp : ComputeHelp -> String
printComputeHelp computeHelp =
    case computeHelp of
        MissingObject name ->
            "The object " ++ name ++ " does not exist."

        MissingVariable name ->
            "The variable " ++ name ++ " does not exist."

        PointsCoincide ->
            "Two points coincide."

        PointsAreColinear ->
            "Some points are colinear."

        AxesAreParallel ->
            "Two axes are parallel."

        CirclesDoNotIntersect ->
            "Two circles do not intersect."

        AxisAndCircleDoNotIntersect ->
            "An axis and a circle do not intersect."

        WhichMustBeBetween from to ->
            "The intersection does not exist."

        ExprHelp exprHelp ->
            printExprHelp exprHelp

        DisconnectedCurves ->
            "Two curves are disconnected."

        NotComputableYet ->
            "This is not computable, yet."


printExprHelp : ExprHelp -> String
printExprHelp exprHelp =
    case exprHelp of
        SyntaxHelp deadEnds ->
            "There is a syntactical error."

        UnknownFunction function ->
            "I do not know the function ‘" ++ function ++ "’."

        WrongArguments { function, args } ->
            "The function ‘" ++ function ++ "’ cannot handle one of its arguments."

        CannotComputeFunction function ->
            "I could not compute the value of the function ‘" ++ function ++ "’."


addTwoPointsPositionRatioHelp help stuff =
    case stuff.twoPointsPosition of
        TwoPointsPositionRatio stuff_ ->
            case help.computeRatio of
                Nothing ->
                    stuff

                Just computeHelp ->
                    { stuff
                        | twoPointsPosition =
                            TwoPointsPositionRatio
                                { stuff_ | ratioHelp = Just (printComputeHelp computeHelp) }
                    }

        _ ->
            stuff


addTwoPointsPositionFromAHelp help stuff =
    case stuff.twoPointsPosition of
        TwoPointsPositionFromA stuff_ ->
            case help.computeDistance of
                Nothing ->
                    stuff

                Just computeHelp ->
                    { stuff
                        | twoPointsPosition =
                            TwoPointsPositionFromA
                                { stuff_ | distanceHelp = Just (printComputeHelp computeHelp) }
                    }

        _ ->
            stuff


addTwoPointsPositionFromBHelp help stuff =
    case stuff.twoPointsPosition of
        TwoPointsPositionFromB stuff_ ->
            case help.computeDistance of
                Nothing ->
                    stuff

                Just computeHelp ->
                    { stuff
                        | twoPointsPosition =
                            TwoPointsPositionFromB
                                { stuff_ | distanceHelp = Just (printComputeHelp computeHelp) }
                    }

        _ ->
            stuff


addBasePointsCoincideHelp help stuff =
    if help.basePointsCoincide then
        { stuff | pointsHelp = Just "You must pick two different points" }

    else
        stuff


addPointsCoincideHelp help stuff =
    if help.pointsCoincide then
        { stuff | pointsHelp = Just "These points must not lie on an axis" }

    else
        stuff


addRadiusHelp help stuff =
    case help.computeRadius of
        Nothing ->
            stuff

        Just computeHelp ->
            { stuff | radiusHelp = Just (printComputeHelp computeHelp) }



--


clearHelp : Dialog -> Dialog
clearHelp dialog =
    case dialog of
        DialogPoint form ->
            DialogPoint (clearPointForm form)

        DialogAxis form ->
            DialogAxis (clearAxisForm form)

        DialogCircle form ->
            DialogCircle (clearCircleForm form)

        DialogCurve form ->
            DialogCurve (clearCurveForm form)

        DialogDetail form ->
            DialogDetail (clearDetailForm form)


clearPointForm : PointForm -> PointForm
clearPointForm form =
    case form of
        FromOnePointForm stuff ->
            FromOnePointForm
                { stuff
                    | basePoint = clearOtherPointHelp stuff.basePoint
                    , directionHelp = Nothing
                    , distanceHelp = Nothing
                }

        FromTwoPointsForm stuff ->
            FromTwoPointsForm
                { stuff
                    | basePointA = clearOtherPointHelp stuff.basePointA
                    , basePointB = clearOtherPointHelp stuff.basePointB
                    , pointsHelp = Nothing
                }

        IntersectionForm stuff ->
            IntersectionForm
                { stuff
                    | objectA = clearOtherIntersectableHelp stuff.objectA
                    , objectB = clearOtherIntersectableHelp stuff.objectB
                    , objectsHelp = Nothing
                    , whichHelp = Nothing
                }


clearAxisForm : AxisForm -> AxisForm
clearAxisForm form =
    case form of
        ThroughOnePointForm stuff ->
            ThroughOnePointForm
                { stuff
                    | point = clearOtherPointHelp stuff.point
                    , orientationHelp = Nothing
                }

        ThroughTwoPointsForm stuff ->
            ThroughTwoPointsForm
                { stuff
                    | pointA = clearOtherPointHelp stuff.pointA
                    , pointB = clearOtherPointHelp stuff.pointB
                    , pointsHelp = Nothing
                }


clearCircleForm : CircleForm -> CircleForm
clearCircleForm form =
    case form of
        WithRadiusForm stuff ->
            WithRadiusForm
                { stuff
                    | centerPoint = clearOtherPointHelp stuff.centerPoint
                    , radiusHelp = Nothing
                }

        ThroughThreePointsForm stuff ->
            ThroughThreePointsForm
                { stuff
                    | pointA = clearOtherPointHelp stuff.pointA
                    , pointB = clearOtherPointHelp stuff.pointB
                    , pointC = clearOtherPointHelp stuff.pointC
                }


clearCurveForm : CurveForm -> CurveForm
clearCurveForm form =
    case form of
        StraightForm stuff ->
            StraightForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                    , pointsHelp = Nothing
                }

        QuadraticForm stuff ->
            QuadraticForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , controlPoint = clearOtherPointHelp stuff.controlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                    , pointsHelp = Nothing
                }

        CubicForm stuff ->
            CubicForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , startControlPoint = clearOtherPointHelp stuff.startControlPoint
                    , endControlPoint = clearOtherPointHelp stuff.endControlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                    , pointsHelp = Nothing
                }


clearDetailForm : DetailForm -> DetailForm
clearDetailForm form =
    { form
        | firstCurve = Tuple.mapFirst clearFirstCurveForm form.firstCurve
        , nextCurves = List.map (Tuple.mapFirst clearNextCurveForm) form.nextCurves
        , lastCurve = Tuple.mapFirst clearLastCurveForm form.lastCurve
    }


clearFirstCurveForm : FirstCurveForm -> FirstCurveForm
clearFirstCurveForm form =
    case form of
        FirstStraightForm stuff ->
            FirstStraightForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                }

        FirstQuadraticForm stuff ->
            FirstQuadraticForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , controlPoint = clearOtherPointHelp stuff.controlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                }

        FirstCubicForm stuff ->
            FirstCubicForm
                { stuff
                    | startPoint = clearOtherPointHelp stuff.startPoint
                    , startControlPoint = clearOtherPointHelp stuff.startControlPoint
                    , endControlPoint = clearOtherPointHelp stuff.endControlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                }

        FirstReferencedCurveForm stuff ->
            FirstReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearNextCurveForm : NextCurveForm -> NextCurveForm
clearNextCurveForm form =
    case form of
        NextStraightForm stuff ->
            NextStraightForm
                { stuff | endPoint = clearOtherPointHelp stuff.endPoint }

        NextQuadraticForm stuff ->
            NextQuadraticForm
                { stuff
                    | controlPoint = clearOtherPointHelp stuff.controlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                }

        NextCubicForm stuff ->
            NextCubicForm
                { stuff
                    | startControlPoint = clearOtherPointHelp stuff.startControlPoint
                    , endControlPoint = clearOtherPointHelp stuff.endControlPoint
                    , endPoint = clearOtherPointHelp stuff.endPoint
                }

        NextReferencedCurveForm stuff ->
            NextReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearLastCurveForm : LastCurveForm -> LastCurveForm
clearLastCurveForm form =
    case form of
        LastStraightForm ->
            form

        LastQuadraticForm stuff ->
            LastQuadraticForm
                { stuff | controlPoint = clearOtherPointHelp stuff.controlPoint }

        LastCubicForm stuff ->
            LastCubicForm
                { stuff
                    | startControlPoint = clearOtherPointHelp stuff.startControlPoint
                    , endControlPoint = clearOtherPointHelp stuff.endControlPoint
                }

        LastReferencedCurveForm stuff ->
            LastReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearOtherPointHelp : OtherPointForm -> OtherPointForm
clearOtherPointHelp form =
    case form of
        ReferencedPointForm stuff ->
            ReferencedPointForm { stuff | help = Nothing }

        InlinedPointForm stuff ->
            InlinedPointForm { stuff | point = clearPointForm stuff.point }


clearOtherIntersectableHelp : OtherIntersectableForm -> OtherIntersectableForm
clearOtherIntersectableHelp form =
    case form of
        ReferencedIntersectableForm stuff ->
            ReferencedIntersectableForm { stuff | help = Nothing }

        InlinedAxisForm stuff ->
            InlinedAxisForm { stuff | axis = clearAxisForm stuff.axis }

        InlinedCircleForm stuff ->
            InlinedCircleForm { stuff | circle = clearCircleForm stuff.circle }

        InlinedCurveForm stuff ->
            InlinedCurveForm { stuff | curve = clearCurveForm stuff.curve }


clearOtherCurveHelp : OtherCurveForm -> OtherCurveForm
clearOtherCurveHelp form =
    form


{-| -}
type EditResult
    = EditOpen ( Edit, Cmd EditMsg )
    | EditSucceeded Pattern
    | EditCanceled


{-| -}
editUpdate : Pattern -> EditMsg -> Edit -> EditResult
editUpdate pattern msg edit =
    case msg of
        UpdatePressed ->
            let
                replaceWith replaceObject newObject =
                    case replaceObject newObject pattern of
                        Err replaceHelp ->
                            case replaceHelp of
                                BadNewObject _ ->
                                    EditOpen ( edit, Cmd.none )

                                CircularDependency ->
                                    EditOpen ( edit, Cmd.none )

                                ObjectDoesNotExist ->
                                    EditOpen ( edit, Cmd.none )

                        Ok newPattern ->
                            EditSucceeded newPattern
            in
            case edit of
                EditPoint stuff ->
                    let
                        addHelp formWithHelp =
                            EditOpen
                                ( EditPoint { stuff | form = formWithHelp }
                                , Cmd.none
                                )
                    in
                    newPointFrom (clearPointForm stuff.form) pattern
                        |> Result.mapError addHelp
                        |> Result.map (replaceWith (Pattern.replacePoint stuff.aPoint))
                        |> Result.resolve

                EditAxis stuff ->
                    let
                        addHelp formWithHelp =
                            EditOpen
                                ( EditAxis { stuff | form = formWithHelp }
                                , Cmd.none
                                )
                    in
                    newAxisFrom (clearAxisForm stuff.form) pattern
                        |> Result.mapError addHelp
                        |> Result.map (replaceWith (Pattern.replaceAxis stuff.aAxis))
                        |> Result.resolve

                EditCircle stuff ->
                    let
                        addHelp formWithHelp =
                            EditOpen
                                ( EditCircle { stuff | form = formWithHelp }
                                , Cmd.none
                                )
                    in
                    newCircleFrom (clearCircleForm stuff.form) pattern
                        |> Result.mapError addHelp
                        |> Result.map (replaceWith (Pattern.replaceCircle stuff.aCircle))
                        |> Result.resolve

                EditCurve stuff ->
                    let
                        addHelp formWithHelp =
                            EditOpen
                                ( EditCurve { stuff | form = formWithHelp }
                                , Cmd.none
                                )
                    in
                    newCurveFrom (clearCurveForm stuff.form) pattern
                        |> Result.mapError addHelp
                        |> Result.map (replaceWith (Pattern.replaceCurve stuff.aCurve))
                        |> Result.resolve

                EditDetail stuff ->
                    let
                        addHelp formWithHelp =
                            EditOpen
                                ( EditDetail { stuff | form = formWithHelp }
                                , Cmd.none
                                )
                    in
                    newDetailFrom (clearDetailForm stuff.form) pattern
                        |> Result.mapError addHelp
                        |> Result.map (replaceWith (Pattern.replaceDetail stuff.aDetail))
                        |> Result.resolve

        EditPreviewPressed ->
            EditOpen ( edit, Cmd.none )

        EditCancelPressed ->
            EditCanceled

        EditPointMsg pointMsg ->
            case edit of
                EditPoint stuff ->
                    let
                        ( newForm, pointCmd ) =
                            updatePointForm pattern stuff.objects pointMsg stuff.form
                    in
                    EditOpen
                        ( EditPoint { stuff | form = newForm }
                        , Cmd.map EditPointMsg pointCmd
                        )

                _ ->
                    EditOpen ( edit, Cmd.none )

        EditAxisMsg axisMsg ->
            case edit of
                EditAxis stuff ->
                    let
                        ( newForm, pointCmd ) =
                            updateAxisForm pattern stuff.objects axisMsg stuff.form
                    in
                    EditOpen
                        ( EditAxis { stuff | form = newForm }
                        , Cmd.map EditAxisMsg pointCmd
                        )

                _ ->
                    EditOpen ( edit, Cmd.none )

        EditCircleMsg circleMsg ->
            case edit of
                EditCircle stuff ->
                    let
                        ( newForm, pointCmd ) =
                            updateCircleForm pattern stuff.objects circleMsg stuff.form
                    in
                    EditOpen
                        ( EditCircle { stuff | form = newForm }
                        , Cmd.map EditCircleMsg pointCmd
                        )

                _ ->
                    EditOpen ( edit, Cmd.none )

        EditCurveMsg curveMsg ->
            case edit of
                EditCurve stuff ->
                    let
                        ( newForm, pointCmd ) =
                            updateCurveForm pattern stuff.objects curveMsg stuff.form
                    in
                    EditOpen
                        ( EditCurve { stuff | form = newForm }
                        , Cmd.map EditCurveMsg pointCmd
                        )

                _ ->
                    EditOpen ( edit, Cmd.none )

        EditDetailMsg detailMsg ->
            case edit of
                EditDetail stuff ->
                    let
                        ( newForm, pointCmd ) =
                            updateDetailForm pattern stuff.objects detailMsg stuff.form
                    in
                    EditOpen
                        ( EditDetail { stuff | form = newForm }
                        , Cmd.map EditDetailMsg pointCmd
                        )

                _ ->
                    EditOpen ( edit, Cmd.none )



---- SUBSCRIPTIONS


{-| -}
createSubscriptions : Create -> Sub CreateMsg
createSubscriptions model =
    Sub.none


{-| -}
editSubscriptions : Edit -> Sub EditMsg
editSubscriptions model =
    Sub.none



---- TAGS


type PointTag
    = FromOnePointTag
    | FromTwoPointsTag
    | IntersectionTag


pointTags : List ( PointTag, String )
pointTags =
    [ ( FromOnePointTag, "From 1 point" )
    , ( FromTwoPointsTag, "From 2 points" )
    , ( IntersectionTag, "At intersection" )
    ]


tagFromPointForm : PointForm -> PointTag
tagFromPointForm form =
    case form of
        FromOnePointForm _ ->
            FromOnePointTag

        FromTwoPointsForm _ ->
            FromTwoPointsTag

        IntersectionForm _ ->
            IntersectionTag


type TwoPointsPositionTag
    = TwoPointsPositionRatioTag
    | TwoPointsPositionFromATag
    | TwoPointsPositionFromBTag


twoPointsPositionTags : List ( TwoPointsPositionTag, String )
twoPointsPositionTags =
    [ ( TwoPointsPositionRatioTag, "At ratio" )
    , ( TwoPointsPositionFromATag, "From 1st" )
    , ( TwoPointsPositionFromBTag, "From 2nd" )
    ]


tagFromTwoPointsPosition : TwoPointsPosition -> TwoPointsPositionTag
tagFromTwoPointsPosition twoPointsPosition =
    case twoPointsPosition of
        TwoPointsPositionRatio _ ->
            TwoPointsPositionRatioTag

        TwoPointsPositionFromA _ ->
            TwoPointsPositionFromATag

        TwoPointsPositionFromB _ ->
            TwoPointsPositionFromBTag


type AxisTag
    = ThroughOnePointTag
    | ThroughTwoPointsTag


axisTags : List ( AxisTag, String )
axisTags =
    [ ( ThroughOnePointTag, "Through 1 point" )
    , ( ThroughTwoPointsTag, "Through 2 points" )
    ]


tagFromAxisForm : AxisForm -> AxisTag
tagFromAxisForm form =
    case form of
        ThroughOnePointForm _ ->
            ThroughOnePointTag

        ThroughTwoPointsForm _ ->
            ThroughTwoPointsTag


type CircleTag
    = WithRadiusTag
    | ThroughThreePointsTag


circleTags : List ( CircleTag, String )
circleTags =
    [ ( WithRadiusTag, "By radius" )
    , ( ThroughThreePointsTag, "Through 3 points" )
    ]


tagFromCircleForm : CircleForm -> CircleTag
tagFromCircleForm form =
    case form of
        WithRadiusForm _ ->
            WithRadiusTag

        ThroughThreePointsForm _ ->
            ThroughThreePointsTag


type CurveTag
    = StraightTag
    | QuadraticTag
    | CubicTag


curveTags : List ( CurveTag, String )
curveTags =
    [ ( StraightTag, "Straight" )
    , ( QuadraticTag, "Quadratic" )
    , ( CubicTag, "Cubic" )
    ]


tagFromCurveForm : CurveForm -> CurveTag
tagFromCurveForm form =
    case form of
        StraightForm _ ->
            StraightTag

        QuadraticForm _ ->
            QuadraticTag

        CubicForm _ ->
            CubicTag


type FirstCurveTag
    = FirstStraightTag
    | FirstQuadraticTag
    | FirstCubicTag
    | FirstReferencedCurveTag


firstCurveTags : List ( FirstCurveTag, String )
firstCurveTags =
    [ ( FirstStraightTag, "Straight" )
    , ( FirstQuadraticTag, "Quadratic" )
    , ( FirstCubicTag, "Cubic" )
    , ( FirstReferencedCurveTag, "Pick curve" )
    ]


tagFromFirstCurveForm : FirstCurveForm -> FirstCurveTag
tagFromFirstCurveForm form =
    case form of
        FirstStraightForm _ ->
            FirstStraightTag

        FirstQuadraticForm _ ->
            FirstQuadraticTag

        FirstCubicForm _ ->
            FirstCubicTag

        FirstReferencedCurveForm _ ->
            FirstReferencedCurveTag


type NextCurveTag
    = NextStraightTag
    | NextQuadraticTag
    | NextCubicTag
    | NextReferencedCurveTag


nextCurveTags : List ( NextCurveTag, String )
nextCurveTags =
    [ ( NextStraightTag, "Straight" )
    , ( NextQuadraticTag, "Quadratic" )
    , ( NextCubicTag, "Cubic" )
    , ( NextReferencedCurveTag, "Pick curve" )
    ]


tagFromNextCurveForm : NextCurveForm -> NextCurveTag
tagFromNextCurveForm form =
    case form of
        NextStraightForm _ ->
            NextStraightTag

        NextQuadraticForm _ ->
            NextQuadraticTag

        NextCubicForm _ ->
            NextCubicTag

        NextReferencedCurveForm _ ->
            NextReferencedCurveTag


type LastCurveTag
    = LastStraightTag
    | LastQuadraticTag
    | LastCubicTag
    | LastReferencedCurveTag


lastCurveTags : List ( LastCurveTag, String )
lastCurveTags =
    [ ( LastStraightTag, "Straight" )
    , ( LastQuadraticTag, "Quadratic" )
    , ( LastCubicTag, "Cubic" )
    , ( LastReferencedCurveTag, "Pick curve" )
    ]


tagFromLastCurveForm : LastCurveForm -> LastCurveTag
tagFromLastCurveForm form =
    case form of
        LastStraightForm ->
            LastStraightTag

        LastQuadraticForm _ ->
            LastQuadraticTag

        LastCubicForm _ ->
            LastCubicTag

        LastReferencedCurveForm _ ->
            LastReferencedCurveTag



--


type DirectionTag
    = LeftwardTag
    | RightwardTag
    | UpTag
    | DownTag
    | CustomTag


directionTags : List ( DirectionTag, String )
directionTags =
    [ ( CustomTag, "Angle" )
    , ( LeftwardTag, "Left" )
    , ( RightwardTag, "Right" )
    , ( UpTag, "Up" )
    , ( DownTag, "Down" )
    ]


tagFromDirection : Direction -> DirectionTag
tagFromDirection direction =
    case direction of
        Leftward ->
            LeftwardTag

        Rightward ->
            RightwardTag

        Up ->
            UpTag

        Down ->
            DownTag

        DirectionAngle _ ->
            CustomTag


type OrientationTag
    = HorizontalTag
    | VerticalTag
    | CustomOrientationTag


orientationTags : List ( OrientationTag, String )
orientationTags =
    [ ( CustomOrientationTag, "Angle" )
    , ( HorizontalTag, "Horizontal" )
    , ( VerticalTag, "Vertical" )
    ]


tagFromOrientation : Orientation -> OrientationTag
tagFromOrientation orientation =
    case orientation of
        Horizontal ->
            HorizontalTag

        Vertical ->
            VerticalTag

        OrientationAngle _ ->
            CustomOrientationTag



--


type OtherPointTag
    = ReferencedPointTag
    | InlinedPointTag


otherPointTags : List ( OtherPointTag, String )
otherPointTags =
    [ ( ReferencedPointTag, "Pick point" )
    , ( InlinedPointTag, "New point" )
    ]


tagFromOtherPointForm : OtherPointForm -> OtherPointTag
tagFromOtherPointForm form =
    case form of
        ReferencedPointForm _ ->
            ReferencedPointTag

        InlinedPointForm _ ->
            InlinedPointTag


type OtherIntersectableTag
    = ReferencedIntersectableTag
    | InlinedAxisTag
    | InlinedCircleTag
    | InlinedCurveTag


otherIntersectableTags =
    [ ( ReferencedIntersectableTag, "Pick object" )
    , ( InlinedAxisTag, "New axis" )
    , ( InlinedCircleTag, "New circle" )
    , ( InlinedCurveTag, "New curve" )
    ]


tagFromOtherIntersectableForm : OtherIntersectableForm -> OtherIntersectableTag
tagFromOtherIntersectableForm form =
    case form of
        ReferencedIntersectableForm _ ->
            ReferencedIntersectableTag

        InlinedAxisForm _ ->
            InlinedAxisTag

        InlinedCircleForm _ ->
            InlinedCircleTag

        InlinedCurveForm _ ->
            InlinedCurveTag



---- HELPER


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<unnamed>"


ordinalFromInt : Int -> String
ordinalFromInt int =
    case int of
        11 ->
            "11th"

        12 ->
            "12th"

        13 ->
            "13th"

        _ ->
            case modBy 10 int of
                1 ->
                    String.fromInt int ++ "st"

                2 ->
                    String.fromInt int ++ "nd"

                3 ->
                    String.fromInt int ++ "rd"

                _ ->
                    String.fromInt int ++ "th"
