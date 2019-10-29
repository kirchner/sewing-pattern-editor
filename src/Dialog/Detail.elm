module Dialog.Detail exposing
    ( Form, init, initWith
    , ActionMenu(..)
    , new, clear
    , Msg, update
    , view
    )

{-|

@docs Form, init, initWith
@docs ActionMenu
@docs new, clear
@docs Msg, update
@docs view

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Pattern
    exposing
        ( A
        , Curve
        , Detail
        , FirstCurve(..)
        , LastCurve(..)
        , NextCurve(..)
        , Objects
        , Pattern
        , Point
        )
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)
import Ui.Color
import Ui.Space



---- FORM


type alias Form point =
    { firstCurve : ( FCurve point, ActionMenu )
    , nextCurves : List ( NCurve point, ActionMenu )
    , lastCurve : ( LCurve point, ActionMenu )
    }


type FCurve point
    = FStraight (FStraightData point)
    | FQuadratic (FQuadraticData point)
    | FCubic (FCubicData point)
    | FReferenced FReferencedData


type alias FStraightData point =
    { startPoint : point
    , endPoint : point
    }


type alias FQuadraticData point =
    { startPoint : point
    , controlPoint : point
    , endPoint : point
    }


type alias FCubicData point =
    { startPoint : point
    , startControlPoint : point
    , endControlPoint : point
    , endPoint : point
    }


type alias FReferencedData =
    { curve : OtherCurveForm
    , reversed : Bool
    }


type NCurve point
    = NStraight (NStraightData point)
    | NQuadratic (NQuadraticData point)
    | NCubic (NCubicData point)
    | NReferenced NReferencedData


type alias NStraightData point =
    { endPoint : point
    }


type alias NQuadraticData point =
    { controlPoint : point
    , endPoint : point
    }


type alias NCubicData point =
    { startControlPoint : point
    , endControlPoint : point
    , endPoint : point
    }


type alias NReferencedData =
    { curve : OtherCurveForm
    , reversed : Bool
    }


type LCurve point
    = LStraight
    | LQuadratic (LQuadraticData point)
    | LCubic (LCubicData point)
    | LReferenced LReferencedData


type alias LQuadraticData point =
    { controlPoint : point
    }


type alias LCubicData point =
    { startControlPoint : point
    , endControlPoint : point
    }


type alias LReferencedData =
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



---- TAGS


type FCurveTag
    = FStraightTag
    | FQuadraticTag
    | FCubicTag
    | FReferencedTag


firstCurveTags : List ( FCurveTag, String )
firstCurveTags =
    [ ( FStraightTag, "Straight" )
    , ( FQuadraticTag, "Quadratic" )
    , ( FCubicTag, "Cubic" )
    , ( FReferencedTag, "Pick curve" )
    ]


type NCurveTag
    = NStraightTag
    | NQuadraticTag
    | NCubicTag
    | NReferencedTag


nextCurveTags : List ( NCurveTag, String )
nextCurveTags =
    [ ( NStraightTag, "Straight" )
    , ( NQuadraticTag, "Quadratic" )
    , ( NCubicTag, "Cubic" )
    , ( NReferencedTag, "Pick curve" )
    ]


type LastCurveTag
    = LStraightTag
    | LQuadraticTag
    | LCubicTag
    | LReferencedTag


lastCurveTags : List ( LastCurveTag, String )
lastCurveTags =
    [ ( LStraightTag, "Straight" )
    , ( LQuadraticTag, "Quadratic" )
    , ( LCubicTag, "Cubic" )
    , ( LReferencedTag, "Pick curve" )
    ]



---- INIT


init : point -> Form point
init initPoint =
    { firstCurve = ( initFStraight initPoint, Closed )
    , nextCurves = []
    , lastCurve = ( LStraight, Closed )
    }



-- FIRST


initFStraight : point -> FCurve point
initFStraight initPoint =
    FStraight
        { startPoint = initPoint
        , endPoint = initPoint
        }


initFQuadratic : point -> FCurve point
initFQuadratic initPoint =
    FQuadratic
        { startPoint = initPoint
        , controlPoint = initPoint
        , endPoint = initPoint
        }


initFCubic : point -> FCurve point
initFCubic initPoint =
    FCubic
        { startPoint = initPoint
        , startControlPoint = initPoint
        , endControlPoint = initPoint
        , endPoint = initPoint
        }


initFReferenced : FCurve point
initFReferenced =
    FReferenced
        { curve = initOtherCurveForm
        , reversed = False
        }



-- NEXT


initNStraight : point -> NCurve point
initNStraight initPoint =
    NStraight
        { endPoint = initPoint
        }


initNQuadratic : point -> NCurve point
initNQuadratic initPoint =
    NQuadratic
        { controlPoint = initPoint
        , endPoint = initPoint
        }


initNCubic : point -> NCurve point
initNCubic initPoint =
    NCubic
        { startControlPoint = initPoint
        , endControlPoint = initPoint
        , endPoint = initPoint
        }


initNReferenced : NCurve point
initNReferenced =
    NReferenced
        { curve = initOtherCurveForm
        , reversed = False
        }



-- LAST


initLStraight : LCurve point
initLStraight =
    LStraight


initLQuadratic : point -> LCurve point
initLQuadratic initPoint =
    LQuadratic
        { controlPoint = initPoint
        }


initLCubic : point -> LCurve point
initLCubic initPoint =
    LCubic
        { startControlPoint = initPoint
        , endControlPoint = initPoint
        }


initLReferenced : LCurve point
initLReferenced =
    LReferenced
        { curve = initOtherCurveForm
        , reversed = False
        }


initOtherCurveForm : OtherCurveForm
initOtherCurveForm =
    { dropdown = Ui.Atom.Dropdown.init
    , maybeACurve = Nothing
    }



-- INIT WITH


initWith :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> A Pattern.Detail
    -> Maybe (Form point)
initWith initPointWith pattern aDetail =
    let
        toForm firstCurve nextCurves lastCurve =
            { firstCurve = ( firstCurve, Closed )
            , nextCurves = List.map close nextCurves
            , lastCurve = ( lastCurve, Closed )
            }

        close nextCurve =
            ( nextCurve, Closed )
    in
    case Pattern.detailInfo aDetail pattern of
        Nothing ->
            Nothing

        Just info ->
            Maybe.map3 toForm
                (initFCurveWith initPointWith pattern info.firstCurve)
                (initNextCurvesFormWith initPointWith pattern info.nextCurves)
                (initLCurveWith initPointWith pattern info.lastCurve)


initFCurveWith :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> FirstCurve
    -> Maybe (FCurve point)
initFCurveWith initPointWith pattern firstCurve =
    let
        initPoint =
            initPointWith pattern
    in
    case firstCurve of
        FirstStraight stuff ->
            Maybe.map FStraight <|
                Maybe.map2 FStraightData
                    (initPoint stuff.startPoint)
                    (initPoint stuff.endPoint)

        FirstQuadratic stuff ->
            Maybe.map FQuadratic <|
                Maybe.map3 FQuadraticData
                    (initPoint stuff.startPoint)
                    (initPoint stuff.controlPoint)
                    (initPoint stuff.endPoint)

        FirstCubic stuff ->
            Maybe.map FCubic <|
                Maybe.map4 FCubicData
                    (initPoint stuff.startPoint)
                    (initPoint stuff.startControlPoint)
                    (initPoint stuff.endControlPoint)
                    (initPoint stuff.endPoint)

        FirstReferencedCurve stuff ->
            Just <|
                FReferenced
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }


initNextCurvesFormWith :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> List NextCurve
    -> Maybe (List (NCurve point))
initNextCurvesFormWith initPointWith pattern nextCurves =
    initNextCurvesFormWithHelp initPointWith pattern nextCurves []


initNextCurvesFormWithHelp :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> List NextCurve
    -> List (NCurve point)
    -> Maybe (List (NCurve point))
initNextCurvesFormWithHelp initPointWith pattern nextCurves collected =
    case nextCurves of
        [] ->
            Just (List.reverse collected)

        nextCurve :: rest ->
            case nextCurveFormWith initPointWith pattern nextCurve of
                Nothing ->
                    Nothing

                Just nextCurveForm ->
                    initNextCurvesFormWithHelp initPointWith
                        pattern
                        rest
                        (nextCurveForm :: collected)


nextCurveFormWith :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> NextCurve
    -> Maybe (NCurve point)
nextCurveFormWith initPointWith pattern nextCurve =
    case nextCurve of
        NextStraight stuff ->
            Maybe.map (NStraight << NStraightData)
                (initPointWith pattern stuff.endPoint)

        NextQuadratic stuff ->
            Maybe.map NQuadratic <|
                Maybe.map2 NQuadraticData
                    (initPointWith pattern stuff.controlPoint)
                    (initPointWith pattern stuff.endPoint)

        NextCubic stuff ->
            Maybe.map NCubic <|
                Maybe.map3 NCubicData
                    (initPointWith pattern stuff.startControlPoint)
                    (initPointWith pattern stuff.endControlPoint)
                    (initPointWith pattern stuff.endPoint)

        NextReferencedCurve stuff ->
            Just <|
                NReferenced
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }


initLCurveWith :
    (Pattern -> A Point -> Maybe point)
    -> Pattern
    -> LastCurve
    -> Maybe (LCurve point)
initLCurveWith initPointWith pattern lastCurve =
    case lastCurve of
        LastStraight ->
            Just LStraight

        LastQuadratic stuff ->
            Maybe.map (LQuadratic << LQuadraticData)
                (initPointWith pattern stuff.controlPoint)

        LastCubic stuff ->
            Maybe.map LCubic <|
                Maybe.map2 LCubicData
                    (initPointWith pattern stuff.startControlPoint)
                    (initPointWith pattern stuff.endControlPoint)

        LastReferencedCurve stuff ->
            Just <|
                LReferenced
                    { curve =
                        { dropdown = Ui.Atom.Dropdown.init
                        , maybeACurve = Just stuff.curve
                        }
                    , reversed = stuff.reversed
                    }



---- NEW


new :
    (point -> Pattern -> Result point (A Point))
    -> (Pattern -> point -> point)
    -> Form point
    -> Pattern
    -> Result (Form point) Detail
new newPoint checkPoint form pattern =
    let
        getFirstCurve =
            newFirstCurveFrom newPoint checkPoint (Tuple.first form.firstCurve) pattern
                |> Result.mapError
                    (\firstCurveWithHelp ->
                        { form
                            | firstCurve =
                                ( firstCurveWithHelp
                                , Tuple.second form.firstCurve
                                )
                            , nextCurves =
                                List.map (Tuple.mapFirst (checkNextCurve checkPoint pattern))
                                    form.nextCurves
                            , lastCurve =
                                ( checkLastCurve checkPoint pattern (Tuple.first form.lastCurve)
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
                                ( checkLastCurve checkPoint pattern (Tuple.first form.lastCurve)
                                , Tuple.second form.lastCurve
                                )
                        }
                    )
                |> Result.andThen (getLastCurve firstCurve)

        getNextCurve ( nextCurveForm, actionMenu ) result =
            case result of
                Ok ( nextCurves, nextCurveForms ) ->
                    case newNextCurveFrom newPoint checkPoint nextCurveForm pattern of
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
                        ( checkNextCurve checkPoint pattern nextCurveForm
                        , actionMenu
                        )
                            :: nextCurveForms

        getLastCurve firstCurve nextCurves =
            newLastCurveFrom newPoint checkPoint (Tuple.first form.lastCurve) pattern
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


newFirstCurveFrom :
    (point -> Pattern -> Result point (A Point))
    -> (Pattern -> point -> point)
    -> FCurve point
    -> Pattern
    -> Result (FCurve point) FirstCurve
newFirstCurveFrom newPoint checkPoint form pattern =
    case form of
        FStraight stuff ->
            let
                getStartPoint =
                    newPoint stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint startPoint =
                    newPoint stuff.endPoint pattern
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
            Result.mapError FStraight getStartPoint

        FQuadratic stuff ->
            let
                getStartPoint =
                    newPoint stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , controlPoint = checkPoint pattern stuff.controlPoint
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getControlPoint

                getControlPoint startPoint =
                    newPoint stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint)

                getEndPoint startPoint controlPoint =
                    newPoint stuff.endPoint pattern
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
            Result.mapError FQuadratic getStartPoint

        FCubic stuff ->
            let
                getStartPoint =
                    newPoint stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , startControlPoint =
                                        checkPoint pattern stuff.startControlPoint
                                    , endControlPoint = checkPoint pattern stuff.endControlPoint
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getStartControlPoint

                getStartControlPoint startPoint =
                    newPoint stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkPoint pattern stuff.endControlPoint
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndControlPoint startPoint)

                getEndControlPoint startPoint startControlPoint =
                    newPoint stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint startControlPoint)

                getEndPoint startPoint startControlPoint endControlPoint =
                    newPoint stuff.endPoint pattern
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
            Result.mapError FCubic getStartPoint

        FReferenced stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        FirstReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


newNextCurveFrom :
    (point -> Pattern -> Result point (A Point))
    -> (Pattern -> point -> point)
    -> NCurve point
    -> Pattern
    -> Result (NCurve point) NextCurve
newNextCurveFrom newPoint checkPoint form pattern =
    case form of
        NStraight stuff ->
            let
                getEndPoint =
                    newPoint stuff.endPoint pattern
                        |> Result.mapError
                            (\endPointWithHelp ->
                                { stuff | endPoint = endPointWithHelp }
                            )
                        |> Result.map toCurve

                toCurve endPoint =
                    NextStraight { endPoint = endPoint }
            in
            Result.mapError NStraight getEndPoint

        NQuadratic stuff ->
            let
                getControlPoint =
                    newPoint stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint controlPoint =
                    newPoint stuff.endPoint pattern
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
            Result.mapError NQuadratic getControlPoint

        NCubic stuff ->
            let
                getStartControlPoint =
                    newPoint stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkPoint pattern stuff.endControlPoint
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newPoint stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkPoint pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startControlPoint)

                getEndPoint startControlPoint endControlPoint =
                    newPoint stuff.endPoint pattern
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
            Result.mapError NCubic getStartControlPoint

        NReferenced stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        NextReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


checkNextCurve : (Pattern -> point -> point) -> Pattern -> NCurve point -> NCurve point
checkNextCurve checkPoint pattern form =
    let
        check =
            checkPoint pattern
    in
    case form of
        NStraight stuff ->
            NStraight
                { stuff | endPoint = check stuff.endPoint }

        NQuadratic stuff ->
            NQuadratic
                { stuff
                    | controlPoint = check stuff.controlPoint
                    , endPoint = check stuff.endPoint
                }

        NCubic stuff ->
            NCubic
                { stuff
                    | startControlPoint = check stuff.startControlPoint
                    , endControlPoint = check stuff.endControlPoint
                    , endPoint = check stuff.endPoint
                }

        NReferenced stuff ->
            form


newLastCurveFrom :
    (point -> Pattern -> Result point (A Point))
    -> (Pattern -> point -> point)
    -> LCurve point
    -> Pattern
    -> Result (LCurve point) LastCurve
newLastCurveFrom newPoint checkPoint form pattern =
    case form of
        LStraight ->
            Ok LastStraight

        LQuadratic stuff ->
            let
                getControlPoint =
                    newPoint stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff | controlPoint = controlPointWithHelp }
                            )
                        |> Result.map toCurve

                toCurve controlPoint =
                    LastQuadratic { controlPoint = controlPoint }
            in
            Result.mapError LQuadratic getControlPoint

        LCubic stuff ->
            let
                getStartControlPoint =
                    newPoint stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkPoint pattern stuff.endControlPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newPoint stuff.endControlPoint pattern
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
            Result.mapError LCubic getStartControlPoint

        LReferenced stuff ->
            case stuff.curve.maybeACurve of
                Nothing ->
                    Err form

                Just aCurve ->
                    Ok <|
                        LastReferencedCurve
                            { curve = aCurve
                            , reversed = stuff.reversed
                            }


checkLastCurve : (Pattern -> point -> point) -> Pattern -> LCurve point -> LCurve point
checkLastCurve checkPoint pattern form =
    case form of
        LStraight ->
            LStraight

        LQuadratic stuff ->
            LQuadratic
                { stuff | controlPoint = checkPoint pattern stuff.controlPoint }

        LCubic stuff ->
            LCubic
                { stuff
                    | startControlPoint = checkPoint pattern stuff.startControlPoint
                    , endControlPoint = checkPoint pattern stuff.endControlPoint
                }

        LReferenced stuff ->
            form



---- CLEAR


clear : (point -> point) -> Form point -> Form point
clear clearPoint form =
    { form
        | firstCurve = Tuple.mapFirst (clearFCurve clearPoint) form.firstCurve
        , nextCurves = List.map (Tuple.mapFirst (clearNCurve clearPoint)) form.nextCurves
        , lastCurve = Tuple.mapFirst (clearLCurve clearPoint) form.lastCurve
    }


clearFCurve : (point -> point) -> FCurve point -> FCurve point
clearFCurve clearPoint form =
    case form of
        FStraight stuff ->
            FStraight
                { stuff
                    | startPoint = clearPoint stuff.startPoint
                    , endPoint = clearPoint stuff.endPoint
                }

        FQuadratic stuff ->
            FQuadratic
                { stuff
                    | startPoint = clearPoint stuff.startPoint
                    , controlPoint = clearPoint stuff.controlPoint
                    , endPoint = clearPoint stuff.endPoint
                }

        FCubic stuff ->
            FCubic
                { stuff
                    | startPoint = clearPoint stuff.startPoint
                    , startControlPoint = clearPoint stuff.startControlPoint
                    , endControlPoint = clearPoint stuff.endControlPoint
                    , endPoint = clearPoint stuff.endPoint
                }

        FReferenced stuff ->
            FReferenced
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearNCurve : (point -> point) -> NCurve point -> NCurve point
clearNCurve clearPoint form =
    case form of
        NStraight stuff ->
            NStraight
                { stuff | endPoint = clearPoint stuff.endPoint }

        NQuadratic stuff ->
            NQuadratic
                { stuff
                    | controlPoint = clearPoint stuff.controlPoint
                    , endPoint = clearPoint stuff.endPoint
                }

        NCubic stuff ->
            NCubic
                { stuff
                    | startControlPoint = clearPoint stuff.startControlPoint
                    , endControlPoint = clearPoint stuff.endControlPoint
                    , endPoint = clearPoint stuff.endPoint
                }

        NReferenced stuff ->
            NReferenced
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearLCurve : (point -> point) -> LCurve point -> LCurve point
clearLCurve clearPoint form =
    case form of
        LStraight ->
            form

        LQuadratic stuff ->
            LQuadratic
                { stuff | controlPoint = clearPoint stuff.controlPoint }

        LCubic stuff ->
            LCubic
                { stuff
                    | startControlPoint = clearPoint stuff.startControlPoint
                    , endControlPoint = clearPoint stuff.endControlPoint
                }

        LReferenced stuff ->
            LReferenced
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearOtherCurveHelp : OtherCurveForm -> OtherCurveForm
clearOtherCurveHelp form =
    form



---- VIEW


view :
    (Pattern -> Objects -> { otherPoint : point, id : String, label : String } -> Element pointMsg)
    -> Pattern
    -> Objects
    -> { detail : Form point, id : String }
    -> Element (Msg pointMsg)
view viewPointHelp pattern objects { detail, id } =
    let
        ---- FIRST
        viewFStraight stuff =
            [ Element.map FirstCurveStartPointMsg <|
                viewPoint
                    { otherPoint = stuff.startPoint
                    , id = id ++ "__first-straight--start-point"
                    , label = "Start point"
                    }
            , Element.map FirstCurveEndPointMsg <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = id ++ "__first-straight--end-point"
                    , label = "End point"
                    }
            ]

        viewFQuadratic stuff =
            [ Element.map FirstCurveStartPointMsg <|
                viewPoint
                    { otherPoint = stuff.startPoint
                    , id = id ++ "__first-quadratic--start-point"
                    , label = "Start point"
                    }
            , Element.map FirstCurveControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.controlPoint
                    , id = id ++ "__first-quadratic--control-point"
                    , label = "Control point"
                    }
            , Element.map FirstCurveEndPointMsg <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = id ++ "__first-quadratic--end-point"
                    , label = "End point"
                    }
            ]

        viewFCubic stuff =
            [ Element.map FirstCurveStartPointMsg <|
                viewPoint
                    { otherPoint = stuff.startPoint
                    , id = id ++ "__first-cubic--start-point"
                    , label = "Start point"
                    }
            , Element.map FirstCurveStartControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.startControlPoint
                    , id = id ++ "__first-cubic--start-control-point"
                    , label = "Start control point"
                    }
            , Element.map FirstCurveEndControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.endControlPoint
                    , id = id ++ "__first-cubic--end-control-point"
                    , label = "End control point"
                    }
            , Element.map FirstCurveEndPointMsg <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = id ++ "__first-cubic--end-point"
                    , label = "End point"
                    }
            ]

        viewFReferenced curve =
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

        ---- LAST
        viewLQuadratic stuff =
            [ Element.map LastCurveControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.controlPoint
                    , id = id ++ "__last-quadratic--control-point"
                    , label = "Control point"
                    }
            ]

        viewLCubic stuff =
            [ Element.map LastCurveStartControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.startControlPoint
                    , id = id ++ "__last-cubic--start-control-point"
                    , label = "Start control point"
                    }
            , Element.map LastCurveEndControlPointMsg <|
                viewPoint
                    { otherPoint = stuff.endControlPoint
                    , id = id ++ "__last-cubic--end-control-point"
                    , label = "End control point"
                    }
            ]

        viewLReferenced curve =
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

        ---- HELPER
        viewPoint =
            viewPointHelp pattern objects

        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Ui.Space.level1 ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Space.level1
        ]
        (List.concat
            [ [ Ui.Atom.segmentControl
                    { id = id ++ "__first-curve-label"
                    , label = Just "1st curve"
                    , help = Nothing
                    , onChange = FirstCurveTypeChanged
                    , options = firstCurveTags
                    , selected = tagFromFCurve (Tuple.first detail.firstCurve)
                    , child =
                        case Tuple.first detail.firstCurve of
                            FStraight stuff ->
                                nested (viewFStraight stuff)

                            FQuadratic stuff ->
                                nested (viewFQuadratic stuff)

                            FCubic stuff ->
                                nested (viewFCubic stuff)

                            FReferenced { curve } ->
                                Just (viewFReferenced curve)
                    }
              , case Tuple.first detail.firstCurve of
                    FReferenced { reversed } ->
                        Ui.Atom.checkbox
                            { id = id ++ "__reverse-checkbox"
                            , onChange = FirstCurveReverseChanged
                            , checked = reversed
                            , label = "Reverse curve"
                            }

                    _ ->
                        Element.none
              ]
            , List.indexedMap (viewNextCurve viewPointHelp pattern objects id) detail.nextCurves
            , [ Ui.Atom.btnSecondary
                    { id = id ++ "__add-curve-button"
                    , onPress = Just AddCurvePressed
                    , label = "Add Curve"
                    }
              , Ui.Atom.segmentControl
                    { id = id ++ "__last-curve-label"
                    , label = Just "Closing curve"
                    , help = Nothing
                    , onChange = LastCurveTypeChanged
                    , options = lastCurveTags
                    , selected = tagFromLCurve (Tuple.first detail.lastCurve)
                    , child =
                        case Tuple.first detail.lastCurve of
                            LStraight ->
                                Nothing

                            LQuadratic stuff ->
                                nested (viewLQuadratic stuff)

                            LCubic stuff ->
                                nested (viewLCubic stuff)

                            LReferenced { curve } ->
                                Just (viewLReferenced curve)
                    }
              , case Tuple.first detail.lastCurve of
                    LReferenced { reversed } ->
                        Ui.Atom.checkbox
                            { id = id ++ "__reverse-checkbox"
                            , onChange = LastCurveReverseChanged
                            , checked = reversed
                            , label = "Reverse curve"
                            }

                    _ ->
                        Element.none
              ]
            ]
        )


viewNextCurve :
    (Pattern -> Objects -> { otherPoint : point, id : String, label : String } -> Element pointMsg)
    -> Pattern
    -> Objects
    -> String
    -> Int
    -> ( NCurve point, ActionMenu )
    -> Element (Msg pointMsg)
viewNextCurve viewPointHelp pattern objects id index ( form, actionMenu ) =
    let
        viewNStraight stuff =
            [ Element.map (NextCurveEndPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = actualId ++ "__next-straight--end-point"
                    , label = "End point"
                    }
            ]

        viewNQuadratic stuff =
            [ Element.map (NextCurveControlPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.controlPoint
                    , id = actualId ++ "__next-quadratic--control-point"
                    , label = "Control point"
                    }
            , Element.map (NextCurveEndPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = actualId ++ "__next-quadratic--end-point"
                    , label = "End point"
                    }
            ]

        viewNCubic stuff =
            [ Element.map (NextCurveStartControlPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.startControlPoint
                    , id = actualId ++ "__next-cubic--start-control-point"
                    , label = "Start control point"
                    }
            , Element.map (NextCurveEndControlPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.endControlPoint
                    , id = actualId ++ "__next-cubic--end-control-point"
                    , label = "End control point"
                    }
            , Element.map (NextCurveEndPointMsg index) <|
                viewPoint
                    { otherPoint = stuff.endPoint
                    , id = actualId ++ "__next-cubic--end-point"
                    , label = "End point"
                    }
            ]

        viewNReferenced curve =
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

        actualId =
            id ++ "__next-" ++ String.fromInt index

        viewPoint =
            viewPointHelp pattern objects

        nested =
            Just
                << Ui.Atom.nested
                << Element.column [ Element.width Element.fill, Element.spacing Ui.Space.level1 ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Space.level1
        ]
        [ Ui.Atom.segmentControl
            { id = actualId
            , label = Just (ordinalFromInt (index + 2) ++ " Curve")
            , help = Nothing
            , onChange = NextCurveTypeChanged index
            , options = nextCurveTags
            , selected = tagFromNCurve form
            , child =
                case form of
                    NStraight stuff ->
                        nested (viewNStraight stuff)

                    NQuadratic stuff ->
                        nested (viewNQuadratic stuff)

                    NCubic stuff ->
                        nested (viewNCubic stuff)

                    NReferenced { curve } ->
                        Just (viewNReferenced curve)
            }
        , case form of
            NReferenced { reversed } ->
                Ui.Atom.checkbox
                    { id = id ++ "__reverse-checkbox"
                    , onChange = NextCurveReverseChanged index
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
            [ Element.spacing Ui.Space.level1
            , Element.alignRight
            ]

         else
            [ Element.spacing Ui.Space.level1
            , Element.alignRight
            , Element.htmlAttribute (Html.Attributes.style "z-index" "1")
            ]
        )
        [ Input.button
            [ Element.paddingEach
                { left = Ui.Space.level1
                , right = Ui.Space.level1
                , top = Ui.Space.level1
                , bottom = Ui.Space.level1
                }
            , Font.size 10
            , Font.color Ui.Color.black
            , Border.widthEach
                { left = 0
                , right = 0
                , top = 0
                , bottom = 2
                }
            , Border.color Ui.Color.secondary
            , Background.color Ui.Color.secondary
            , Element.mouseOver
                [ Background.color Ui.Color.secondaryDark
                , Border.color Ui.Color.black
                ]
            , Element.focused
                [ Border.color Ui.Color.black ]
            , Element.htmlAttribute <|
                Html.Attributes.style "transition" <|
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
                                    , Background.color Ui.Color.secondary
                                    , Element.mouseOver
                                        [ Background.color Ui.Color.secondaryDark ]
                                    , Element.htmlAttribute (Html.Attributes.tabindex -1)
                                    , Element.htmlAttribute <|
                                        Html.Events.stopPropagationOn "click" <|
                                            Decode.succeed ( msg, True )
                                    ]
                                    (Element.text label)
                        in
                        Element.column
                            [ Events.onMouseDown MouseDown
                            , Events.onMouseUp MouseUp
                            , Element.moveDown 2
                            , Font.size 14
                            , Font.color Ui.Color.black
                            ]
                            [ viewAction MoveDownPressed "Move down"
                            , viewAction MoveUpPressed "Move up"
                            , viewAction InsertCurveBeforePressed "Insert curve before"
                            , viewAction InsertCurveAfterPressed "Insert curve after"
                            , viewAction RemovePressed "Remove"
                            ]
            , Events.onLoseFocus LostFocus
            ]
            { onPress = Just Pressed
            , label =
                Element.row
                    [ Element.spacing Ui.Space.level1 ]
                    [ Element.text "Actions"
                    , Ui.Atom.fa "angle-down"
                    ]
            }
        ]



---- UPDATE


type Msg pointMsg
    = AddCurvePressed
      -- FIRST CURVE
    | FirstCurveTypeChanged FCurveTag
    | FirstCurveStartPointMsg pointMsg
    | FirstCurveStartControlPointMsg pointMsg
    | FirstCurveControlPointMsg pointMsg
    | FirstCurveEndControlPointMsg pointMsg
    | FirstCurveEndPointMsg pointMsg
    | FirstCurveDropdownMsg (Ui.Atom.Dropdown.Msg (A Curve))
    | FirstCurveReverseChanged Bool
    | FirstCurveActionMenuMsg ActionMenuMsg
      -- NEXT CURVE
    | NextCurveTypeChanged Int NCurveTag
    | NextCurveStartControlPointMsg Int pointMsg
    | NextCurveControlPointMsg Int pointMsg
    | NextCurveEndControlPointMsg Int pointMsg
    | NextCurveEndPointMsg Int pointMsg
    | NextCurveDropdownMsg Int (Ui.Atom.Dropdown.Msg (A Curve))
    | NextCurveReverseChanged Int Bool
    | NextCurveActionMenuMsg Int ActionMenuMsg
      -- LAST CURVE
    | LastCurveTypeChanged LastCurveTag
    | LastCurveStartControlPointMsg pointMsg
    | LastCurveControlPointMsg pointMsg
    | LastCurveEndControlPointMsg pointMsg
    | LastCurveDropdownMsg (Ui.Atom.Dropdown.Msg (A Curve))
    | LastCurveReverseChanged Bool
    | LastCurveActionMenuMsg ActionMenuMsg


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


update :
    (Pattern -> Objects -> pointMsg -> point -> ( point, Cmd pointMsg ))
    -> point
    -> Pattern
    -> Pattern.Objects
    -> Msg pointMsg
    -> Form point
    -> ( Form point, Cmd (Msg pointMsg) )
update pointUpdate initPoint pattern objects detailMsg detail =
    case detailMsg of
        AddCurvePressed ->
            ( { detail
                | nextCurves =
                    detail.nextCurves ++ [ ( initNStraight initPoint, Closed ) ]
              }
            , Cmd.none
            )

        -- FIRST CURVE
        FirstCurveTypeChanged firstCurveTag ->
            ( if tagFromFCurve (Tuple.first detail.firstCurve) == firstCurveTag then
                detail

              else
                case firstCurveTag of
                    FStraightTag ->
                        { detail | firstCurve = ( initFStraight initPoint, Closed ) }

                    FQuadraticTag ->
                        { detail | firstCurve = ( initFQuadratic initPoint, Closed ) }

                    FCubicTag ->
                        { detail | firstCurve = ( initFCubic initPoint, Closed ) }

                    FReferencedTag ->
                        { detail | firstCurve = ( initFReferenced, Closed ) }
            , Cmd.none
            )

        FirstCurveStartPointMsg pointMsg ->
            let
                updateStartPoint toForm stuff =
                    let
                        ( newPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.startPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( toForm { stuff | startPoint = newPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartPointMsg subCmd
                    )
            in
            case Tuple.first detail.firstCurve of
                FStraight stuff ->
                    updateStartPoint FStraight stuff

                FQuadratic stuff ->
                    updateStartPoint FQuadratic stuff

                FCubic stuff ->
                    updateStartPoint FCubic stuff

                FReferenced stuff ->
                    ( detail, Cmd.none )

        FirstCurveStartControlPointMsg pointMsg ->
            case Tuple.first detail.firstCurve of
                FStraight stuff ->
                    ( detail, Cmd.none )

                FQuadratic stuff ->
                    ( detail, Cmd.none )

                FCubic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FCubic { stuff | startControlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveStartControlPointMsg subCmd
                    )

                FReferenced stuff ->
                    ( detail, Cmd.none )

        FirstCurveControlPointMsg pointMsg ->
            case Tuple.first detail.firstCurve of
                FStraight stuff ->
                    ( detail, Cmd.none )

                FQuadratic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.controlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FQuadratic { stuff | controlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveControlPointMsg subCmd
                    )

                FCubic stuff ->
                    ( detail, Cmd.none )

                FReferenced stuff ->
                    ( detail, Cmd.none )

        FirstCurveEndControlPointMsg pointMsg ->
            case Tuple.first detail.firstCurve of
                FStraight stuff ->
                    ( detail, Cmd.none )

                FQuadratic stuff ->
                    ( detail, Cmd.none )

                FCubic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( FCubic { stuff | endControlPoint = newOtherPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndControlPointMsg subCmd
                    )

                FReferenced stuff ->
                    ( detail, Cmd.none )

        FirstCurveEndPointMsg pointMsg ->
            let
                updateEndPoint toForm stuff =
                    let
                        ( newPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.startPoint
                    in
                    ( { detail
                        | firstCurve =
                            ( toForm { stuff | startPoint = newPoint }
                            , Tuple.second detail.firstCurve
                            )
                      }
                    , Cmd.map FirstCurveEndPointMsg subCmd
                    )
            in
            case Tuple.first detail.firstCurve of
                FStraight stuff ->
                    updateEndPoint FStraight stuff

                FQuadratic stuff ->
                    updateEndPoint FQuadratic stuff

                FCubic stuff ->
                    updateEndPoint FCubic stuff

                FReferenced stuff ->
                    ( detail, Cmd.none )

        FirstCurveDropdownMsg dropdownMsg ->
            case Tuple.first detail.firstCurve of
                FReferenced stuff ->
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
                            ( FReferenced
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
                FReferenced stuff ->
                    ( { detail
                        | firstCurve =
                            ( FReferenced { stuff | reversed = newValue }
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
                    ( if tagFromNCurve nextCurve == nextCurveTag then
                        detail

                      else
                        case nextCurveTag of
                            NStraightTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNStraight initPoint, Closed ))
                                            detail.nextCurves
                                }

                            NQuadraticTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNQuadratic initPoint, Closed ))
                                            detail.nextCurves
                                }

                            NCubicTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNCubic initPoint, Closed ))
                                            detail.nextCurves
                                }

                            NReferencedTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNReferenced, Closed ))
                                            detail.nextCurves
                                }
                    , Cmd.none
                    )

        NextCurveStartControlPointMsg index pointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NStraight _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NQuadratic stuff, _ ) ->
                    ( detail, Cmd.none )

                Just ( NCubic stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NCubic { stuff | startControlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveStartControlPointMsg index) subCmd
                    )

                Just ( NReferenced _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveControlPointMsg index pointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NStraight _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NQuadratic stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.controlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NQuadratic { stuff | controlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveControlPointMsg index) subCmd
                    )

                Just ( NCubic _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NReferenced _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveEndControlPointMsg index pointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NStraight _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NQuadratic _, _ ) ->
                    ( detail, Cmd.none )

                Just ( NCubic stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NCubic { stuff | endControlPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndControlPointMsg index) subCmd
                    )

                Just ( NReferenced _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveEndPointMsg index pointMsg ->
            case List.getAt index detail.nextCurves of
                Nothing ->
                    ( detail, Cmd.none )

                Just ( NStraight stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NStraight { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NQuadratic stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NQuadratic { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NCubic stuff, actionMenu ) ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NCubic { stuff | endPoint = newOtherPoint }
                                    , actionMenu
                                    )
                                )
                                detail.nextCurves
                      }
                    , Cmd.map (NextCurveEndPointMsg index) subCmd
                    )

                Just ( NReferenced _, _ ) ->
                    ( detail, Cmd.none )

        NextCurveDropdownMsg index dropdownMsg ->
            case List.getAt index detail.nextCurves of
                Just ( NReferenced stuff, actionMenu ) ->
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
                                    ( NReferenced
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
                Just ( NReferenced stuff, actionMenu ) ->
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NReferenced { stuff | reversed = newValue }
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
                tagFromLCurve (Tuple.first detail.lastCurve)
                    == lastCurveTag
              then
                detail

              else
                case lastCurveTag of
                    LStraightTag ->
                        { detail | lastCurve = ( initLStraight, Closed ) }

                    LQuadraticTag ->
                        { detail | lastCurve = ( initLQuadratic initPoint, Closed ) }

                    LCubicTag ->
                        { detail | lastCurve = ( initLCubic initPoint, Closed ) }

                    LReferencedTag ->
                        { detail | lastCurve = ( initLReferenced, Closed ) }
            , Cmd.none
            )

        LastCurveStartControlPointMsg pointMsg ->
            case Tuple.first detail.lastCurve of
                LStraight ->
                    ( detail, Cmd.none )

                LQuadratic stuff ->
                    ( detail, Cmd.none )

                LCubic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LCubic { stuff | startControlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveStartControlPointMsg subCmd
                    )

                LReferenced stuff ->
                    ( detail, Cmd.none )

        LastCurveControlPointMsg pointMsg ->
            case Tuple.first detail.lastCurve of
                LStraight ->
                    ( detail, Cmd.none )

                LQuadratic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.controlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LQuadratic { stuff | controlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveControlPointMsg subCmd
                    )

                LCubic stuff ->
                    ( detail, Cmd.none )

                LReferenced stuff ->
                    ( detail, Cmd.none )

        LastCurveEndControlPointMsg pointMsg ->
            case Tuple.first detail.lastCurve of
                LStraight ->
                    ( detail, Cmd.none )

                LQuadratic stuff ->
                    ( detail, Cmd.none )

                LCubic stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            pointUpdate pattern objects pointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | lastCurve =
                            ( LCubic { stuff | endControlPoint = newOtherPoint }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.map LastCurveEndControlPointMsg subCmd
                    )

                LReferenced stuff ->
                    ( detail, Cmd.none )

        LastCurveDropdownMsg dropdownMsg ->
            case Tuple.first detail.lastCurve of
                LReferenced stuff ->
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
                            ( LReferenced
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
                LReferenced stuff ->
                    ( { detail
                        | lastCurve =
                            ( LReferenced { stuff | reversed = newValue }
                            , Tuple.second detail.lastCurve
                            )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( detail, Cmd.none )

        LastCurveActionMenuMsg actionMenuMsg ->
            ( detail, Cmd.none )



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


tagFromFCurve : FCurve point -> FCurveTag
tagFromFCurve form =
    case form of
        FStraight _ ->
            FStraightTag

        FQuadratic _ ->
            FQuadraticTag

        FCubic _ ->
            FCubicTag

        FReferenced _ ->
            FReferencedTag


tagFromNCurve : NCurve point -> NCurveTag
tagFromNCurve form =
    case form of
        NStraight _ ->
            NStraightTag

        NQuadratic _ ->
            NQuadraticTag

        NCubic _ ->
            NCubicTag

        NReferenced _ ->
            NReferencedTag


tagFromLCurve : LCurve point -> LastCurveTag
tagFromLCurve form =
    case form of
        LStraight ->
            LStraightTag

        LQuadratic _ ->
            LQuadraticTag

        LCubic _ ->
            LCubicTag

        LReferenced _ ->
            LReferencedTag
