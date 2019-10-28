module Dialog.Detail exposing
    ( Form, init, initFirstCurveFormWith, initNextCurvesFormWith, initLastCurveFormWith
    , ActionMenu(..)
    , new, clear
    , Msg, update
    , view
    )

{-|

@docs Form, init, initFirstCurveFormWith, initNextCurvesFormWith, initLastCurveFormWith
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


type alias Form otherPointForm =
    { firstCurve : ( FirstCurveForm otherPointForm, ActionMenu )
    , nextCurves : List ( NextCurveForm otherPointForm, ActionMenu )
    , lastCurve : ( LastCurveForm otherPointForm, ActionMenu )
    }


type FirstCurveForm otherPointForm
    = FirstStraightForm
        { startPoint : otherPointForm
        , endPoint : otherPointForm
        }
    | FirstQuadraticForm
        { startPoint : otherPointForm
        , controlPoint : otherPointForm
        , endPoint : otherPointForm
        }
    | FirstCubicForm
        { startPoint : otherPointForm
        , startControlPoint : otherPointForm
        , endControlPoint : otherPointForm
        , endPoint : otherPointForm
        }
    | FirstReferencedCurveForm
        { curve : OtherCurveForm
        , reversed : Bool
        }


type NextCurveForm otherPointForm
    = NextStraightForm
        { endPoint : otherPointForm
        }
    | NextQuadraticForm
        { controlPoint : otherPointForm
        , endPoint : otherPointForm
        }
    | NextCubicForm
        { startControlPoint : otherPointForm
        , endControlPoint : otherPointForm
        , endPoint : otherPointForm
        }
    | NextReferencedCurveForm
        { curve : OtherCurveForm
        , reversed : Bool
        }


type LastCurveForm otherPointForm
    = LastStraightForm
    | LastQuadraticForm
        { controlPoint : otherPointForm
        }
    | LastCubicForm
        { startControlPoint : otherPointForm
        , endControlPoint : otherPointForm
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


init : otherPointForm -> Form otherPointForm
init initOtherPointForm =
    { firstCurve =
        ( initFirstStraightForm initOtherPointForm
        , Closed
        )
    , nextCurves = []
    , lastCurve =
        ( LastStraightForm
        , Closed
        )
    }



-- FIRST


initFirstStraightForm : otherPointForm -> FirstCurveForm otherPointForm
initFirstStraightForm initOtherPointForm =
    FirstStraightForm
        { startPoint = initOtherPointForm
        , endPoint = initOtherPointForm
        }


initFirstQuadraticForm : otherPointForm -> FirstCurveForm otherPointForm
initFirstQuadraticForm initOtherPointForm =
    FirstQuadraticForm
        { startPoint = initOtherPointForm
        , controlPoint = initOtherPointForm
        , endPoint = initOtherPointForm
        }


initFirstCubicForm : otherPointForm -> FirstCurveForm otherPointForm
initFirstCubicForm initOtherPointForm =
    FirstCubicForm
        { startPoint = initOtherPointForm
        , startControlPoint = initOtherPointForm
        , endControlPoint = initOtherPointForm
        , endPoint = initOtherPointForm
        }


initFirstReferencedCurveForm : FirstCurveForm otherPointForm
initFirstReferencedCurveForm =
    FirstReferencedCurveForm
        { curve = initOtherCurveForm
        , reversed = False
        }



-- NEXT


initNextStraightForm : otherPointForm -> NextCurveForm otherPointForm
initNextStraightForm initOtherPointForm =
    NextStraightForm
        { endPoint = initOtherPointForm
        }


initNextQuadraticForm : otherPointForm -> NextCurveForm otherPointForm
initNextQuadraticForm initOtherPointForm =
    NextQuadraticForm
        { controlPoint = initOtherPointForm
        , endPoint = initOtherPointForm
        }


initNextCubicForm : otherPointForm -> NextCurveForm otherPointForm
initNextCubicForm initOtherPointForm =
    NextCubicForm
        { startControlPoint = initOtherPointForm
        , endControlPoint = initOtherPointForm
        , endPoint = initOtherPointForm
        }


initNextReferencedCurveForm : NextCurveForm otherPointForm
initNextReferencedCurveForm =
    NextReferencedCurveForm
        { curve = initOtherCurveForm
        , reversed = False
        }



-- LAST


initLastStraightForm : LastCurveForm otherPointForm
initLastStraightForm =
    LastStraightForm


initLastQuadraticForm : otherPointForm -> LastCurveForm otherPointForm
initLastQuadraticForm initOtherPointForm =
    LastQuadraticForm
        { controlPoint = initOtherPointForm
        }


initLastCubicForm : otherPointForm -> LastCurveForm otherPointForm
initLastCubicForm initOtherPointForm =
    LastCubicForm
        { startControlPoint = initOtherPointForm
        , endControlPoint = initOtherPointForm
        }


initLastReferencedCurveForm : LastCurveForm otherPointForm
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


initFirstCurveFormWith :
    (Pattern -> A Point -> Maybe otherPointForm)
    -> Pattern
    -> FirstCurve
    -> Maybe (FirstCurveForm otherPointForm)
initFirstCurveFormWith initOtherPointFormWith pattern firstCurve =
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


initNextCurvesFormWith :
    (Pattern -> A Point -> Maybe otherPointForm)
    -> Pattern
    -> List NextCurve
    -> Maybe (List (NextCurveForm otherPointForm))
initNextCurvesFormWith initOtherPointFormWith pattern nextCurves =
    initNextCurvesFormWithHelp initOtherPointFormWith pattern nextCurves []


initNextCurvesFormWithHelp :
    (Pattern -> A Point -> Maybe otherPointForm)
    -> Pattern
    -> List NextCurve
    -> List (NextCurveForm otherPointForm)
    -> Maybe (List (NextCurveForm otherPointForm))
initNextCurvesFormWithHelp initOtherPointFormWith pattern nextCurves collected =
    case nextCurves of
        [] ->
            Just (List.reverse collected)

        nextCurve :: rest ->
            case nextCurveFormWith initOtherPointFormWith pattern nextCurve of
                Nothing ->
                    Nothing

                Just nextCurveForm ->
                    initNextCurvesFormWithHelp initOtherPointFormWith
                        pattern
                        rest
                        (nextCurveForm :: collected)


nextCurveFormWith :
    (Pattern -> A Point -> Maybe otherPointForm)
    -> Pattern
    -> NextCurve
    -> Maybe (NextCurveForm otherPointForm)
nextCurveFormWith initOtherPointFormWith pattern nextCurve =
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


initLastCurveFormWith :
    (Pattern -> A Point -> Maybe otherPointForm)
    -> Pattern
    -> LastCurve
    -> Maybe (LastCurveForm otherPointForm)
initLastCurveFormWith initOtherPointFormWith pattern lastCurve =
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



---- NEW


new :
    (otherPointForm -> Pattern -> Result otherPointForm (A Point))
    -> (Pattern -> otherPointForm -> otherPointForm)
    -> Form otherPointForm
    -> Pattern
    -> Result (Form otherPointForm) Detail
new newOtherPointForm checkOtherPointForm form pattern =
    let
        getFirstCurve =
            newFirstCurveFrom
                newOtherPointForm
                checkOtherPointForm
                (Tuple.first form.firstCurve)
                pattern
                |> Result.mapError
                    (\firstCurveWithHelp ->
                        { form
                            | firstCurve =
                                ( firstCurveWithHelp
                                , Tuple.second form.firstCurve
                                )
                            , nextCurves =
                                List.map (Tuple.mapFirst (checkNextCurve checkOtherPointForm pattern))
                                    form.nextCurves
                            , lastCurve =
                                ( checkLastCurve checkOtherPointForm pattern (Tuple.first form.lastCurve)
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
                                ( checkLastCurve checkOtherPointForm pattern (Tuple.first form.lastCurve)
                                , Tuple.second form.lastCurve
                                )
                        }
                    )
                |> Result.andThen (getLastCurve firstCurve)

        getNextCurve ( nextCurveForm, actionMenu ) result =
            case result of
                Ok ( nextCurves, nextCurveForms ) ->
                    case
                        newNextCurveFrom newOtherPointForm
                            checkOtherPointForm
                            nextCurveForm
                            pattern
                    of
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
                        ( checkNextCurve checkOtherPointForm pattern nextCurveForm
                        , actionMenu
                        )
                            :: nextCurveForms

        getLastCurve firstCurve nextCurves =
            newLastCurveFrom newOtherPointForm
                checkOtherPointForm
                (Tuple.first form.lastCurve)
                pattern
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
    (otherPointForm -> Pattern -> Result otherPointForm (A Point))
    -> (Pattern -> otherPointForm -> otherPointForm)
    -> FirstCurveForm otherPointForm
    -> Pattern
    -> Result (FirstCurveForm otherPointForm) FirstCurve
newFirstCurveFrom newOtherPointForm checkOtherPointForm form pattern =
    case form of
        FirstStraightForm stuff ->
            let
                getStartPoint =
                    newOtherPointForm stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint startPoint =
                    newOtherPointForm stuff.endPoint pattern
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
                    newOtherPointForm stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , controlPoint = checkOtherPointForm pattern stuff.controlPoint
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getControlPoint

                getControlPoint startPoint =
                    newOtherPointForm stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint)

                getEndPoint startPoint controlPoint =
                    newOtherPointForm stuff.endPoint pattern
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
                    newOtherPointForm stuff.startPoint pattern
                        |> Result.mapError
                            (\startPointWithHelp ->
                                { stuff
                                    | startPoint = startPointWithHelp
                                    , startControlPoint =
                                        checkOtherPointForm pattern stuff.startControlPoint
                                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getStartControlPoint

                getStartControlPoint startPoint =
                    newOtherPointForm stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndControlPoint startPoint)

                getEndControlPoint startPoint startControlPoint =
                    newOtherPointForm stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startPoint startControlPoint)

                getEndPoint startPoint startControlPoint endControlPoint =
                    newOtherPointForm stuff.endPoint pattern
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


newNextCurveFrom :
    (otherPointForm -> Pattern -> Result otherPointForm (A Point))
    -> (Pattern -> otherPointForm -> otherPointForm)
    -> NextCurveForm otherPointForm
    -> Pattern
    -> Result (NextCurveForm otherPointForm) NextCurve
newNextCurveFrom newOtherPointForm checkOtherPointForm form pattern =
    case form of
        NextStraightForm stuff ->
            let
                getEndPoint =
                    newOtherPointForm stuff.endPoint pattern
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
                    newOtherPointForm stuff.controlPoint pattern
                        |> Result.mapError
                            (\controlPointWithHelp ->
                                { stuff
                                    | controlPoint = controlPointWithHelp
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndPoint

                getEndPoint controlPoint =
                    newOtherPointForm stuff.endPoint pattern
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
                    newOtherPointForm stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newOtherPointForm stuff.endControlPoint pattern
                        |> Result.mapError
                            (\endControlPointWithHelp ->
                                { stuff
                                    | endControlPoint = endControlPointWithHelp
                                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                                }
                            )
                        |> Result.andThen (getEndPoint startControlPoint)

                getEndPoint startControlPoint endControlPoint =
                    newOtherPointForm stuff.endPoint pattern
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


checkNextCurve :
    (Pattern -> otherPointForm -> otherPointForm)
    -> Pattern
    -> NextCurveForm otherPointForm
    -> NextCurveForm otherPointForm
checkNextCurve checkOtherPointForm pattern form =
    case form of
        NextStraightForm stuff ->
            NextStraightForm
                { stuff | endPoint = checkOtherPointForm pattern stuff.endPoint }

        NextQuadraticForm stuff ->
            NextQuadraticForm
                { stuff
                    | controlPoint = checkOtherPointForm pattern stuff.controlPoint
                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                }

        NextCubicForm stuff ->
            NextCubicForm
                { stuff
                    | startControlPoint = checkOtherPointForm pattern stuff.startControlPoint
                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                    , endPoint = checkOtherPointForm pattern stuff.endPoint
                }

        NextReferencedCurveForm stuff ->
            form


newLastCurveFrom :
    (otherPointForm -> Pattern -> Result otherPointForm (A Point))
    -> (Pattern -> otherPointForm -> otherPointForm)
    -> LastCurveForm otherPointForm
    -> Pattern
    -> Result (LastCurveForm otherPointForm) LastCurve
newLastCurveFrom newOtherPointForm checkOtherPointForm form pattern =
    case form of
        LastStraightForm ->
            Ok LastStraight

        LastQuadraticForm stuff ->
            let
                getControlPoint =
                    newOtherPointForm stuff.controlPoint pattern
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
                    newOtherPointForm stuff.startControlPoint pattern
                        |> Result.mapError
                            (\startControlPointWithHelp ->
                                { stuff
                                    | startControlPoint = startControlPointWithHelp
                                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                                }
                            )
                        |> Result.andThen getEndControlPoint

                getEndControlPoint startControlPoint =
                    newOtherPointForm stuff.endControlPoint pattern
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


checkLastCurve :
    (Pattern -> otherPointForm -> otherPointForm)
    -> Pattern
    -> LastCurveForm otherPointForm
    -> LastCurveForm otherPointForm
checkLastCurve checkOtherPointForm pattern form =
    case form of
        LastStraightForm ->
            LastStraightForm

        LastQuadraticForm stuff ->
            LastQuadraticForm
                { stuff | controlPoint = checkOtherPointForm pattern stuff.controlPoint }

        LastCubicForm stuff ->
            LastCubicForm
                { stuff
                    | startControlPoint = checkOtherPointForm pattern stuff.startControlPoint
                    , endControlPoint = checkOtherPointForm pattern stuff.endControlPoint
                }

        LastReferencedCurveForm stuff ->
            form



---- CLEAR


clear :
    (otherPointForm -> otherPointForm)
    -> Form otherPointForm
    -> Form otherPointForm
clear clearOtherPointForm form =
    { form
        | firstCurve = Tuple.mapFirst (clearFirstCurveForm clearOtherPointForm) form.firstCurve
        , nextCurves = List.map (Tuple.mapFirst (clearNextCurveForm clearOtherPointForm)) form.nextCurves
        , lastCurve = Tuple.mapFirst (clearLastCurveForm clearOtherPointForm) form.lastCurve
    }


clearFirstCurveForm :
    (otherPointForm -> otherPointForm)
    -> FirstCurveForm otherPointForm
    -> FirstCurveForm otherPointForm
clearFirstCurveForm clearOtherPointForm form =
    case form of
        FirstStraightForm stuff ->
            FirstStraightForm
                { stuff
                    | startPoint = clearOtherPointForm stuff.startPoint
                    , endPoint = clearOtherPointForm stuff.endPoint
                }

        FirstQuadraticForm stuff ->
            FirstQuadraticForm
                { stuff
                    | startPoint = clearOtherPointForm stuff.startPoint
                    , controlPoint = clearOtherPointForm stuff.controlPoint
                    , endPoint = clearOtherPointForm stuff.endPoint
                }

        FirstCubicForm stuff ->
            FirstCubicForm
                { stuff
                    | startPoint = clearOtherPointForm stuff.startPoint
                    , startControlPoint = clearOtherPointForm stuff.startControlPoint
                    , endControlPoint = clearOtherPointForm stuff.endControlPoint
                    , endPoint = clearOtherPointForm stuff.endPoint
                }

        FirstReferencedCurveForm stuff ->
            FirstReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearNextCurveForm :
    (otherPointForm -> otherPointForm)
    -> NextCurveForm otherPointForm
    -> NextCurveForm otherPointForm
clearNextCurveForm clearOtherPointForm form =
    case form of
        NextStraightForm stuff ->
            NextStraightForm
                { stuff | endPoint = clearOtherPointForm stuff.endPoint }

        NextQuadraticForm stuff ->
            NextQuadraticForm
                { stuff
                    | controlPoint = clearOtherPointForm stuff.controlPoint
                    , endPoint = clearOtherPointForm stuff.endPoint
                }

        NextCubicForm stuff ->
            NextCubicForm
                { stuff
                    | startControlPoint = clearOtherPointForm stuff.startControlPoint
                    , endControlPoint = clearOtherPointForm stuff.endControlPoint
                    , endPoint = clearOtherPointForm stuff.endPoint
                }

        NextReferencedCurveForm stuff ->
            NextReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearLastCurveForm :
    (otherPointForm -> otherPointForm)
    -> LastCurveForm otherPointForm
    -> LastCurveForm otherPointForm
clearLastCurveForm clearOtherPointForm form =
    case form of
        LastStraightForm ->
            form

        LastQuadraticForm stuff ->
            LastQuadraticForm
                { stuff | controlPoint = clearOtherPointForm stuff.controlPoint }

        LastCubicForm stuff ->
            LastCubicForm
                { stuff
                    | startControlPoint = clearOtherPointForm stuff.startControlPoint
                    , endControlPoint = clearOtherPointForm stuff.endControlPoint
                }

        LastReferencedCurveForm stuff ->
            LastReferencedCurveForm
                { stuff | curve = clearOtherCurveHelp stuff.curve }


clearOtherCurveHelp : OtherCurveForm -> OtherCurveForm
clearOtherCurveHelp form =
    form



---- VIEW


view :
    (Pattern
     -> Objects
     -> { otherPoint : otherPointForm, id : String, label : String }
     -> Element otherPointMsg
    )
    -> Pattern
    -> Objects
    -> { detail : Form otherPointForm, id : String }
    -> Element (Msg otherPointMsg)
view viewOtherPointForm pattern objects { detail, id } =
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
                    , selected = tagFromFirstCurveForm (Tuple.first detail.firstCurve)
                    , child =
                        case Tuple.first detail.firstCurve of
                            FirstStraightForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.column
                                            [ Element.width Element.fill
                                            , Element.spacing Ui.Space.level1
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-straight--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm
                                                    pattern
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
                                            , Element.spacing Ui.Space.level1
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-quadratic--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveControlPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.controlPoint
                                                    , id = id ++ "__first-quadratic--control-point"
                                                    , label = "Control point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm
                                                    pattern
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
                                            , Element.spacing Ui.Space.level1
                                            ]
                                            [ Element.map FirstCurveStartPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.startPoint
                                                    , id = id ++ "__first-cubic--start-point"
                                                    , label = "Start point"
                                                    }
                                            , Element.map FirstCurveStartControlPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.startControlPoint
                                                    , id = id ++ "__first-cubic--start-control-point"
                                                    , label = "Start control point"
                                                    }
                                            , Element.map FirstCurveEndControlPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.endControlPoint
                                                    , id = id ++ "__first-cubic--end-control-point"
                                                    , label = "End control point"
                                                    }
                                            , Element.map FirstCurveEndPointMsg <|
                                                viewOtherPointForm
                                                    pattern
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
                        Ui.Atom.checkbox
                            { id = id ++ "__reverse-checkbox"
                            , onChange = FirstCurveReverseChanged
                            , checked = reversed
                            , label = "Reverse curve"
                            }

                    _ ->
                        Element.none
              ]
            , List.indexedMap (viewNextCurve viewOtherPointForm pattern objects id)
                detail.nextCurves
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
                    , selected = tagFromLastCurveForm (Tuple.first detail.lastCurve)
                    , child =
                        case Tuple.first detail.lastCurve of
                            LastStraightForm ->
                                Nothing

                            LastQuadraticForm stuff ->
                                Just <|
                                    Ui.Atom.nested <|
                                        Element.map LastCurveControlPointMsg <|
                                            viewOtherPointForm
                                                pattern
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
                                            , Element.spacing Ui.Space.level1
                                            ]
                                            [ Element.map LastCurveStartControlPointMsg <|
                                                viewOtherPointForm
                                                    pattern
                                                    objects
                                                    { otherPoint = stuff.startControlPoint
                                                    , id = id ++ "__last-cubic--start-control-point"
                                                    , label = "Start control point"
                                                    }
                                            , Element.map LastCurveEndControlPointMsg <|
                                                viewOtherPointForm
                                                    pattern
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
    (Pattern
     -> Objects
     -> { otherPoint : otherPointForm, id : String, label : String }
     -> Element otherPointMsg
    )
    -> Pattern
    -> Objects
    -> String
    -> Int
    -> ( NextCurveForm otherPointForm, ActionMenu )
    -> Element (Msg otherPointMsg)
viewNextCurve viewOtherPointForm pattern objects id index ( form, actionMenu ) =
    let
        actualId =
            id ++ "__next-" ++ String.fromInt index
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
            , selected = tagFromNextCurveForm form
            , child =
                case form of
                    NextStraightForm stuff ->
                        Just <|
                            Ui.Atom.nested <|
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing Ui.Space.level1
                                    ]
                                    [ Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
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
                                    , Element.spacing Ui.Space.level1
                                    ]
                                    [ Element.map (NextCurveControlPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
                                            objects
                                            { otherPoint = stuff.controlPoint
                                            , id = actualId ++ "__next-quadratic--control-point"
                                            , label = "Control point"
                                            }
                                    , Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
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
                                    , Element.spacing Ui.Space.level1
                                    ]
                                    [ Element.map (NextCurveStartControlPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
                                            objects
                                            { otherPoint = stuff.startControlPoint
                                            , id = actualId ++ "__next-cubic--start-control-point"
                                            , label = "Start control point"
                                            }
                                    , Element.map (NextCurveEndControlPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
                                            objects
                                            { otherPoint = stuff.endControlPoint
                                            , id = actualId ++ "__next-cubic--end-control-point"
                                            , label = "End control point"
                                            }
                                    , Element.map (NextCurveEndPointMsg index) <|
                                        viewOtherPointForm
                                            pattern
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
            , Element.htmlAttribute <|
                Html.Attributes.style "z-index" "1"
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
                                    , Element.htmlAttribute <|
                                        Html.Attributes.tabindex -1
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
                            , Font.color Ui.Color.black
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
                    [ Element.spacing Ui.Space.level1 ]
                    [ Element.text "Actions"
                    , Ui.Atom.fa "angle-down"
                    ]
            }
        ]



---- UPDATE


type Msg otherPointMsg
    = AddCurvePressed
      -- FIRST CURVE
    | FirstCurveTypeChanged FirstCurveTag
    | FirstCurveStartPointMsg otherPointMsg
    | FirstCurveStartControlPointMsg otherPointMsg
    | FirstCurveControlPointMsg otherPointMsg
    | FirstCurveEndControlPointMsg otherPointMsg
    | FirstCurveEndPointMsg otherPointMsg
    | FirstCurveDropdownMsg (Ui.Atom.Dropdown.Msg (A Curve))
    | FirstCurveReverseChanged Bool
    | FirstCurveActionMenuMsg ActionMenuMsg
      -- NEXT CURVE
    | NextCurveTypeChanged Int NextCurveTag
    | NextCurveStartControlPointMsg Int otherPointMsg
    | NextCurveControlPointMsg Int otherPointMsg
    | NextCurveEndControlPointMsg Int otherPointMsg
    | NextCurveEndPointMsg Int otherPointMsg
    | NextCurveDropdownMsg Int (Ui.Atom.Dropdown.Msg (A Curve))
    | NextCurveReverseChanged Int Bool
    | NextCurveActionMenuMsg Int ActionMenuMsg
      -- LAST CURVE
    | LastCurveTypeChanged LastCurveTag
    | LastCurveStartControlPointMsg otherPointMsg
    | LastCurveControlPointMsg otherPointMsg
    | LastCurveEndControlPointMsg otherPointMsg
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
    (Pattern
     -> Objects
     -> otherPointMsg
     -> otherPointForm
     -> ( otherPointForm, Cmd otherPointMsg )
    )
    -> otherPointForm
    -> Pattern
    -> Pattern.Objects
    -> Msg otherPointMsg
    -> Form otherPointForm
    -> ( Form otherPointForm, Cmd (Msg otherPointMsg) )
update otherPointFormUpdate initOtherPointForm pattern objects detailMsg detail =
    case detailMsg of
        AddCurvePressed ->
            ( { detail
                | nextCurves =
                    detail.nextCurves ++ [ ( initNextStraightForm initOtherPointForm, Closed ) ]
              }
            , Cmd.none
            )

        -- FIRST CURVE
        FirstCurveTypeChanged firstCurveTag ->
            ( if tagFromFirstCurveForm (Tuple.first detail.firstCurve) == firstCurveTag then
                detail

              else
                case firstCurveTag of
                    FirstStraightTag ->
                        { detail | firstCurve = ( initFirstStraightForm initOtherPointForm, Closed ) }

                    FirstQuadraticTag ->
                        { detail | firstCurve = ( initFirstQuadraticForm initOtherPointForm, Closed ) }

                    FirstCubicTag ->
                        { detail | firstCurve = ( initFirstCubicForm initOtherPointForm, Closed ) }

                    FirstReferencedCurveTag ->
                        { detail | firstCurve = ( initFirstReferencedCurveForm, Closed ) }
            , Cmd.none
            )

        FirstCurveStartPointMsg otherPointMsg ->
            case Tuple.first detail.firstCurve of
                FirstStraightForm stuff ->
                    let
                        ( newOtherPoint, subCmd ) =
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startControlPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.controlPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endControlPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
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
                                            (always ( initNextStraightForm initOtherPointForm, Closed ))
                                            detail.nextCurves
                                }

                            NextQuadraticTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextQuadraticForm initOtherPointForm, Closed ))
                                            detail.nextCurves
                                }

                            NextCubicTag ->
                                { detail
                                    | nextCurves =
                                        List.updateAt index
                                            (always ( initNextCubicForm initOtherPointForm, Closed ))
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextCubicForm { stuff | startControlPoint = newOtherPoint }
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.controlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextQuadraticForm { stuff | controlPoint = newOtherPoint }
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endControlPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextCubicForm { stuff | endControlPoint = newOtherPoint }
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextStraightForm { stuff | endPoint = newOtherPoint }
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
                    in
                    ( { detail
                        | nextCurves =
                            List.updateAt index
                                (always
                                    ( NextQuadraticForm { stuff | endPoint = newOtherPoint }
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endPoint
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
                        { detail | lastCurve = ( initLastQuadraticForm initOtherPointForm, Closed ) }

                    LastCubicTag ->
                        { detail | lastCurve = ( initLastCubicForm initOtherPointForm, Closed ) }

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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.startControlPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.controlPoint
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
                            otherPointFormUpdate pattern objects otherPointMsg stuff.endControlPoint
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


tagFromFirstCurveForm : FirstCurveForm otherPointForm -> FirstCurveTag
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


tagFromNextCurveForm : NextCurveForm otherPointForm -> NextCurveTag
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


tagFromLastCurveForm : LastCurveForm otherPointForm -> LastCurveTag
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
