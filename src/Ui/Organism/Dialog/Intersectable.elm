module Ui.Organism.Dialog.Intersectable exposing
    ( Form, whichSize
    , initReferenced, initWith
    , new, clear
    , Msg, update
    , view
    )

{-|

@docs Form, whichSize
@docs initReferenced, initWith
@docs new, clear
@docs Msg, update
@docs view

-}

import Element exposing (Element)
import Pattern
    exposing
        ( A
        , Axis
        , Circle
        , Curve
        , Intersectable(..)
        , IntersectableTag(..)
        , Pattern
        )
import Ui.Atom.Dropdown exposing (Dropdown)
import Ui.Atom.Input
import Ui.Theme.Typography


{-| -}
type Form axisForm circleForm curveForm
    = Referenced
        { dropdown : Dropdown
        , maybeIntersectable : Maybe Intersectable
        , help : Maybe String
        }
    | InlinedAxis
        { expanded : Bool
        , axis : axisForm
        }
    | InlinedCircle
        { expanded : Bool
        , circle : circleForm
        }
    | InlinedCurve
        { expanded : Bool
        , curve : curveForm
        }


{-| -}
whichSize : Form axisForm circleForm curveForm -> Form axisForm circleForm curveForm -> Maybe Int
whichSize formA formB =
    let
        tag form =
            case form of
                Referenced { maybeIntersectable } ->
                    Maybe.map Pattern.intersectableTag maybeIntersectable

                InlinedAxis _ ->
                    Just IntersectableAxisTag

                InlinedCircle _ ->
                    Just IntersectableCircleTag

                InlinedCurve _ ->
                    Just IntersectableCurveTag
    in
    Maybe.map2 Pattern.whichSize (tag formA) (tag formB)


type Tag
    = ReferencedTag
    | InlinedAxisTag
    | InlinedCircleTag
    | InlinedCurveTag


tags : List ( Tag, String )
tags =
    [ ( ReferencedTag, "Pick object" )
    , ( InlinedAxisTag, "New axis" )
    , ( InlinedCircleTag, "New circle" )
    , ( InlinedCurveTag, "New curve" )
    ]


{-| -}
initReferenced : Form axisForm circleForm curveForm
initReferenced =
    Referenced
        { dropdown = Ui.Atom.Dropdown.init
        , maybeIntersectable = Nothing
        , help = Nothing
        }


initInlinedAxis : axisForm -> Form axisForm circleForm curveForm
initInlinedAxis initAxis =
    InlinedAxis
        { expanded = True
        , axis = initAxis
        }


initInlinedCircle : circleForm -> Form axisForm circleForm curveForm
initInlinedCircle initCircle =
    InlinedCircle
        { expanded = True
        , circle = initCircle
        }


initInlinedCurve : curveForm -> Form axisForm circleForm curveForm
initInlinedCurve initCurve =
    InlinedCurve
        { expanded = True
        , curve = initCurve
        }


type alias InitConfig coordinates axisForm circleForm curveForm =
    { axis : Pattern coordinates -> A Axis -> Maybe axisForm
    , circle : Pattern coordinates -> A Circle -> Maybe circleForm
    , curve : Pattern coordinates -> A Curve -> Maybe curveForm
    }


{-| -}
initWith :
    InitConfig coordinates axisForm circleForm curveForm
    -> Pattern coordinates
    -> Intersectable
    -> Maybe (Form axisForm circleForm curveForm)
initWith initIntersectable pattern intersectable =
    if Pattern.inlinedIntersectable intersectable then
        case intersectable of
            IntersectableAxis aAxis ->
                let
                    toForm axis =
                        InlinedAxis
                            { expanded = False
                            , axis = axis
                            }
                in
                aAxis
                    |> initIntersectable.axis pattern
                    |> Maybe.map toForm

            IntersectableCircle aCircle ->
                let
                    toForm circle =
                        InlinedCircle
                            { expanded = False
                            , circle = circle
                            }
                in
                aCircle
                    |> initIntersectable.circle pattern
                    |> Maybe.map toForm

            IntersectableCurve aCurve ->
                let
                    toForm curve =
                        InlinedCurve
                            { expanded = False
                            , curve = curve
                            }
                in
                aCurve
                    |> initIntersectable.curve pattern
                    |> Maybe.map toForm

    else
        Just <|
            Referenced
                { dropdown = Ui.Atom.Dropdown.init
                , maybeIntersectable = Just intersectable
                , help = Nothing
                }


otherIntersectableFormExpanded : Form axisForm circleForm curveForm -> Bool
otherIntersectableFormExpanded form =
    case form of
        Referenced _ ->
            True

        InlinedAxis { expanded } ->
            expanded

        InlinedCircle { expanded } ->
            expanded

        InlinedCurve { expanded } ->
            expanded


type alias NewConfig coordinates axisForm circleForm curveForm =
    { axis : axisForm -> Pattern coordinates -> Result axisForm Axis
    , circle : circleForm -> Pattern coordinates -> Result circleForm Circle
    , curve : curveForm -> Pattern coordinates -> Result curveForm Curve
    }


{-| -}
new :
    NewConfig coordinates axisForm circleForm curveForm
    -> Form axisForm circleForm curveForm
    -> Pattern coordinates
    -> Result (Form axisForm circleForm curveForm) Intersectable
new newIntersectable form pattern =
    case form of
        Referenced stuff ->
            case stuff.maybeIntersectable of
                Nothing ->
                    Err <|
                        Referenced
                            { stuff | help = Just "Pick an object" }

                Just aIntersectable ->
                    Ok aIntersectable

        InlinedAxis stuff ->
            newIntersectable.axis stuff.axis pattern
                |> Result.mapError
                    (\axisFormWithHelp ->
                        InlinedAxis { stuff | axis = axisFormWithHelp }
                    )
                |> Result.map (Pattern.this >> IntersectableAxis)

        InlinedCircle stuff ->
            newIntersectable.circle stuff.circle pattern
                |> Result.mapError
                    (\circleFormWithHelp ->
                        InlinedCircle { stuff | circle = circleFormWithHelp }
                    )
                |> Result.map (Pattern.this >> IntersectableCircle)

        InlinedCurve stuff ->
            newIntersectable.curve stuff.curve pattern
                |> Result.mapError
                    (\curveFormWithHelp ->
                        InlinedCurve { stuff | curve = curveFormWithHelp }
                    )
                |> Result.map (Pattern.this >> IntersectableCurve)


type alias ClearConfig axisForm circleForm curveForm =
    { axis : axisForm -> axisForm
    , circle : circleForm -> circleForm
    , curve : curveForm -> curveForm
    }


{-| -}
clear :
    ClearConfig axisForm circleForm curveForm
    -> Form axisForm circleForm curveForm
    -> Form axisForm circleForm curveForm
clear clearIntersectable form =
    case form of
        Referenced stuff ->
            Referenced { stuff | help = Nothing }

        InlinedAxis stuff ->
            InlinedAxis { stuff | axis = clearIntersectable.axis stuff.axis }

        InlinedCircle stuff ->
            InlinedCircle { stuff | circle = clearIntersectable.circle stuff.circle }

        InlinedCurve stuff ->
            InlinedCurve { stuff | curve = clearIntersectable.curve stuff.curve }



---- VIEW


type alias ViewConfig coordinates axisForm axisMsg circleForm circleMsg curveForm curveMsg =
    { axis : Pattern coordinates -> Pattern.Objects -> { axis : axisForm, id : String } -> Element axisMsg
    , circle : Pattern coordinates -> Pattern.Objects -> { circle : circleForm, id : String } -> Element circleMsg
    , curve : Pattern coordinates -> Pattern.Objects -> { curve : curveForm, id : String } -> Element curveMsg
    }


{-| -}
view :
    ViewConfig coordinates axisForm axisMsg circleForm circleMsg curveForm curveMsg
    -> Pattern coordinates
    -> Pattern.Objects
    ->
        { otherIntersectable : Form axisForm circleForm curveForm
        , id : String
        , label : String
        }
    -> Element (Msg axisMsg circleMsg curveMsg)
view viewIntersectable pattern objects { otherIntersectable, id, label } =
    let
        selectedTag =
            tagFromOtherIntersectableForm otherIntersectable

        expanded =
            otherIntersectableFormExpanded otherIntersectable
    in
    Ui.Atom.Input.segmentControl
        { id = id
        , label = Just label
        , help = Nothing
        , onChange = OtherIntersectableTypeChanged
        , options = List.map (Tuple.mapSecond Ui.Theme.Typography.body) tags
        , selected = selectedTag
        , child =
            case otherIntersectable of
                Referenced { dropdown, maybeIntersectable } ->
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = intersectableName
                            , entryToHash = Pattern.hashIntersectable
                            }
                            { id = id ++ "__other-intersectable-object"
                            , lift = ReferencedIntersectableDropdownMsg
                            , label = label
                            }
                            (List.concat
                                [ List.map IntersectableAxis objects.axes
                                , List.map IntersectableCircle objects.circles
                                , List.map IntersectableCurve objects.curves
                                ]
                            )
                            dropdown
                            maybeIntersectable

                InlinedAxis { axis } ->
                    Just <|
                        Ui.Atom.Input.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedAxisMsg <|
                                    viewIntersectable.axis pattern
                                        objects
                                        { axis = axis
                                        , id = id ++ "__inlined--axis"
                                        }
                            , hidden = Element.none
                            }

                InlinedCircle { circle } ->
                    Just <|
                        Ui.Atom.Input.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedCircleMsg <|
                                    viewIntersectable.circle pattern
                                        objects
                                        { circle = circle
                                        , id = id ++ "__inlined--circle"
                                        }
                            , hidden = Element.none
                            }

                InlinedCurve { curve } ->
                    Just <|
                        Ui.Atom.Input.nestedHideable
                            { show = expanded
                            , onPress = InlinedIntersectableExpandToggled
                            , shown =
                                Element.map InlinedCurveMsg <|
                                    viewIntersectable.curve pattern
                                        objects
                                        { curve = curve
                                        , id = "__inlined--curve"
                                        }
                            , hidden = Element.none
                            }
        }



---- UPDATE


{-| -}
type Msg axisMsg circleMsg curveMsg
    = OtherIntersectableTypeChanged Tag
    | ReferencedIntersectableDropdownMsg (Ui.Atom.Dropdown.Msg Intersectable)
    | InlinedAxisMsg axisMsg
    | InlinedCircleMsg circleMsg
    | InlinedCurveMsg curveMsg
    | InlinedIntersectableExpandToggled


type alias UpdateConfig coordinates axisForm axisMsg circleForm circleMsg curveForm curveMsg =
    { updateAxis : Pattern coordinates -> Pattern.Objects -> axisMsg -> axisForm -> ( axisForm, Cmd axisMsg )
    , updateCircle : Pattern coordinates -> Pattern.Objects -> circleMsg -> circleForm -> ( circleForm, Cmd circleMsg )
    , updateCurve : Pattern coordinates -> Pattern.Objects -> curveMsg -> curveForm -> ( curveForm, Cmd curveMsg )
    , initAxis : axisForm
    , initCircle : circleForm
    , initCurve : curveForm
    }


{-| -}
update :
    UpdateConfig coordinates axisForm axisMsg circleForm circleMsg curveForm curveMsg
    -> Pattern coordinates
    -> Pattern.Objects
    -> Msg axisMsg circleMsg curveMsg
    -> Form axisForm circleForm curveForm
    -> ( Form axisForm circleForm curveForm, Cmd (Msg axisMsg circleMsg curveMsg) )
update config pattern objects msg form =
    case ( msg, form ) of
        ( OtherIntersectableTypeChanged intersectableTag, _ ) ->
            if tagFromOtherIntersectableForm form == intersectableTag then
                ( form, Cmd.none )

            else
                ( case intersectableTag of
                    ReferencedTag ->
                        initReferenced

                    InlinedAxisTag ->
                        initInlinedAxis config.initAxis

                    InlinedCircleTag ->
                        initInlinedCircle config.initCircle

                    InlinedCurveTag ->
                        initInlinedCurve config.initCurve
                , Cmd.none
                )

        ( ReferencedIntersectableDropdownMsg dropdownMsg, Referenced stuff ) ->
            let
                ( newDropdown, dropdownCmd, newMaybeIntersectable ) =
                    Ui.Atom.Dropdown.update
                        { entryToHash = Pattern.hashIntersectable }
                        (List.concat
                            [ List.map IntersectableAxis objects.axes
                            , List.map IntersectableCircle objects.circles
                            , List.map IntersectableCurve objects.curves
                            ]
                        )
                        dropdownMsg
                        stuff.dropdown
                        stuff.maybeIntersectable
            in
            ( Referenced
                { stuff
                    | dropdown = newDropdown
                    , maybeIntersectable = newMaybeIntersectable
                }
            , Cmd.map ReferencedIntersectableDropdownMsg dropdownCmd
            )

        ( InlinedAxisMsg axisMsg, InlinedAxis stuff ) ->
            let
                ( newAxis, axisCmd ) =
                    config.updateAxis pattern objects axisMsg stuff.axis
            in
            ( InlinedAxis { stuff | axis = newAxis }
            , Cmd.map InlinedAxisMsg axisCmd
            )

        ( InlinedCircleMsg circleMsg, InlinedCircle stuff ) ->
            let
                ( newCircle, circleCmd ) =
                    config.updateCircle pattern objects circleMsg stuff.circle
            in
            ( InlinedCircle { stuff | circle = newCircle }
            , Cmd.map InlinedCircleMsg circleCmd
            )

        ( InlinedCurveMsg curveMsg, InlinedCurve stuff ) ->
            let
                ( newCurve, curveCmd ) =
                    config.updateCurve pattern objects curveMsg stuff.curve
            in
            ( InlinedCurve { stuff | curve = newCurve }
            , Cmd.map InlinedCurveMsg curveCmd
            )

        ( InlinedIntersectableExpandToggled, InlinedAxis stuff ) ->
            ( InlinedAxis { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        ( InlinedIntersectableExpandToggled, InlinedCircle stuff ) ->
            ( InlinedCircle { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        ( InlinedIntersectableExpandToggled, InlinedCurve stuff ) ->
            ( InlinedCurve { stuff | expanded = not stuff.expanded }
            , Cmd.none
            )

        -- CATCH ALL
        _ ->
            ( form, Cmd.none )



---- HELPER


tagFromOtherIntersectableForm : Form axisForm circleForm curveForm -> Tag
tagFromOtherIntersectableForm form =
    case form of
        Referenced _ ->
            ReferencedTag

        InlinedAxis _ ->
            InlinedAxisTag

        InlinedCircle _ ->
            InlinedCircleTag

        InlinedCurve _ ->
            InlinedCurveTag


intersectableName : Intersectable -> String
intersectableName intersectable =
    Maybe.withDefault "<unnamed>" <|
        case intersectable of
            IntersectableAxis aAxis ->
                Pattern.name aAxis

            IntersectableCircle aCircle ->
                Pattern.name aCircle

            IntersectableCurve aCurve ->
                Pattern.name aCurve
