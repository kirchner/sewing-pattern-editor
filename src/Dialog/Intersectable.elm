module Dialog.Intersectable exposing
    ( Form, intersectableTagFromForm
    , initReferenced, initWith
    , new, clear
    , Msg, update
    , view
    )

{-|

@docs Form, intersectableTagFromForm
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
        , Intersectable
        , IntersectableTag(..)
        , Objects
        , Pattern
        )
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)


type Form axisForm circleForm curveForm
    = Referenced
        { dropdown : Dropdown
        , maybeAIntersectable : Maybe (A Intersectable)
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


intersectableTagFromForm : Pattern -> Form axisForm circleForm curveForm -> Maybe IntersectableTag
intersectableTagFromForm pattern form =
    case form of
        Referenced { maybeAIntersectable } ->
            Maybe.andThen (Pattern.tagFromIntersectable pattern)
                maybeAIntersectable

        InlinedAxis _ ->
            Just IntersectableAxisTag

        InlinedCircle _ ->
            Just IntersectableCircleTag

        InlinedCurve _ ->
            Just IntersectableCurveTag


initReferenced : Form axisForm circleForm curveForm
initReferenced =
    Referenced
        { dropdown = Ui.Atom.Dropdown.init
        , maybeAIntersectable = Nothing
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


type alias InitConfig axisForm circleForm curveForm =
    { axis : Pattern -> A Axis -> Maybe axisForm
    , circle : Pattern -> A Circle -> Maybe circleForm
    , curve : Pattern -> A Curve -> Maybe curveForm
    }


initWith :
    InitConfig axisForm circleForm curveForm
    -> Pattern
    -> A Intersectable
    -> Maybe (Form axisForm circleForm curveForm)
initWith initIntersectable pattern aIntersectable =
    if Pattern.inlined aIntersectable then
        case Pattern.tagFromIntersectable pattern aIntersectable of
            Just IntersectableAxisTag ->
                let
                    toForm axis =
                        InlinedAxis
                            { expanded = False
                            , axis = axis
                            }
                in
                Pattern.axisFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initIntersectable.axis pattern)
                    |> Maybe.map toForm

            Just IntersectableCircleTag ->
                let
                    toForm circle =
                        InlinedCircle
                            { expanded = False
                            , circle = circle
                            }
                in
                Pattern.circleFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initIntersectable.circle pattern)
                    |> Maybe.map toForm

            Just IntersectableCurveTag ->
                let
                    toForm curve =
                        InlinedCurve
                            { expanded = False
                            , curve = curve
                            }
                in
                Pattern.curveFromIntersectable pattern aIntersectable
                    |> Maybe.andThen (initIntersectable.curve pattern)
                    |> Maybe.map toForm

            Nothing ->
                Nothing

    else
        Just <|
            Referenced
                { dropdown = Ui.Atom.Dropdown.init
                , maybeAIntersectable = Just aIntersectable
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


type alias NewConfig axisForm circleForm curveForm =
    { axis : axisForm -> Pattern -> Result axisForm Axis
    , circle : circleForm -> Pattern -> Result circleForm Circle
    , curve : curveForm -> Pattern -> Result curveForm Curve
    }


new :
    NewConfig axisForm circleForm curveForm
    -> Form axisForm circleForm curveForm
    -> Pattern
    -> Result (Form axisForm circleForm curveForm) (A Intersectable)
new newIntersectable form pattern =
    case form of
        Referenced stuff ->
            case stuff.maybeAIntersectable of
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
                |> Result.map (Pattern.this >> Pattern.intersectableAxis)

        InlinedCircle stuff ->
            newIntersectable.circle stuff.circle pattern
                |> Result.mapError
                    (\circleFormWithHelp ->
                        InlinedCircle { stuff | circle = circleFormWithHelp }
                    )
                |> Result.map (Pattern.this >> Pattern.intersectableCircle)

        InlinedCurve stuff ->
            newIntersectable.curve stuff.curve pattern
                |> Result.mapError
                    (\curveFormWithHelp ->
                        InlinedCurve { stuff | curve = curveFormWithHelp }
                    )
                |> Result.map (Pattern.this >> Pattern.intersectableCurve)


type alias ClearConfig axisForm circleForm curveForm =
    { axis : axisForm -> axisForm
    , circle : circleForm -> circleForm
    , curve : curveForm -> curveForm
    }


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


type alias ViewConfig axisForm axisMsg circleForm circleMsg curveForm curveMsg =
    { axis : Pattern -> Pattern.Objects -> { axis : axisForm, id : String } -> Element axisMsg
    , circle : Pattern -> Pattern.Objects -> { circle : circleForm, id : String } -> Element circleMsg
    , curve : Pattern -> Pattern.Objects -> { curve : curveForm, id : String } -> Element curveMsg
    }


view :
    ViewConfig axisForm axisMsg circleForm circleMsg curveForm curveMsg
    -> Pattern
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
    Ui.Atom.segmentControl
        { id = id
        , label = Just label
        , help = Nothing
        , onChange = OtherIntersectableTypeChanged
        , options = tags
        , selected = selectedTag
        , child =
            case otherIntersectable of
                Referenced { dropdown, maybeAIntersectable, help } ->
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

                InlinedAxis { axis } ->
                    Just <|
                        Ui.Atom.nestedHideable
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
                        Ui.Atom.nestedHideable
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
                        Ui.Atom.nestedHideable
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


type Msg axisMsg circleMsg curveMsg
    = OtherIntersectableTypeChanged Tag
    | ReferencedIntersectableDropdownMsg (Ui.Atom.Dropdown.Msg (A Intersectable))
    | InlinedAxisMsg axisMsg
    | InlinedCircleMsg circleMsg
    | InlinedCurveMsg curveMsg
    | InlinedIntersectableExpandToggled


type alias UpdateConfig axisForm axisMsg circleForm circleMsg curveForm curveMsg =
    { updateAxis : Pattern -> Pattern.Objects -> axisMsg -> axisForm -> ( axisForm, Cmd axisMsg )
    , updateCircle : Pattern -> Pattern.Objects -> circleMsg -> circleForm -> ( circleForm, Cmd circleMsg )
    , updateCurve : Pattern -> Pattern.Objects -> curveMsg -> curveForm -> ( curveForm, Cmd curveMsg )
    , initAxis : axisForm
    , initCircle : circleForm
    , initCurve : curveForm
    }


update :
    UpdateConfig axisForm axisMsg circleForm circleMsg curveForm curveMsg
    -> Pattern
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
            ( Referenced
                { stuff
                    | dropdown = newDropdown
                    , maybeAIntersectable = newMaybeAIntersectable
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


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<unnamed>"
