module Dialog.OtherPoint exposing
    ( Form
    , initReferenced, initInlined, initWith
    , new, clear
    , Msg, update
    , view
    )

{-|

@docs Form
@docs initReferenced, initInlined, initWith
@docs new, clear
@docs Msg, update
@docs view

-}

import Element exposing (Element)
import Pattern exposing (A, Objects, Pattern, Point)
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)
import Ui.Atom.Input



---- MODEL


{-| -}
type Form pointForm
    = Referenced
        { dropdown : Dropdown
        , maybeAPoint : Maybe (A Point)
        , help : Maybe String
        }
    | Inlined
        { expanded : Bool
        , point : pointForm
        }


type Tag
    = ReferencedTag
    | InlinedTag


tags : List ( Tag, String )
tags =
    [ ( ReferencedTag, "Pick point" )
    , ( InlinedTag, "New point" )
    ]


{-| -}
initReferenced : Form pointForm
initReferenced =
    Referenced
        { dropdown = Ui.Atom.Dropdown.init
        , maybeAPoint = Nothing
        , help = Nothing
        }


{-| -}
initInlined : Bool -> pointForm -> Form pointForm
initInlined expanded initFromOnePointForm =
    Inlined
        { expanded = expanded
        , point = initFromOnePointForm
        }


{-| -}
initWith :
    (Pattern coordinates -> A Point -> Maybe pointForm)
    -> Pattern coordinates
    -> A Point
    -> Maybe (Form pointForm)
initWith initPointFormWith pattern aPoint =
    if Pattern.inlined aPoint then
        let
            toForm point =
                Inlined
                    { expanded = False
                    , point = point
                    }
        in
        Maybe.map toForm (initPointFormWith pattern aPoint)

    else
        Just <|
            Referenced
                { dropdown = Ui.Atom.Dropdown.init
                , maybeAPoint = Just aPoint
                , help = Nothing
                }


{-| -}
new :
    (pointForm -> Pattern coordinates -> Result pointForm Point)
    -> Form pointForm
    -> Pattern coordinates
    -> Result (Form pointForm) (A Point)
new newPointFrom form pattern =
    case form of
        Referenced stuff ->
            case stuff.maybeAPoint of
                Nothing ->
                    Err (Referenced { stuff | help = Just "Pick a point" })

                Just aPoint ->
                    Ok aPoint

        Inlined stuff ->
            newPointFrom stuff.point pattern
                |> Result.mapError
                    (\pointFormWithHelp ->
                        Inlined { stuff | point = pointFormWithHelp }
                    )
                |> Result.map Pattern.this


{-| -}
clear : (pointForm -> pointForm) -> Form pointForm -> Form pointForm
clear clearPointForm form =
    case form of
        Referenced stuff ->
            Referenced { stuff | help = Nothing }

        Inlined stuff ->
            Inlined { stuff | point = clearPointForm stuff.point }



---- UPDATE


{-| -}
type Msg pointMsg
    = OtherPointTypeChanged Tag
    | ReferencedDropdownMsg (Ui.Atom.Dropdown.Msg (A Pattern.Point))
    | InlinedMsg pointMsg
    | InlinedExpandToggled


{-| -}
update :
    pointForm
    -> (Pattern coordinates -> Objects -> pointMsg -> pointForm -> ( pointForm, Cmd pointMsg ))
    -> Pattern coordinates
    -> Objects
    -> Msg pointMsg
    -> Form pointForm
    -> ( Form pointForm, Cmd (Msg pointMsg) )
update initFromOnePointForm updatePointForm pattern objects msg form =
    case msg of
        OtherPointTypeChanged otherPointTag ->
            case otherPointTag of
                ReferencedTag ->
                    ( initReferenced
                    , Cmd.none
                    )

                InlinedTag ->
                    ( initInlined True initFromOnePointForm
                    , Cmd.none
                    )

        ReferencedDropdownMsg dropdownMsg ->
            case form of
                Referenced stuff ->
                    let
                        ( newDropdown, dropdownCmd, newMaybeAPoint ) =
                            Ui.Atom.Dropdown.update
                                { entryToHash = Pattern.hash }
                                objects.points
                                dropdownMsg
                                stuff.dropdown
                                stuff.maybeAPoint
                    in
                    ( Referenced
                        { stuff
                            | dropdown = newDropdown
                            , maybeAPoint = newMaybeAPoint
                        }
                    , Cmd.map ReferencedDropdownMsg dropdownCmd
                    )

                _ ->
                    ( form, Cmd.none )

        InlinedMsg subMsg ->
            case form of
                Inlined stuff ->
                    let
                        ( newPoint, subCmd ) =
                            updatePointForm pattern objects subMsg stuff.point
                    in
                    ( Inlined { stuff | point = newPoint }
                    , Cmd.map InlinedMsg subCmd
                    )

                _ ->
                    ( form, Cmd.none )

        InlinedExpandToggled ->
            case form of
                Inlined stuff ->
                    ( Inlined { stuff | expanded = not stuff.expanded }
                    , Cmd.none
                    )

                _ ->
                    ( form, Cmd.none )



---- VIEW


{-| -}
view :
    (Pattern coordinates -> Objects -> { point : pointForm, id : String } -> Element pointMsg)
    -> Pattern coordinates
    -> Objects
    -> { otherPoint : Form pointForm, id : String, label : String }
    -> Element (Msg pointMsg)
view viewPointFormHelp pattern objects { otherPoint, id, label } =
    Ui.Atom.Input.segmentControl
        { id = id
        , label = Just label
        , help =
            case otherPoint of
                Referenced { help } ->
                    help

                _ ->
                    Nothing
        , onChange = OtherPointTypeChanged
        , options = tags
        , selected = tagFromForm otherPoint
        , child =
            case otherPoint of
                Referenced { dropdown, maybeAPoint, help } ->
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = objectName
                            , entryToHash = Pattern.hash
                            }
                            { id = id ++ "__referenced--point"
                            , lift = ReferencedDropdownMsg
                            , label = label
                            }
                            objects.points
                            dropdown
                            maybeAPoint

                Inlined { expanded, point } ->
                    Just <|
                        Ui.Atom.Input.nestedHideable
                            { show = expanded
                            , onPress = InlinedExpandToggled
                            , shown =
                                Element.map InlinedMsg <|
                                    viewPointFormHelp pattern
                                        objects
                                        { point = point
                                        , id = id ++ "__inlined--point"
                                        }
                            , hidden = Element.none
                            }
        }



---- HELP


objectName : A object -> String
objectName =
    Pattern.name >> Maybe.withDefault "<unnamed>"


tagFromForm : Form pointForm -> Tag
tagFromForm form =
    case form of
        Referenced _ ->
            ReferencedTag

        Inlined _ ->
            InlinedTag
