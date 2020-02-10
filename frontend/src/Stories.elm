port module Stories exposing (main)

import Bulletproof
import Bulletproof.Knob
import Element
import Element.Font as Font
import Pattern
import Time
import Ui.Atom.Input
import Ui.Molecule.Modal
import Ui.Molecule.PatternList
import Ui.Organism.Dialog
import Ui.Theme.Typography


port saveSettings : String -> Cmd msg


main : Bulletproof.Program
main =
    Bulletproof.program saveSettings
        [ Bulletproof.folder "Atoms"
            [ buttons
            , checkboxes
            , text
            ]
        , Bulletproof.folder "Molecules"
            [ modals
            , patternList
            ]
        , Bulletproof.folder "Organisms"
            [ dialogs
            ]
        ]


buttons : Bulletproof.Story
buttons =
    Bulletproof.folder "Buttons"
        [ Bulletproof.story "Primary"
            (\label ->
                Ui.Atom.Input.btnPrimary
                    { id = "btnPrimary"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Create Pattern"
        , Bulletproof.story "Secondary"
            (\label ->
                Ui.Atom.Input.btnSecondary
                    { id = "btnSecondary"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Preview"
        ]


checkboxes : Bulletproof.Story
checkboxes =
    Bulletproof.story "Checkbox"
        (\checked label ->
            Ui.Atom.Input.checkbox
                { id = "checkbox"
                , onChange = \_ -> ()
                , checked = checked
                , label = label
                }
                |> fromElmUI
        )
        |> Bulletproof.Knob.bool "Checked" False
        |> Bulletproof.Knob.string "Label" "Offline mode"


text : Bulletproof.Story
text =
    Bulletproof.folder "Text"
        [ Bulletproof.story "Formula"
            (\text_ label help ->
                Ui.Atom.Input.formula
                    { id = "formula"
                    , onChange = \_ -> ()
                    , text = text_
                    , label = label
                    , help = help
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.radio "Text"
                [ ( "Single line", "10 * arm_length" )
                , ( "Multiple lines", "a * (\n  c + d\n)" )
                ]
            |> Bulletproof.Knob.string "Label" "Distance"
            |> Bulletproof.Knob.radio "Help"
                [ ( "Without help", Nothing )
                , ( "With help", Just "There is a syntax error." )
                ]
        ]


modals : Bulletproof.Story
modals =
    Bulletproof.folder "Modals"
        [ Bulletproof.story "Small"
            (\title content ->
                Ui.Molecule.Modal.small Ui.Molecule.Modal.Open
                    { onCancelPress = ()
                    , onClosed = ()
                    , title = title
                    , content = Ui.Theme.Typography.body content
                    , actions =
                        [ Ui.Atom.Input.btnDanger
                            { id = "btnDanger"
                            , onPress = Nothing
                            , label = "Delete"
                            }
                        ]
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Title" "Delete project?"
            |> Bulletproof.Knob.string "Content" "Do you want to delete the project?"
        , Bulletproof.story "Wide"
            (\title content ->
                Ui.Molecule.Modal.wide Ui.Molecule.Modal.Open
                    { onCancelPress = ()
                    , onClosed = ()
                    , title = title
                    , content = Ui.Theme.Typography.body content
                    , actions =
                        [ Ui.Atom.Input.btnDanger
                            { id = "btnDanger"
                            , onPress = Nothing
                            , label = "Delete"
                            }
                        ]
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Title" "Delete project?"
            |> Bulletproof.Knob.string "Content" "Do you want to delete the project?"
        ]


patternList : Bulletproof.Story
patternList =
    Bulletproof.story "Pattern List"
        (\search patternInfos ->
            Ui.Molecule.PatternList.view
                { search = search
                , onSearchChange = \_ -> ()
                , onImport = ()
                , onCreate = ()
                , patternInfos = patternInfos
                , now = Time.millisToPosix 123012933
                }
                |> fromElmUI
        )
        |> Bulletproof.Knob.string "Search" ""
        |> Bulletproof.Knob.radio "Pattern infos"
            [ ( "No infos", [] )
            , ( "Some infos"
              , [ { name = "Trousers"
                  , description = "A pair of trousers"
                  , storage = Ui.Molecule.PatternList.LocalStorage "trousers"
                  , updatedAt = Time.millisToPosix 123010000
                  , onClone = ()
                  }
                , { name = "Bodice Block"
                  , description = "A basic bodice block"
                  , storage = Ui.Molecule.PatternList.Github "kirchner" "bodice-block"
                  , updatedAt = Time.millisToPosix 123000000
                  , onClone = ()
                  }
                ]
              )
            ]


dialogs : Bulletproof.Story
dialogs =
    Bulletproof.folder "Dialogs"
        [ Bulletproof.story "Create Point"
            (Ui.Organism.Dialog.createPoint
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        , Bulletproof.story "Create Axis"
            (Ui.Organism.Dialog.createAxis
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        ]



---- FROM ELM UI


fromElmUI =
    Bulletproof.fromElmUI
        [ Element.focusStyle
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing
            }
        ]
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.family
            [ Font.typeface "Rubik"
            , Font.sansSerif
            ]
        ]
