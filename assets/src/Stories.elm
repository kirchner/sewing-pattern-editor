port module Stories exposing (main)

import Bulletproof
import Bulletproof.Knob
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Length exposing (millimeters)
import Pattern
import Pixels exposing (pixels)
import Point2d
import Quantity
import Svg
import Svg.Attributes
import Time
import Ui.Atom.Input
import Ui.Atom.Marker
import Ui.Molecule.Modal
import Ui.Molecule.PatternList
import Ui.Organism.Dialog
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography


port saveSettings : String -> Cmd msg


main : Bulletproof.Program
main =
    Bulletproof.program saveSettings
        [ Bulletproof.folder "Theme"
            [ color
            ]
        , Bulletproof.folder "Atom"
            [ Bulletproof.folder "Input"
                [ buttons
                , iconButtons
                , checkboxes
                , text
                ]
            ]
        , Bulletproof.folder "Molecule"
            [ modals
            , patternList
            ]
        , Bulletproof.folder "Organism"
            [ dialogs
            ]
        , Bulletproof.folder "Page"
            [ landingpage
            , login
            , signup
            ]
        , marker
        ]


color : Bulletproof.Story
color =
    let
        block color_ =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 128)
                , Background.color color_
                ]
                Element.none
    in
    Bulletproof.story "Color"
        (Element.column
            [ Element.spacing 16
            , Element.width Element.fill
            ]
            [ block Ui.Theme.Color.primaryBright
            , block Ui.Theme.Color.primaryLight
            , block Ui.Theme.Color.primary
            , block Ui.Theme.Color.primaryDark
            , block Ui.Theme.Color.secondary
            , block Ui.Theme.Color.secondaryDark
            , block Ui.Theme.Color.danger
            , block Ui.Theme.Color.dangerDark
            , block Ui.Theme.Color.success
            , block Ui.Theme.Color.white
            , block Ui.Theme.Color.black
            , block Ui.Theme.Color.grayDark
            ]
            |> fromElmUI
        )


buttons : Bulletproof.Story
buttons =
    Bulletproof.folder "Buttons"
        [ Bulletproof.story "btnPrimary"
            (\label ->
                Ui.Atom.Input.btnPrimary
                    { id = "btnPrimary"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Create Pattern"
        , Bulletproof.story "btnSecondary"
            (\label ->
                Ui.Atom.Input.btnSecondary
                    { id = "btnSecondary"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Preview"
        , Bulletproof.story "btnSecondaryBordered"
            (\labelLeft labelRight ->
                Element.row []
                    [ Ui.Atom.Input.btnSecondaryBorderedLeft
                        { id = "btnSecondaryBorderedLeft"
                        , onPress = Nothing
                        , label = labelLeft
                        }
                    , Ui.Atom.Input.btnSecondaryBorderedRight
                        { id = "btnSecondaryBorderedRight"
                        , onPress = Nothing
                        , label = labelRight
                        }
                    ]
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label left" "Do something"
            |> Bulletproof.Knob.string "Label right" "Do something else"
        , Bulletproof.story "btnDanger"
            (\label ->
                Ui.Atom.Input.btnDanger
                    { id = "btnDanger"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Delete project"
        , Bulletproof.story "btnCancel"
            (\label ->
                Ui.Atom.Input.btnCancel
                    { id = "btnCancel"
                    , onPress = Nothing
                    , label = label
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "Cancel"
        ]


iconButtons : Bulletproof.Story
iconButtons =
    Bulletproof.folder "IconButtons"
        [ Bulletproof.story "btnIcon"
            (\icon ->
                Ui.Atom.Input.btnIcon
                    { id = "btnIcon"
                    , onPress = Nothing
                    , icon = icon
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "dog"
        , Bulletproof.story "btnIconDanger"
            (\icon ->
                Ui.Atom.Input.btnIconDanger
                    { id = "btnIconDanger"
                    , onPress = Nothing
                    , icon = icon
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "paw"
        , Bulletproof.story "btnIconLarge"
            (\icon ->
                Ui.Atom.Input.btnIconLarge
                    { id = "btnIconLarge"
                    , onPress = Nothing
                    , icon = icon
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Label" "bone"
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
        [ Bulletproof.story "text"
            (\text_ label help ->
                Ui.Atom.Input.text
                    { id = "text"
                    , onChange = \_ -> ()
                    , text = text_
                    , label = label
                    , help = help
                    }
                    |> fromElmUI
            )
            |> Bulletproof.Knob.string "Text" "arm_circ"
            |> Bulletproof.Knob.string "Label" "Name"
            |> Bulletproof.Knob.radio "Help"
                [ ( "Without help", Nothing )
                , ( "With help", Just "This is name is already taken." )
                ]
        , Bulletproof.story "formula"
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
    Bulletproof.folder "Modal"
        [ Bulletproof.story "small"
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
        , Bulletproof.story "wide"
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
    Bulletproof.story "PatternList"
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
    Bulletproof.folder "Dialog"
        [ Bulletproof.story "createPoint"
            (Ui.Organism.Dialog.createPoint
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        , Bulletproof.story "createAxis"
            (Ui.Organism.Dialog.createAxis
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        , Bulletproof.story "createCircle"
            (Ui.Organism.Dialog.createCircle
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        , Bulletproof.story "createCurve"
            (Ui.Organism.Dialog.createCurve
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        , Bulletproof.story "createDetail"
            (Ui.Organism.Dialog.createDetail
                |> Ui.Organism.Dialog.createView
                    { pattern = Pattern.empty
                    , hoveredInCanvas = Nothing
                    }
                |> fromElmUI
            )
        ]


landingpage : Bulletproof.Story
landingpage =
    Bulletproof.story "landingpage"
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color Ui.Theme.Color.secondary
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.paddingXY Ui.Theme.Spacing.level8 Ui.Theme.Spacing.level3
                ]
                [ Element.el
                    [ Element.alignLeft ]
                    (Ui.Theme.Typography.headingOne "SewingLab")
                , Element.row
                    [ Element.alignRight ]
                    [ Ui.Atom.Input.btnSecondary
                        { id = "login"
                        , onPress = Nothing
                        , label = "Log in"
                        }
                    , Ui.Atom.Input.btnPrimary
                        { id = "signup"
                        , onPress = Nothing
                        , label = "Sign up"
                        }
                    ]
                ]
            , Element.row
                [ Element.width (Element.fill |> Element.maximum 1024)
                , Element.centerX
                , Element.padding Ui.Theme.Spacing.level8
                , Element.spacing Ui.Theme.Spacing.level3
                ]
                [ Element.column
                    [ Element.width (Element.fillPortion 1)
                    , Element.spacing Ui.Theme.Spacing.level3
                    ]
                    [ Ui.Theme.Typography.headingTwo "A place for sewing patterns"
                    , Ui.Theme.Typography.paragraphBody
                        [ Element.text "SewingLab is a platform for creating customizable sewing patterns and sharing them with other people. Create bespoke clothing with patterns which are dynamically generated from body measurements."
                        ]
                    ]
                , Element.column
                    [ Element.width (Element.fillPortion 1)
                    , Background.color Ui.Theme.Color.white
                    , Element.padding Ui.Theme.Spacing.level3
                    , Element.spacing Ui.Theme.Spacing.level2
                    , Border.rounded 6
                    ]
                    [ Ui.Atom.Input.text
                        { id = "email"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Email"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.text
                        { id = "username"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Username"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.text
                        { id = "password"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Password"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.btnPrimaryFill
                        { id = "signup"
                        , onPress = Nothing
                        , label = "Sign up to SewingLab"
                        }
                    ]
                ]
            ]
            |> fromElmUI
        )


login : Bulletproof.Story
login =
    Bulletproof.story "login"
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color Ui.Theme.Color.secondary
            , Element.padding Ui.Theme.Spacing.level8
            ]
            (Element.column
                [ Element.centerX
                , Element.width (Element.px 640)
                , Element.padding Ui.Theme.Spacing.level5
                , Element.spacing Ui.Theme.Spacing.level4
                , Background.color Ui.Theme.Color.white
                , Border.rounded 6
                ]
                [ Element.el
                    [ Element.centerX ]
                    (Ui.Theme.Typography.headingTwo "Log in to SewingLab")
                , Element.el
                    [ Element.padding 7 ]
                    (Ui.Theme.Typography.paragraphBody
                        [ Element.text "Need a SewingLab account? "
                        , Element.link
                            [ Font.color Ui.Theme.Color.primary
                            , Font.underline
                            , Element.focused
                                [ Border.color Ui.Theme.Color.primaryDark ]
                            ]
                            { url = "#"
                            , label = Element.text "Create an account"
                            }
                        ]
                    )
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level2
                    ]
                    [ Ui.Atom.Input.text
                        { id = "usernameOrEmail"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Username or email address"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.text
                        { id = "password"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Password"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.btnPrimary
                        { id = "login"
                        , onPress = Nothing
                        , label = "Log in"
                        }
                    ]
                ]
            )
            |> fromElmUI
        )


signup : Bulletproof.Story
signup =
    Bulletproof.story "signup"
        (Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color Ui.Theme.Color.secondary
            , Element.padding Ui.Theme.Spacing.level8
            ]
            (Element.column
                [ Element.centerX
                , Element.width (Element.px 640)
                , Element.padding Ui.Theme.Spacing.level5
                , Element.spacing Ui.Theme.Spacing.level4
                , Background.color Ui.Theme.Color.white
                , Border.rounded 6
                ]
                [ Element.el
                    [ Element.centerX ]
                    (Ui.Theme.Typography.headingTwo "Get started with your account")
                , Element.el
                    [ Element.padding 7 ]
                    (Ui.Theme.Typography.paragraphBody
                        [ Element.text "Find sewing patterns. Adjust them to your needs or create your own. Already have an account? "
                        , Element.link
                            [ Font.color Ui.Theme.Color.primary
                            , Font.underline
                            , Element.focused
                                [ Border.color Ui.Theme.Color.primaryDark ]
                            ]
                            { url = "#"
                            , label = Element.text "Log in"
                            }
                        ]
                    )
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level2
                    ]
                    [ Ui.Atom.Input.text
                        { id = "email"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Email"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.text
                        { id = "username"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Username"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.text
                        { id = "password"
                        , onChange = \_ -> ()
                        , text = ""
                        , label = "Password"
                        , help = Nothing
                        }
                    , Ui.Atom.Input.btnPrimary
                        { id = "signup"
                        , onPress = Nothing
                        , label = "Sign up"
                        }
                    ]
                ]
            )
            |> fromElmUI
        )


marker : Bulletproof.Story
marker =
    let
        resolution =
            pixels 3 |> Quantity.per (millimeters 1)

        width =
            640

        height =
            640
    in
    Bulletproof.folder "Marker"
        [ Bulletproof.story "vertical"
            (\index ->
                Svg.svg
                    [ Svg.Attributes.viewBox <|
                        String.join " "
                            [ String.fromInt (width // -2)
                            , String.fromInt (height // -2)
                            , String.fromInt width
                            , String.fromInt height
                            ]
                    , Html.Attributes.style "width" (String.fromInt width ++ "px")
                    , Html.Attributes.style "height" (String.fromInt height ++ "px")
                    , Html.Attributes.style "border" "1px solid"
                    ]
                    [ Ui.Atom.Marker.draw
                        { orientation = Ui.Atom.Marker.Vertical
                        , index = index
                        , center = Point2d.origin
                        , resolution = resolution
                        }
                    ]
                    |> Element.html
                    |> fromElmUI
            )
            |> Bulletproof.Knob.int "Index"
                5
                [ Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 23
                ]
        , Bulletproof.story "horizontal"
            (\index ->
                Svg.svg
                    [ Svg.Attributes.viewBox <|
                        String.join " "
                            [ String.fromInt (width // -2)
                            , String.fromInt (height // -2)
                            , String.fromInt width
                            , String.fromInt height
                            ]
                    , Html.Attributes.style "width" (String.fromInt width ++ "px")
                    , Html.Attributes.style "height" (String.fromInt height ++ "px")
                    , Html.Attributes.style "border" "1px solid"
                    ]
                    [ Ui.Atom.Marker.draw
                        { orientation = Ui.Atom.Marker.Horizontal
                        , index = index
                        , center = Point2d.origin
                        , resolution = resolution
                        }
                    ]
                    |> Element.html
                    |> fromElmUI
            )
            |> Bulletproof.Knob.int "Index"
                5
                [ Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 23
                ]
        ]



---- FROM ELM UI


fromElmUI : Element msg -> Bulletproof.Renderer
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
