module DesignSystem exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Curve2d exposing (Curve2d(..))
import Detail2d exposing (LastCurve2d(..), NextCurve2d(..))
import Element exposing (DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Element
import Html.Attributes
import Json.Decode as Decode
import Pattern.Store exposing (StoredPattern)
import Pixels exposing (pixels)
import Point2d
import Quantity exposing (per)
import Task
import Time
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)
import Ui.Atom.Icon
import Ui.Atom.Input
import Ui.Atom.Object exposing (Intersectable(..))
import Ui.Atom.Tabs
import Ui.Molecule.MenuBtn
import Ui.Molecule.ObjectList
import Ui.Molecule.Pattern
import Ui.Molecule.PatternList
import Ui.Organism.CreatePattern
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography
import Url exposing (Url)
import Url.Parser exposing (Parser)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


type alias Model =
    { key : Key
    , route : Route
    , deviceClass : DeviceClass
    , orientation : Orientation
    , showNavigation : Bool
    , checked : Bool
    , fruit : Maybe Fruit
    , dropdown : Dropdown
    , selectedTab : Tab
    , selection : Maybe Fruit
    , position : Position
    , formula : String
    , dropdownAppended : Dropdown
    , selectionAppended : Maybe Fruit
    , positionAppended : Position
    , positionNested : Position
    , showFormula : Bool
    , objectsContainerWidth : Maybe Float
    , patternState : Ui.Molecule.Pattern.State
    , menuBtnPrimary : Ui.Molecule.MenuBtn.State
    , menuBtnSecondary : Ui.Molecule.MenuBtn.State
    , search : String
    , storageTag : Ui.Organism.CreatePattern.StorageTag
    }


type CreateAction
    = CreatePoint
    | CreateAxis
    | CreateCircle
    | CreateCurve
    | CreateDetail


type Fruit
    = Apple
    | Banana
    | Cherry
    | Durian
    | Elderberries
    | Figs
    | Grapefruit


fruits : List Fruit
fruits =
    [ Apple
    , Banana
    , Cherry
    , Durian
    , Elderberries
    , Figs
    , Grapefruit
    ]


fruitToString : Fruit -> String
fruitToString fruit =
    case fruit of
        Apple ->
            "Apple"

        Banana ->
            "Banana"

        Cherry ->
            "Cherry"

        Durian ->
            "Durian"

        Elderberries ->
            "Elderberries"

        Figs ->
            "Figs"

        Grapefruit ->
            "Grapefruit"


type Position
    = Left
    | Center
    | Right


positions : List ( Position, String )
positions =
    [ ( Left, "Left" )
    , ( Center, "Center" )
    , ( Right, "Right" )
    ]


type Tab
    = ObjectsTab
    | VariablesTab
    | GroupsTab


type Route
    = -- FOUNDATIONS
      Typography
    | Color
    | Space
      -- ATOMS
    | Buttons
    | FormElements
    | Icons
    | Objects
    | Tabs
      -- MOLECULES
    | JoinedFormElements
    | Dropdowns
    | ObjectList
    | PatternList
      -- ORGANISMS
    | CreatePattern


routeToTitle : Route -> String
routeToTitle route =
    case route of
        Typography ->
            "Typography"

        Color ->
            "Color"

        Space ->
            "Space"

        Buttons ->
            "Buttons"

        FormElements ->
            "Form Elements"

        Icons ->
            "Icons"

        Objects ->
            "Objects"

        Tabs ->
            "Tabs"

        JoinedFormElements ->
            "Joined Form Elements"

        Dropdowns ->
            "Dropdowns"

        ObjectList ->
            "Object List"

        PatternList ->
            "Pattern List"

        CreatePattern ->
            "Create Pattern"


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Typography ->
            "/typography"

        Color ->
            "/color"

        Space ->
            "/space"

        Buttons ->
            "/buttons"

        FormElements ->
            "/form-elements"

        Icons ->
            "/icons"

        Objects ->
            "/objects"

        Tabs ->
            "/tabs"

        JoinedFormElements ->
            "/joined-form-elements"

        Dropdowns ->
            "/dropdowns"

        ObjectList ->
            "/object-list"

        PatternList ->
            "/pattern-list"

        CreatePattern ->
            "/create-pattern"


routeFromUrl : Url -> Maybe Route
routeFromUrl url =
    Url.Parser.parse urlParser url


urlParser : Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Typography Url.Parser.top
        , Url.Parser.map Typography (Url.Parser.s "typography")
        , Url.Parser.map Color (Url.Parser.s "color")
        , Url.Parser.map Space (Url.Parser.s "space")
        , Url.Parser.map Buttons (Url.Parser.s "buttons")
        , Url.Parser.map FormElements (Url.Parser.s "form-elements")
        , Url.Parser.map Icons (Url.Parser.s "icons")
        , Url.Parser.map Objects (Url.Parser.s "objects")
        , Url.Parser.map Tabs (Url.Parser.s "tabs")
        , Url.Parser.map JoinedFormElements (Url.Parser.s "joined-form-elements")
        , Url.Parser.map Dropdowns (Url.Parser.s "dropdowns")
        , Url.Parser.map ObjectList (Url.Parser.s "object-list")
        , Url.Parser.map PatternList (Url.Parser.s "pattern-list")
        , Url.Parser.map CreatePattern (Url.Parser.s "create-pattern")
        ]



---- INIT


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { width, height } url key =
    let
        { class, orientation } =
            Element.classifyDevice
                { width = width
                , height = height
                }
    in
    changeRouteTo (routeFromUrl url)
        { key = key
        , route = Typography
        , deviceClass = class
        , orientation = orientation
        , showNavigation = False
        , checked = False
        , fruit = Nothing
        , dropdown = Ui.Atom.Dropdown.init
        , selection = Nothing
        , selectedTab = ObjectsTab
        , position = Left
        , formula = "distance(\n  A12,\n  B4\n)"
        , dropdownAppended = Ui.Atom.Dropdown.init
        , selectionAppended = Nothing
        , positionAppended = Left
        , positionNested = Left
        , showFormula = True
        , objectsContainerWidth = Nothing
        , patternState = Ui.Molecule.Pattern.init
        , menuBtnPrimary = Ui.Molecule.MenuBtn.init
        , menuBtnSecondary = Ui.Molecule.MenuBtn.init
        , search = ""
        , storageTag = Ui.Organism.CreatePattern.GithubTag
        }



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Design System"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width Element.fill
            , Element.height Element.fill
            , Font.family
                [ Font.typeface "Rubik"
                , Font.sansSerif
                ]
            ]
            (Element.lazy body model)
        ]
    }


body : Model -> Element Msg
body model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (if model.deviceClass == Phone then
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                [ navigationBar model.showNavigation
                , if model.showNavigation then
                    navigation model.deviceClass model.route

                  else
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.paddingXY Ui.Theme.Spacing.level2 Ui.Theme.Spacing.level2
                        , Element.scrollbarY
                        ]
                        (content model)
                ]

         else
            Element.row
                [ Element.width
                    (Element.fill
                        |> Element.maximum 1024
                    )
                , Element.paddingXY Ui.Theme.Spacing.level4 Ui.Theme.Spacing.level8
                , Element.height Element.fill
                , Element.centerX
                , Element.scrollbarY
                , Element.spacing Ui.Theme.Spacing.level5
                ]
                [ navigation model.deviceClass model.route
                , content model
                ]
        )


navigationBar : Bool -> Element Msg
navigationBar open =
    Element.el
        [ Element.width Element.fill
        , Border.widthEach
            { top = 0
            , bottom = 1
            , left = 0
            , right = 0
            }
        , Border.color Ui.Theme.Color.secondaryDark
        ]
        (Input.button
            [ Element.mouseOver
                [ Font.color Ui.Theme.Color.primaryDark ]
            , Element.padding Ui.Theme.Spacing.level3
            , Element.htmlAttribute <|
                Html.Attributes.class "navigation-menu"
            ]
            { onPress = Just ClickedNavigationMenu
            , label =
                Ui.Atom.Icon.faLarge <|
                    if open then
                        "times"

                    else
                        "bars"
            }
        )


navigation : DeviceClass -> Route -> Element Msg
navigation deviceClass currentRoute =
    let
        group : String -> List (Element msg) -> Element msg
        group title links =
            Element.column
                [ Element.spacing Ui.Theme.Spacing.level3
                , Element.width Element.fill
                ]
                [ Element.el
                    [ Font.variant Font.smallCaps
                    , Font.size fontSize
                    , Font.bold
                    , Element.padding Ui.Theme.Spacing.level2
                    ]
                    (Element.text title)
                , Element.column
                    [ Element.width Element.fill ]
                    links
                ]

        link : Route -> Element msg
        link route =
            Ui.Theme.Focus.outline <|
                Element.link
                    (if currentRoute == route then
                        [ Element.width Element.fill
                        , Background.color Ui.Theme.Color.primaryLight
                        ]

                     else
                        [ Element.width Element.fill
                        , Element.mouseOver
                            [ Background.color Ui.Theme.Color.primaryBright ]
                        ]
                    )
                    { url = routeToUrl route
                    , label =
                        Element.el
                            [ Element.padding Ui.Theme.Spacing.level2
                            , Font.size fontSize
                            ]
                            (Element.text (routeToTitle route))
                    }

        fontSize =
            case deviceClass of
                Phone ->
                    24

                _ ->
                    16

        width =
            case deviceClass of
                Phone ->
                    Element.fill

                _ ->
                    Element.px (5 * Ui.Theme.Spacing.level8)
    in
    Element.column
        [ Element.width width
        , Element.height Element.fill
        , Element.padding 13
        , Element.spacing Ui.Theme.Spacing.level3
        ]
        [ group "foundations"
            [ link Typography
            , link Color
            , link Space
            ]
        , group "atoms"
            [ link Buttons
            , link FormElements
            , link Icons
            , link Objects
            , link Tabs
            ]
        , group "molecules"
            [ link JoinedFormElements
            , link Dropdowns
            , link ObjectList
            , link PatternList
            ]
        , group "organisms"
            [ link CreatePattern
            ]
        ]


content : Model -> Element Msg
content model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level3
        , Element.alignTop
        ]
        [ Ui.Theme.Typography.headingOne (routeToTitle model.route)
        , case model.route of
            Typography ->
                viewTypography model

            Color ->
                viewColor model

            Space ->
                viewSpace model

            Buttons ->
                viewButtons model

            FormElements ->
                viewFormElements model

            Icons ->
                viewIcons model

            Objects ->
                viewObjects model

            Tabs ->
                viewTabs model

            JoinedFormElements ->
                viewJoinedFormElements model

            Dropdowns ->
                viewDropdowns model

            ObjectList ->
                viewObjectList model

            PatternList ->
                viewPatternList model

            CreatePattern ->
                viewCreatePattern model
        ]



-- TYPOGRAPHY


viewTypography : Model -> Element Msg
viewTypography _ =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Type Scale"
        , Element.column
            [ Element.paddingXY Ui.Theme.Spacing.level5 Ui.Theme.Spacing.level4
            , Element.spacing Ui.Theme.Spacing.level4
            ]
            [ Ui.Theme.Typography.headingOne "Heading One"
            , Ui.Theme.Typography.headingTwo "Heading Two"
            , Ui.Theme.Typography.headingThree "Heading Three"
            , Ui.Theme.Typography.headingFour "Heading Four"
            , Ui.Theme.Typography.bodyBold "Body Bold"
            , Ui.Theme.Typography.body "Body"
            , Ui.Theme.Typography.button "Button"
            ]
        , Ui.Theme.Typography.headingThree "Paragraphs and Links"
        , Ui.Theme.Typography.paragraphBody
            [ Element.text "This is a pragraph with a "
            , Ui.Atom.link
                { id = "link"
                , onPress = Nothing
                , label = "link"
                }
            , Element.text "."
            ]
        ]



-- COLOR


viewColor : Model -> Element Msg
viewColor _ =
    let
        colorBox backgroundColor fontColor name =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px (2 * Ui.Theme.Spacing.level8))
                , Border.width 1
                , Border.color Ui.Theme.Color.grayDark
                , Background.color backgroundColor
                , Font.color fontColor
                ]
                (Element.el [ Element.padding Ui.Theme.Spacing.level2 ]
                    (Ui.Theme.Typography.bodyBold name)
                )
    in
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ colorBox Ui.Theme.Color.primaryBright Ui.Theme.Color.black "Primary Bright"
        , colorBox Ui.Theme.Color.primaryLight Ui.Theme.Color.black "Primary Light"
        , colorBox Ui.Theme.Color.primary Ui.Theme.Color.white "Primary"
        , colorBox Ui.Theme.Color.primaryDark Ui.Theme.Color.white "Primary Dark"
        , colorBox Ui.Theme.Color.secondary Ui.Theme.Color.black "Secondary"
        , colorBox Ui.Theme.Color.secondaryDark Ui.Theme.Color.black "Secondary Dark"
        , colorBox Ui.Theme.Color.complementary Ui.Theme.Color.white "Complementary"
        , colorBox Ui.Theme.Color.complementaryDark Ui.Theme.Color.white "Complementary Dark"
        , colorBox Ui.Theme.Color.neutral Ui.Theme.Color.black "Neutral"
        , colorBox Ui.Theme.Color.neutralDark Ui.Theme.Color.black "Neutral Dark"
        , colorBox Ui.Theme.Color.danger Ui.Theme.Color.white "Danger"
        , colorBox Ui.Theme.Color.dangerDark Ui.Theme.Color.white "Danger Dark"
        , colorBox Ui.Theme.Color.success Ui.Theme.Color.black "Success"
        , colorBox Ui.Theme.Color.white Ui.Theme.Color.black "White"
        , colorBox Ui.Theme.Color.black Ui.Theme.Color.white "Black"
        , colorBox Ui.Theme.Color.grayDark Ui.Theme.Color.white "Gray Dark"
        ]



-- SPACE


viewSpace : Model -> Element Msg
viewSpace _ =
    let
        spaceBox space name =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px (3 * Ui.Theme.Spacing.level8))
                , Background.color Ui.Theme.Color.secondaryDark
                , Element.padding space
                ]
                (Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Background.color Ui.Theme.Color.secondary
                    ]
                    (Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Ui.Theme.Typography.bodyBold
                            (name ++ ": " ++ String.fromInt space ++ "px")
                        )
                    )
                )
    in
    Element.wrappedRow
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.width
                (Element.fill
                    |> Element.minimum (4 * Ui.Theme.Spacing.level8)
                )
            ]
            [ spaceBox Ui.Theme.Spacing.level1 "Level 1"
            , spaceBox Ui.Theme.Spacing.level2 "Level 2"
            , spaceBox Ui.Theme.Spacing.level3 "Level 3"
            , spaceBox Ui.Theme.Spacing.level4 "Level 4"
            ]
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.width
                (Element.fill
                    |> Element.minimum (4 * Ui.Theme.Spacing.level8)
                )
            ]
            [ spaceBox Ui.Theme.Spacing.level5 "Level 5"
            , spaceBox Ui.Theme.Spacing.level6 "Level 6"
            , spaceBox Ui.Theme.Spacing.level7 "Level 7"
            , spaceBox Ui.Theme.Spacing.level8 "Level 8"
            ]
        ]



-- BUTTONS


viewButtons : Model -> Element Msg
viewButtons _ =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Standard"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ Ui.Atom.Input.btnPrimary
                { id = "primary-btn"
                , onPress = Nothing
                , label = "Primary"
                }
            , Ui.Atom.Input.btnSecondary
                { id = "secondary-btn"
                , onPress = Nothing
                , label = "Secondary"
                }
            , Ui.Atom.Input.btnDanger
                { id = "danger-btn"
                , onPress = Nothing
                , label = "Danger"
                }
            , Ui.Atom.Input.btnCancel
                { id = "cancel-btn"
                , onPress = Nothing
                , label = "Cancel"
                }
            ]
        , Ui.Theme.Typography.headingThree "Call to Action"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ Ui.Atom.Input.btnCallToAction
                { id = "call-to-action-button"
                , onPress = Nothing
                , label = "Call to Action"
                }
            ]
        , Ui.Theme.Typography.headingThree "Icon"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ Ui.Atom.Input.btnIcon
                { id = "icon-btn"
                , onPress = Nothing
                , icon = "thumbs-up"
                }
            , Ui.Atom.Input.btnIconDanger
                { id = "icon-danger-btn"
                , onPress = Nothing
                , icon = "thumbs-down"
                }
            , Ui.Atom.Input.btnIconLarge
                { id = "icon-large-btn"
                , onPress = Nothing
                , icon = "thumbs-up"
                }
            ]
        ]



-- FORM ELEMENTS


viewFormElements : Model -> Element Msg
viewFormElements model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level2
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Checkbox"
        , Element.el
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.width Element.fill
            ]
            (Ui.Atom.Input.checkbox
                { id = "checkbox"
                , onChange = CheckedCheckbox
                , checked = model.checked
                , label = "Checkbox"
                }
            )
        , Ui.Theme.Typography.headingThree "Radio Buttons"
        , Element.el
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.width Element.fill
            ]
            (Ui.Atom.Input.radioColumn
                { id = "radio-column"
                , onChange = FruitChanged
                , options =
                    [ Ui.Atom.Input.option
                        { value = Apple
                        , label = "Apple"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Banana
                        , label = "Banana"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Cherry
                        , label = "Cherry"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Durian
                        , label = "Durian"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Elderberries
                        , label = "Elderberries"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Figs
                        , label = "Figs"
                        , child = Element.none
                        }
                    , Ui.Atom.Input.option
                        { value = Grapefruit
                        , label = "Grapefruit"
                        , child = Element.none
                        }
                    ]
                , selected = model.fruit
                , label = "Select a fruit"
                }
            )
        , Ui.Theme.Typography.headingThree "Dropdown"
        , Element.el
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.width Element.fill
            ]
            (Ui.Atom.Dropdown.view
                { entryToString = fruitToString
                , entryToHash = fruitToString
                }
                { id = "fruit-dropdown"
                , lift = DropdownMsg
                , label = "Fruit"
                }
                fruits
                model.dropdown
                model.selection
            )
        , Ui.Theme.Typography.headingThree "Segment Control"
        , Element.el
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.width Element.fill
            ]
            (Ui.Atom.Input.segmentControl
                { id = "position-segment-control"
                , label = Just "Position"
                , help = Nothing
                , onChange = ChangedPosition
                , options = positions
                , selected = model.position
                , child = Nothing
                }
            )
        , Ui.Theme.Typography.headingThree "Text"
        , Element.column
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.spacing Ui.Theme.Spacing.level4
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.text
                { id = "input-text"
                , onChange = \_ -> NoOp
                , text = ""
                , label = "Text"
                , help = Nothing
                }
            , Ui.Atom.Input.text
                { id = "input-text"
                , onChange = \_ -> NoOp
                , text = ""
                , label = "Text"
                , help = Just "Help message"
                }
            , Ui.Atom.Input.formula
                { id = "input-formula"
                , onChange = ChangedFormula
                , text = model.formula
                , label = "Formula"
                , help = Nothing
                }
            , Ui.Atom.Input.formula
                { id = "input-formula"
                , onChange = ChangedFormula
                , text = model.formula
                , label = "Formula"
                , help = Just "Help message"
                }
            ]
        ]



-- ICONS


viewIcons : Model -> Element Msg
viewIcons _ =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Normal"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.padding Ui.Theme.Spacing.level2
            ]
            [ Ui.Atom.Icon.fa "check"
            , Ui.Atom.Icon.fa "exclamation"
            , Ui.Atom.Icon.fa "check-circle"
            , Ui.Atom.Icon.fa "exclamation-circle"
            , Ui.Atom.Icon.fa "chevron-up"
            , Ui.Atom.Icon.fa "chevron-down"
            , Ui.Atom.Icon.point
            , Ui.Atom.Icon.axis
            , Ui.Atom.Icon.circle
            , Ui.Atom.Icon.curve
            , Ui.Atom.Icon.detail
            ]
        , Ui.Theme.Typography.headingThree "Body"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level3
            , Element.padding Ui.Theme.Spacing.level2
            ]
            [ Ui.Atom.Icon.faBody "check"
            , Ui.Atom.Icon.faBody "exclamation"
            , Ui.Atom.Icon.faBody "check-circle"
            , Ui.Atom.Icon.faBody "exclamation-circle"
            , Ui.Atom.Icon.faBody "chevron-up"
            , Ui.Atom.Icon.faBody "chevron-down"
            ]
        , Ui.Theme.Typography.headingThree "Large"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.padding Ui.Theme.Spacing.level2
            ]
            [ Ui.Atom.Icon.faLarge "check"
            , Ui.Atom.Icon.faLarge "exclamation"
            , Ui.Atom.Icon.faLarge "check-circle"
            , Ui.Atom.Icon.faLarge "exclamation-circle"
            , Ui.Atom.Icon.faLarge "chevron-up"
            , Ui.Atom.Icon.faLarge "chevron-down"
            ]
        ]



-- OBJECTS


viewObjects : Model -> Element Msg
viewObjects model =
    Element.el
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        , Element.htmlAttribute (Html.Attributes.id "objects-container")
        ]
        (case model.objectsContainerWidth of
            Nothing ->
                Element.none

            Just width ->
                let
                    resolution =
                        pixels (1 / 12 * width / 336)
                            |> per (millimeters 1)
                in
                Element.map PatternMsg <|
                    Ui.Molecule.Pattern.view
                        { id = "objects" }
                        { width = width
                        , height = width
                        , resolution = resolution
                        , center = Point2d.millimeters 0 800
                        }
                        storedPattern.pattern
                        model.patternState
        )


storedPattern : StoredPattern coordinates
storedPattern =
    Decode.decodeString Pattern.Store.decoder storedPatternRaw
        |> Result.withDefault (Pattern.Store.init "" "")


storedPatternRaw : String
storedPatternRaw =
    """
{
  "slug": "2ccc6480-1ae1-4001-904e-5c908cc14280",
  "name": "Demo",
  "pattern": {
    "points": {
      "A": {
        "type": "fromOnePoint",
        "basePoint": {
          "type": "that",
          "name": "Origin"
        },
        "direction": {
          "type": "rightward"
        },
        "distance": "1"
      },
      "B": {
        "type": "betweenRatio",
        "basePointA": {
          "type": "that",
          "name": "A"
        },
        "basePointB": {
          "type": "that",
          "name": "Origin"
        },
        "ratio": "0.2"
      },
      "C": {
        "type": "fromOnePoint",
        "basePoint": {
          "type": "that",
          "name": "B"
        },
        "direction": {
          "type": "down"
        },
        "distance": "2"
      },
      "D": {
        "type": "intersection",
        "objectA": {
          "type": "this",
          "intersectable": {
            "type": "intersectableAxis",
            "axis": {
              "type": "throughOnePoint",
              "point": {
                "type": "that",
                "name": "Origin"
              },
              "orientation": {
                "type": "vertical"
              }
            }
          }
        },
        "objectB": {
          "type": "this",
          "intersectable": {
            "type": "intersectableAxis",
            "axis": {
              "type": "throughOnePoint",
              "point": {
                "type": "that",
                "name": "C"
              },
              "orientation": {
                "type": "horizontal"
              }
            }
          }
        },
        "which": 1
      },
      "E": {
        "type": "intersection",
        "objectA": {
          "type": "this",
          "intersectable": {
            "type": "intersectableCircle",
            "circle": {
              "type": "withRadius",
              "centerPoint": {
                "type": "that",
                "name": "A"
              },
              "radius": "1.4"
            }
          }
        },
        "objectB": {
          "type": "this",
          "intersectable": {
            "type": "intersectableCircle",
            "circle": {
              "type": "withRadius",
              "centerPoint": {
                "type": "that",
                "name": "D"
              },
              "radius": "1.7"
            }
          }
        },
        "which": 1
      },
      "Origin": {
        "type": "origin",
        "x": 0,
        "y": 0
      }
    },
    "axes": {
      "Axis": {
        "type": "throughOnePoint",
        "point": {
          "type": "that",
          "name": "C"
        },
        "orientation": {
          "type": "orientationAngle",
          "angle": "30"
        }
      },
      "Axis2": {
        "type": "throughTwoPoints",
        "pointA": {
          "type": "that",
          "name": "Origin"
        },
        "pointB": {
          "type": "that",
          "name": "E"
        }
      }
    },
    "circles": {
      "Circle1": {
        "type": "throughThreePoints",
        "pointA": {
          "type": "that",
          "name": "E"
        },
        "pointB": {
          "type": "that",
          "name": "C"
        },
        "pointC": {
          "type": "that",
          "name": "D"
        }
      },
      "Circle2": {
        "type": "withRadius",
        "centerPoint": {
          "type": "that",
          "name": "B"
        },
        "radius": "1"
      }
    },
    "curves": {
      "Curve1": {
        "type": "straight",
        "startPoint": {
          "type": "that",
          "name": "B"
        },
        "endPoint": {
          "type": "that",
          "name": "E"
        }
      },
      "Curve2": {
        "type": "quadratic",
        "startPoint": {
          "type": "that",
          "name": "Origin"
        },
        "controlPoint": {
          "type": "this",
          "point": {
            "type": "fromOnePoint",
            "basePoint": {
              "type": "that",
              "name": "Origin"
            },
            "direction": {
              "type": "rightward"
            },
            "distance": "0.7"
          }
        },
        "endPoint": {
          "type": "that",
          "name": "C"
        }
      },
      "Curve3": {
        "type": "cubic",
        "startPoint": {
          "type": "that",
          "name": "Origin"
        },
        "startControlPoint": {
          "type": "this",
          "point": {
            "type": "fromOnePoint",
            "basePoint": {
              "type": "that",
              "name": "Origin"
            },
            "direction": {
              "type": "leftward"
            },
            "distance": "1"
          }
        },
        "endControlPoint": {
          "type": "this",
          "point": {
            "type": "fromOnePoint",
            "basePoint": {
              "type": "that",
              "name": "D"
            },
            "direction": {
              "type": "directionAngle",
              "angle": "270"
            },
            "distance": "1"
          }
        },
        "endPoint": {
          "type": "that",
          "name": "D"
        }
      }
    },
    "details": {
      "Detail1": {
        "firstCurve": {
          "type": "firstReferencedCurve",
          "curve": {
            "type": "that",
            "name": "Curve3"
          },
          "reversed": false
        },
        "nextCurves": [
          {
            "type": "nextReferencedCurve",
            "curve": {
              "type": "that",
              "name": "Curve2"
            },
            "reversed": false
          }
        ],
        "lastCurve": {
          "type": "lastCubic",
          "startControlPoint": {
            "type": "this",
            "point": {
              "type": "fromOnePoint",
              "basePoint": {
                "type": "that",
                "name": "C"
              },
              "direction": {
                "type": "down"
              },
              "distance": "1"
            }
          },
          "endControlPoint": {
            "type": "this",
            "point": {
              "type": "fromOnePoint",
              "basePoint": {
                "type": "that",
                "name": "D"
              },
              "direction": {
                "type": "down"
              },
              "distance": "1"
            }
          }
        }
      }
    },
    "transformations": {},
    "variables": {}
  },
  "zoom": 0.05209868481924359,
  "center": {
    "x": 1.0842357342697637,
    "y": 0.8810865034476534
  }
}"""



-- TABS


viewTabs : Model -> Element Msg
viewTabs model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        , Element.htmlAttribute (Html.Attributes.id "tabs-container")
        ]
        [ Ui.Atom.Tabs.view
            { label = "Data"
            , tabs =
                [ { tag = ObjectsTab
                  , id = "objects"
                  , label = "Objects"
                  }
                , { tag = VariablesTab
                  , id = "variables"
                  , label = "Variables"
                  }
                , { tag = GroupsTab
                  , id = "groups"
                  , label = "Groups"
                  }
                ]
            , selected = model.selectedTab
            , content =
                \tab ->
                    Element.el
                        [ Element.height (Element.px 120)
                        , Element.width Element.fill
                        , Border.width Ui.Theme.Spacing.level1
                        , Border.color Ui.Theme.Color.secondary
                        ]
                        (Element.el
                            [ Element.centerX
                            , Element.centerY
                            ]
                            (Ui.Theme.Typography.body <|
                                case tab of
                                    ObjectsTab ->
                                        "Objects"

                                    VariablesTab ->
                                        "Variables"

                                    GroupsTab ->
                                        "Groups"
                            )
                        )
            , onSelect = SelectedTab
            }
        ]



-- JOINED FORM ELEMENTS


viewJoinedFormElements : Model -> Element Msg
viewJoinedFormElements model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Segment Control + Dropdown"
        , Element.column
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.spacing Ui.Theme.Spacing.level4
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.segmentControl
                { id = "position-segment-control"
                , label = Just "Position"
                , help = Nothing
                , onChange = ChangedPositionAppended
                , options = positions
                , selected = model.positionAppended
                , child =
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = fruitToString
                            , entryToHash = fruitToString
                            }
                            { id = "fruit-dropdown-appended"
                            , lift = DropdownAppendedMsg
                            , label = "Fruit"
                            }
                            fruits
                            model.dropdownAppended
                            model.selectionAppended
                }
            , Ui.Atom.Input.segmentControl
                { id = "position-segment-control"
                , label = Just "Position"
                , help = Just "Help message"
                , onChange = ChangedPositionAppended
                , options = positions
                , selected = model.positionAppended
                , child =
                    Just <|
                        Ui.Atom.Dropdown.viewAppended
                            { entryToString = fruitToString
                            , entryToHash = fruitToString
                            }
                            { id = "fruit-dropdown-appended"
                            , lift = DropdownAppendedMsg
                            , label = "Fruit"
                            }
                            fruits
                            model.dropdownAppended
                            model.selectionAppended
                }
            ]
        , Ui.Theme.Typography.headingThree "Segment Control + Text"
        , Element.column
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.spacing Ui.Theme.Spacing.level4
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.segmentControl
                { id = "position-text-segment-control"
                , label = Just "Position"
                , help = Nothing
                , onChange = ChangedPositionAppended
                , options = positions
                , selected = model.positionAppended
                , child =
                    Just <|
                        Ui.Atom.Input.textAppended "text-appended"
                            { onChange = \_ -> NoOp
                            , text = ""
                            , label = "Text"
                            }
                }
            , Ui.Atom.Input.segmentControl
                { id = "position-formula-segment-control"
                , label = Just "Position"
                , help = Nothing
                , onChange = ChangedPositionAppended
                , options = positions
                , selected = model.positionAppended
                , child =
                    Just <|
                        Ui.Atom.Input.formulaAppended "formula-appended"
                            { onChange = ChangedFormula
                            , text = model.formula
                            , label = "Formula"
                            }
                }
            , Ui.Atom.Input.segmentControl
                { id = "position-formula-segment-control"
                , label = Just "Position"
                , help = Just "Help message"
                , onChange = ChangedPositionAppended
                , options = positions
                , selected = model.positionAppended
                , child =
                    Just <|
                        Ui.Atom.Input.formulaAppended "formula-appended"
                            { onChange = ChangedFormula
                            , text = model.formula
                            , label = "Formula"
                            }
                }
            ]
        , Ui.Theme.Typography.headingThree "Segment Control + Nested Form"
        , Element.column
            [ Element.padding Ui.Theme.Spacing.level1
            , Element.spacing Ui.Theme.Spacing.level4
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.segmentControl
                { id = "position-nested-formula-segment-control"
                , label = Just "Position"
                , help = Nothing
                , onChange = ChangedPositionNested
                , options = positions
                , selected = model.positionNested
                , child =
                    case model.positionNested of
                        Left ->
                            Just <|
                                Ui.Atom.Input.formulaAppended "formula-appended"
                                    { onChange = ChangedFormula
                                    , text = model.formula
                                    , label = "Formula"
                                    }

                        Center ->
                            Just <|
                                Ui.Atom.Input.nestedHideable
                                    { show = model.showFormula
                                    , onPress = ClickedShowFormula
                                    , shown =
                                        Element.column
                                            [ Element.spacing Ui.Theme.Spacing.level4
                                            , Element.width Element.fill
                                            ]
                                            [ Ui.Atom.Input.formula
                                                { id = "input-formula"
                                                , onChange = ChangedFormula
                                                , text = model.formula
                                                , label = "Formula"
                                                , help = Nothing
                                                }
                                            ]
                                    , hidden = Element.none
                                    }

                        Right ->
                            Nothing
                }
            , Ui.Atom.Input.segmentControl
                { id = "position-nested-formula-segment-control"
                , label = Just "Position"
                , help = Just "Help message"
                , onChange = ChangedPositionNested
                , options = positions
                , selected = model.positionNested
                , child =
                    case model.positionNested of
                        Left ->
                            Just <|
                                Ui.Atom.Input.formulaAppended "formula-appended"
                                    { onChange = ChangedFormula
                                    , text = model.formula
                                    , label = "Formula"
                                    }

                        Center ->
                            Just <|
                                Ui.Atom.Input.nestedHideable
                                    { show = model.showFormula
                                    , onPress = ClickedShowFormula
                                    , shown =
                                        Element.column
                                            [ Element.spacing Ui.Theme.Spacing.level4
                                            , Element.width Element.fill
                                            ]
                                            [ Ui.Atom.Input.formula
                                                { id = "input-formula"
                                                , onChange = ChangedFormula
                                                , text = model.formula
                                                , label = "Formula"
                                                , help = Nothing
                                                }
                                            ]
                                    , hidden = Element.none
                                    }

                        Right ->
                            Nothing
                }
            ]
        ]



-- DROPDOWNS


viewDropdowns : Model -> Element Msg
viewDropdowns model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        ]
        [ Ui.Theme.Typography.headingThree "Action Button + Dropdown"
        , Element.wrappedRow
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ Ui.Molecule.MenuBtn.viewPrimary
                { id = "create-btn"
                , onMsg = MenuBtnPrimaryMsg
                , actions =
                    [ { icon = Ui.Atom.Icon.point
                      , label = "Create a point"
                      , action = CreatePoint
                      }
                    , { icon = Ui.Atom.Icon.axis
                      , label = "Create an axis"
                      , action = CreateAxis
                      }
                    , { icon = Ui.Atom.Icon.circle
                      , label = "Create a circle"
                      , action = CreateCircle
                      }
                    , { icon = Ui.Atom.Icon.curve
                      , label = "Create a curve"
                      , action = CreateCurve
                      }
                    , { icon = Ui.Atom.Icon.detail
                      , label = "Create a detail"
                      , action = CreateDetail
                      }
                    ]
                }
                model.menuBtnPrimary
            , Ui.Molecule.MenuBtn.viewSecondary
                { id = "create-btn"
                , onMsg = MenuBtnSecondaryMsg
                , actions =
                    [ { icon = Ui.Atom.Icon.point
                      , label = "Create a point"
                      , action = CreatePoint
                      }
                    , { icon = Ui.Atom.Icon.axis
                      , label = "Create an axis"
                      , action = CreateAxis
                      }
                    , { icon = Ui.Atom.Icon.circle
                      , label = "Create a circle"
                      , action = CreateCircle
                      }
                    , { icon = Ui.Atom.Icon.curve
                      , label = "Create a curve"
                      , action = CreateCurve
                      }
                    , { icon = Ui.Atom.Icon.detail
                      , label = "Create a detail"
                      , action = CreateDetail
                      }
                    ]
                }
                model.menuBtnSecondary
            ]
        ]



---- OBJECT LIST


viewObjectList : Model -> Element Msg
viewObjectList model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Ui.Molecule.ObjectList.view
            { toMsg = ObjectListMsg
            , hidePressed = \_ -> NoOp
            , editPressed = \_ -> NoOp
            , removePressed = \_ -> NoOp
            }
            storedPattern.pattern
            model.patternState
        ]



---- PATTERN LIST


viewPatternList : Model -> Element Msg
viewPatternList model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level8
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Ui.Molecule.PatternList.view
            { search = model.search
            , onSearchChange = UserChangedSearch
            , onImport = NoOp
            , onCreate = NoOp
            , patternInfos =
                [ { name = "Bodice Block, Leena's Patternmaker"
                  , description = "A simple bodice block taken from the Seamly2d documentation."
                  , storage = Ui.Molecule.PatternList.Github "kirchner" "bodice-block"
                  , updatedAt = Time.millisToPosix 1000000
                  , onClone = NoOp
                  }
                , { name = "Trousers"
                  , description = "Some pair of trousers."
                  , storage = Ui.Molecule.PatternList.LocalStorage "trousers"
                  , updatedAt = Time.millisToPosix 1200000
                  , onClone = NoOp
                  }
                ]
            , now = Time.millisToPosix 1300000
            }
        , Ui.Molecule.PatternList.view
            { search = model.search
            , onSearchChange = UserChangedSearch
            , onImport = NoOp
            , onCreate = NoOp
            , patternInfos = []
            , now = Time.millisToPosix 1300000
            }
        ]



---- PATTERN LIST


viewCreatePattern : Model -> Element Msg
viewCreatePattern model =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level8
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Ui.Organism.CreatePattern.view
            { name = ""
            , onNameChange = \_ -> NoOp
            , description = ""
            , onDescriptionChange = \_ -> NoOp
            , storageTag = model.storageTag
            , onStorageChange = UserChangedStorageTag
            , onCreate = NoOp
            , slug = ""
            , onSlugChange = \_ -> NoOp
            , owner = ""
            , onOwnerChange = \_ -> NoOp
            , repositoryName = ""
            , onRepositoryNameChange = \_ -> NoOp
            }
        ]



---- UPDATE


type Msg
    = NoOp
    | ClickedLink UrlRequest
    | ChangedUrl Url
    | ResizedBrowser Int Int
    | ClickedNavigationMenu
    | CheckedCheckbox Bool
    | FruitChanged Fruit
    | DropdownMsg (Ui.Atom.Dropdown.Msg Fruit)
    | DropdownAppendedMsg (Ui.Atom.Dropdown.Msg Fruit)
    | SelectedTab Tab String
    | ChangedPosition Position
    | ChangedPositionAppended Position
    | ChangedPositionNested Position
    | ChangedFormula String
    | ClickedShowFormula
    | RenderedPage
    | GotViewportOfObjectsContainer (Result Browser.Dom.Error Browser.Dom.Viewport)
      -- OBJECTS
    | ObjectListMsg Ui.Molecule.ObjectList.Msg
    | PatternMsg Ui.Molecule.Pattern.Msg
      -- MENU BUTTON
    | MenuBtnPrimaryMsg (Ui.Molecule.MenuBtn.Msg CreateAction)
    | MenuBtnSecondaryMsg (Ui.Molecule.MenuBtn.Msg CreateAction)
      -- PATTERN LIST
    | UserChangedSearch String
      -- CREATE PATTERN
    | UserChangedStorageTag Ui.Organism.CreatePattern.StorageTag


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedLink (Internal url) ->
            changeRouteTo (routeFromUrl url) model

        ClickedLink (External href) ->
            ( model, Browser.Navigation.load href )

        ChangedUrl url ->
            changeRouteTo (routeFromUrl url) model

        ResizedBrowser width height ->
            let
                { class, orientation } =
                    Element.classifyDevice
                        { width = width
                        , height = height
                        }
            in
            ( if class /= model.deviceClass || orientation /= model.orientation then
                { model
                    | deviceClass = class
                    , orientation = orientation
                    , showNavigation =
                        if class == Phone then
                            False

                        else
                            model.showNavigation
                }

              else
                model
            , Browser.Dom.getViewportOf "objects-container"
                |> Task.attempt GotViewportOfObjectsContainer
            )

        ClickedNavigationMenu ->
            ( { model | showNavigation = not model.showNavigation }, Cmd.none )

        CheckedCheckbox checked ->
            ( { model | checked = checked }, Cmd.none )

        FruitChanged fruit ->
            ( { model | fruit = Just fruit }, Cmd.none )

        DropdownMsg dropdownMsg ->
            let
                ( newDropdown, dropdownCmd, newSelection ) =
                    Ui.Atom.Dropdown.update
                        { entryToHash = fruitToString }
                        fruits
                        dropdownMsg
                        model.dropdown
                        model.selection
            in
            ( { model
                | dropdown = newDropdown
                , selection = newSelection
              }
            , Cmd.map DropdownMsg dropdownCmd
            )

        DropdownAppendedMsg dropdownMsg ->
            let
                ( newDropdown, dropdownCmd, newSelection ) =
                    Ui.Atom.Dropdown.update
                        { entryToHash = fruitToString }
                        fruits
                        dropdownMsg
                        model.dropdownAppended
                        model.selectionAppended
            in
            ( { model
                | dropdownAppended = newDropdown
                , selectionAppended = newSelection
              }
            , Cmd.map DropdownAppendedMsg dropdownCmd
            )

        SelectedTab tab id ->
            ( { model | selectedTab = tab }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)
            )

        ChangedPosition position ->
            ( { model | position = position }, Cmd.none )

        ChangedPositionAppended position ->
            ( { model | positionAppended = position }, Cmd.none )

        ChangedPositionNested position ->
            ( { model | positionNested = position }, Cmd.none )

        ChangedFormula formula ->
            ( { model | formula = formula }, Cmd.none )

        ClickedShowFormula ->
            ( { model | showFormula = not model.showFormula }, Cmd.none )

        RenderedPage ->
            ( model
            , if model.route == Objects then
                Browser.Dom.getViewportOf "objects-container"
                    |> Task.attempt GotViewportOfObjectsContainer

              else
                Cmd.none
            )

        GotViewportOfObjectsContainer (Err _) ->
            ( model, Cmd.none )

        GotViewportOfObjectsContainer (Ok { viewport }) ->
            ( { model | objectsContainerWidth = Just viewport.width }
            , Cmd.none
            )

        -- OBJECTS
        ObjectListMsg objectListMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.ObjectList.update objectListMsg
                        storedPattern.pattern
                        model.patternState
              }
            , Cmd.none
            )

        PatternMsg patternMsg ->
            ( { model
                | patternState =
                    Ui.Molecule.Pattern.update patternMsg
                        storedPattern.pattern
                        model.patternState
              }
            , Cmd.none
            )

        -- MENU BUTTON
        MenuBtnPrimaryMsg menuBtnMsg ->
            let
                ( newMenuBtn, menuBtnCmd, _ ) =
                    Ui.Molecule.MenuBtn.update menuBtnMsg model.menuBtnPrimary
            in
            ( { model | menuBtnPrimary = newMenuBtn }
            , Cmd.map MenuBtnPrimaryMsg menuBtnCmd
            )

        MenuBtnSecondaryMsg menuBtnMsg ->
            let
                ( newMenuBtn, menuBtnCmd, _ ) =
                    Ui.Molecule.MenuBtn.update menuBtnMsg model.menuBtnSecondary
            in
            ( { model | menuBtnSecondary = newMenuBtn }
            , Cmd.map MenuBtnSecondaryMsg menuBtnCmd
            )

        -- PATTERN LIST
        UserChangedSearch newSearch ->
            ( { model | search = newSearch }
            , Cmd.none
            )

        -- CREATE PATTERN
        UserChangedStorageTag newStorageTag ->
            ( { model | storageTag = newStorageTag }
            , Cmd.none
            )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just route ->
            ( { model
                | route = route
                , showNavigation = False
              }
            , Cmd.none
            )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizedBrowser
        , Sub.map DropdownMsg (Ui.Atom.Dropdown.subscriptions model.dropdown)
        , Sub.map DropdownAppendedMsg (Ui.Atom.Dropdown.subscriptions model.dropdownAppended)
        , if model.objectsContainerWidth == Nothing && model.route == Objects then
            Browser.Events.onAnimationFrame (\_ -> RenderedPage)

          else
            Sub.none
        ]
