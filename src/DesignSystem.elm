module DesignSystem exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
import Browser.Navigation exposing (Key)
import Design
import Element exposing (DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Element
import Html exposing (Html)
import Html.Attributes
import Ui.Atom
import Ui.Atom.Dropdown exposing (Dropdown)
import Ui.Color
import Ui.Space
import Ui.Typography
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
    , selection : Maybe Fruit
    , position : Position
    , formula : String
    }


type Fruit
    = Apple
    | Banana
    | Orange


fruits : List Fruit
fruits =
    [ Apple
    , Banana
    , Orange
    ]


fruitToString : Fruit -> String
fruitToString fruit =
    case fruit of
        Apple ->
            "Apple"

        Banana ->
            "Banana"

        Orange ->
            "Orange"


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


type Route
    = -- FOUNDATIONS
      Typography
    | Color
    | Space
      -- ATOMS
    | Buttons
    | FormElements
    | Icons


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
        , position = Left
        , formula = ""
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
                [ Font.external
                    { name = "Rubik"
                    , url = "https://fonts.googleapis.com/css?family=Rubik:300"
                    }
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
                        , Element.paddingXY Ui.Space.level4 Ui.Space.level8
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
                , Element.paddingXY Ui.Space.level4 Ui.Space.level8
                , Element.height Element.fill
                , Element.centerX
                , Element.scrollbarY
                , Element.spacing Ui.Space.level5
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
        , Border.color Ui.Color.secondaryDark
        ]
        (Input.button
            [ Element.mouseOver
                [ Font.color Ui.Color.primaryDark ]
            , Element.padding Ui.Space.level3
            , Element.htmlAttribute <|
                Html.Attributes.class "navigation-menu"
            ]
            { onPress = Just ClickedNavigationMenu
            , label =
                Ui.Atom.faLarge <|
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
                [ Element.spacing Ui.Space.level3
                , Element.width Element.fill
                ]
                [ Element.el
                    [ Font.variant Font.smallCaps
                    , Font.size fontSize
                    , Font.bold
                    , Element.padding Ui.Space.level2
                    ]
                    (Element.text title)
                , Element.column
                    [ Element.spacing Ui.Space.level1
                    , Element.width Element.fill
                    ]
                    links
                ]

        link : Route -> Element msg
        link route =
            Element.link
                (if currentRoute == route then
                    [ Element.width Element.fill
                    , Background.color Ui.Color.primaryLight
                    ]

                 else
                    [ Element.width Element.fill
                    , Element.mouseOver
                        [ Background.color Ui.Color.primaryBright ]
                    ]
                )
                { url = routeToUrl route
                , label =
                    Element.el
                        [ Element.padding Ui.Space.level2
                        , Font.size fontSize
                        ]
                        (Element.text (routeToTitle route))
                }

        fontSize =
            case deviceClass of
                Phone ->
                    28

                _ ->
                    16

        width =
            case deviceClass of
                Phone ->
                    Element.fill

                _ ->
                    Element.px (5 * Ui.Space.level8)
    in
    Element.column
        [ Element.width width
        , Element.height Element.fill
        , Element.padding Ui.Space.level3
        , Element.spacing Ui.Space.level3
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
            ]
        ]


content : Model -> Element Msg
content model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Space.level8
        , Element.alignTop
        ]
        [ Ui.Typography.headingOne (routeToTitle model.route)
        , case model.route of
            Typography ->
                Element.column
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ Ui.Typography.headingThree "Type Scale"
                    , Element.column
                        [ Element.paddingXY Ui.Space.level5 Ui.Space.level4
                        , Element.spacing Ui.Space.level4
                        ]
                        [ Ui.Typography.headingOne "Heading One"
                        , Ui.Typography.headingTwo "Heading Two"
                        , Ui.Typography.headingThree "Heading Three"
                        , Ui.Typography.headingFour "Heading Four"
                        , Ui.Typography.bodyBold "Body Bold"
                        , Ui.Typography.body "Body"
                        , Ui.Typography.button "Button"
                        ]
                    ]

            Color ->
                let
                    colorBox backgroundColor fontColor name =
                        Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px (2 * Ui.Space.level8))
                            , Border.width 1
                            , Border.color Ui.Color.grayDark
                            , Background.color backgroundColor
                            , Font.color fontColor
                            ]
                            (Element.el [ Element.padding Ui.Space.level2 ]
                                (Ui.Typography.bodyBold name)
                            )
                in
                Element.column
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ colorBox Ui.Color.primaryBright Ui.Color.black "Primary Bright"
                    , colorBox Ui.Color.primaryLight Ui.Color.black "Primary Light"
                    , colorBox Ui.Color.primary Ui.Color.white "Primary"
                    , colorBox Ui.Color.primaryDark Ui.Color.white "Primary Dark"
                    , colorBox Ui.Color.secondary Ui.Color.black "Secondary"
                    , colorBox Ui.Color.secondaryDark Ui.Color.black "Secondary Dark"
                    , colorBox Ui.Color.complementary Ui.Color.white "Complementary"
                    , colorBox Ui.Color.complementaryDark Ui.Color.white "Complementary Dark"
                    , colorBox Ui.Color.neutral Ui.Color.black "Neutral"
                    , colorBox Ui.Color.neutralDark Ui.Color.black "Neutral Dark"
                    , colorBox Ui.Color.danger Ui.Color.white "Danger"
                    , colorBox Ui.Color.dangerDark Ui.Color.white "Danger Dark"
                    , colorBox Ui.Color.success Ui.Color.black "Success"
                    , colorBox Ui.Color.white Ui.Color.black "White"
                    , colorBox Ui.Color.black Ui.Color.white "Black"
                    , colorBox Ui.Color.grayDark Ui.Color.white "Gray Dark"
                    ]

            Space ->
                let
                    spaceBox space name =
                        Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px (3 * Ui.Space.level8))
                            , Background.color Ui.Color.secondaryDark
                            , Element.padding space
                            ]
                            (Element.el
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                , Background.color Ui.Color.secondary
                                ]
                                (Element.el
                                    [ Element.centerX
                                    , Element.centerY
                                    ]
                                    (Ui.Typography.bodyBold
                                        (name ++ ": " ++ String.fromInt space ++ "px")
                                    )
                                )
                            )
                in
                Element.wrappedRow
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ Element.column
                        [ Element.spacing Ui.Space.level4
                        , Element.width
                            (Element.fill
                                |> Element.minimum (4 * Ui.Space.level8)
                            )
                        ]
                        [ spaceBox Ui.Space.level1 "Level 1"
                        , spaceBox Ui.Space.level2 "Level 2"
                        , spaceBox Ui.Space.level3 "Level 3"
                        , spaceBox Ui.Space.level4 "Level 4"
                        ]
                    , Element.column
                        [ Element.spacing Ui.Space.level4
                        , Element.width
                            (Element.fill
                                |> Element.minimum (4 * Ui.Space.level8)
                            )
                        ]
                        [ spaceBox Ui.Space.level5 "Level 5"
                        , spaceBox Ui.Space.level6 "Level 6"
                        , spaceBox Ui.Space.level7 "Level 7"
                        , spaceBox Ui.Space.level8 "Level 8"
                        ]
                    ]

            Buttons ->
                Element.column
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ Ui.Typography.headingThree "Standard"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level4
                        , Element.padding Ui.Space.level4
                        ]
                        [ Ui.Atom.btnPrimary
                            { onPress = Nothing
                            , label = "Primary"
                            }
                        , Ui.Atom.btnSecondary "secondary-button"
                            { onPress = Nothing
                            , label = "Secondary"
                            }
                        , Ui.Atom.btnDanger
                            { onPress = Nothing
                            , label = "Danger"
                            }
                        , Ui.Atom.btnCancel
                            { onPress = Nothing
                            , label = "Cancel"
                            }
                        ]
                    , Ui.Typography.headingThree "Call to Action"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level4
                        , Element.padding Ui.Space.level4
                        ]
                        [ Ui.Atom.btnCallToAction "call-to-action-button"
                            { onPress = Nothing
                            , label = "Call to Action"
                            }
                        ]
                    , Ui.Typography.headingThree "Icon"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level4
                        , Element.padding Ui.Space.level4
                        ]
                        [ Ui.Atom.btnIcon
                            { onPress = Nothing
                            , icon = "thumbs-up"
                            }
                        , Ui.Atom.btnIconDanger
                            { onPress = Nothing
                            , icon = "thumbs-down"
                            }
                        , Ui.Atom.btnIconLarge
                            { onPress = Nothing
                            , icon = "thumbs-up"
                            }
                        ]
                    ]

            FormElements ->
                Element.column
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ Ui.Typography.headingThree "Checkbox"
                    , Element.el
                        [ Element.padding Ui.Space.level4
                        , Element.width Element.fill
                        ]
                        (Ui.Atom.checkbox
                            { onChange = CheckedCheckbox
                            , checked = model.checked
                            , label = "Checkbox"
                            }
                        )
                    , Ui.Typography.headingThree "Radio Buttons"
                    , Element.el
                        [ Element.padding Ui.Space.level4
                        , Element.width Element.fill
                        ]
                        (Ui.Atom.radioColumn "radio-column"
                            { onChange = FruitChanged
                            , options =
                                [ Ui.Atom.option Apple "Apple"
                                , Ui.Atom.option Banana "Banana"
                                , Ui.Atom.option Orange "Orange"
                                ]
                            , selected = model.fruit
                            , label = "Select a fruit"
                            }
                        )
                    , Ui.Typography.headingThree "Dropdown"
                    , Element.el
                        [ Element.padding Ui.Space.level4
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
                    , Ui.Typography.headingThree "Segment Control"
                    , Element.el
                        [ Element.padding Ui.Space.level4
                        , Element.width Element.fill
                        ]
                        (Ui.Atom.segmentControl
                            { onChange = ChangedPosition
                            , options = positions
                            , selected = model.position
                            , elementAppended = False
                            }
                        )
                    , Ui.Typography.headingThree "Text"
                    , Element.column
                        [ Element.padding Ui.Space.level4
                        , Element.spacing Ui.Space.level4
                        , Element.width Element.fill
                        ]
                        [ Ui.Atom.inputText "input-text"
                            { onChange = \_ -> NoOp
                            , text = ""
                            , label = "Text"
                            , help = Nothing
                            }
                        , Ui.Atom.inputFormula "input-formula"
                            { onChange = ChangedFormula
                            , text = model.formula
                            , label = "Formula"
                            , help = Nothing
                            }
                        ]
                    ]

            Icons ->
                Element.column
                    [ Element.spacing Ui.Space.level4
                    , Element.width Element.fill
                    ]
                    [ Ui.Typography.headingThree "Normal"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level2
                        , Element.padding Ui.Space.level2
                        ]
                        [ Ui.Atom.fa "check"
                        , Ui.Atom.fa "exclamation"
                        , Ui.Atom.fa "check-circle"
                        , Ui.Atom.fa "exclamation-circle"
                        , Ui.Atom.fa "chevron-up"
                        , Ui.Atom.fa "chevron-down"
                        ]
                    , Ui.Typography.headingThree "Body"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level3
                        , Element.padding Ui.Space.level2
                        ]
                        [ Ui.Atom.faBody "check"
                        , Ui.Atom.faBody "exclamation"
                        , Ui.Atom.faBody "check-circle"
                        , Ui.Atom.faBody "exclamation-circle"
                        , Ui.Atom.faBody "chevron-up"
                        , Ui.Atom.faBody "chevron-down"
                        ]
                    , Ui.Typography.headingThree "Large"
                    , Element.wrappedRow
                        [ Element.spacing Ui.Space.level4
                        , Element.padding Ui.Space.level2
                        ]
                        [ Ui.Atom.faLarge "check"
                        , Ui.Atom.faLarge "exclamation"
                        , Ui.Atom.faLarge "check-circle"
                        , Ui.Atom.faLarge "exclamation-circle"
                        , Ui.Atom.faLarge "chevron-up"
                        , Ui.Atom.faLarge "chevron-down"
                        ]
                    ]
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
    | ChangedPosition Position
    | ChangedFormula String


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
            if class /= model.deviceClass || orientation /= model.orientation then
                ( { model
                    | deviceClass = class
                    , orientation = orientation
                    , showNavigation =
                        if class == Phone then
                            False

                        else
                            model.showNavigation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

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

        ChangedPosition position ->
            ( { model | position = position }, Cmd.none )

        ChangedFormula formula ->
            ( { model | formula = formula }, Cmd.none )


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
        ]
