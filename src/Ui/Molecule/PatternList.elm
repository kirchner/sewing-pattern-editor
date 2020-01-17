module Ui.Molecule.PatternList exposing (PatternInfo, Storage(..), view)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Time exposing (Posix)
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Focus
import Ui.Theme.Spacing
import Ui.Theme.Typography


type alias PatternInfo msg =
    { name : String
    , description : String
    , storage : Storage
    , updatedAt : Posix
    , onClone : msg
    }


type Storage
    = LocalStorage String
    | Github String String


type alias Config msg =
    { search : String
    , onSearchChange : String -> msg
    , onImport : msg
    , onCreate : msg
    , patternInfos : List (PatternInfo msg)
    , now : Posix
    }


view : Config msg -> Element msg
view { search, onSearchChange, onCreate, patternInfos, now } =
    if List.isEmpty patternInfos then
        Element.column
            [ Element.spacing Ui.Theme.Spacing.level4
            , Element.width Element.fill
            , Element.padding Ui.Theme.Spacing.level8
            ]
            [ Element.el
                [ Element.centerX ]
                (Ui.Theme.Typography.bodyBold "We don't know about any patterns, yet.")
            , Element.row
                [ Element.centerX ]
                [ -- Ui.Atom.Input.btnSecondary
                  --   { id = "import-pattern-btn"
                  --   , onPress = Just onImport
                  --   , label = "Import a pattern"
                  --   } ,
                  Ui.Atom.Input.btnPrimary
                    { id = "new-pattern-btn"
                    , onPress = Just onCreate
                    , label = "Create a new pattern"
                    }
                ]
            ]

    else
        Element.column
            [ Element.width Element.fill
            , Element.spacing Ui.Theme.Spacing.level7
            , Element.padding Ui.Theme.Spacing.level1
            ]
            [ Element.wrappedRow
                [ Element.width Element.fill ]
                [ Element.el [ Element.width Element.fill ] <|
                    Ui.Atom.Input.text
                        { id = "pattern-filter-input"
                        , onChange = onSearchChange
                        , text = search
                        , label = "Filter by name"
                        , help = Nothing
                        }
                , Element.row
                    [ Element.alignBottom ]
                    [ --Element.el [] <|
                      --  Ui.Atom.Input.btnSecondary
                      --      { id = "import-pattern-btn"
                      --      , onPress = Just onImport
                      --      , label = "Import a pattern"
                      --      } ,
                      Element.el
                        [ Element.alignRight ]
                        (Ui.Atom.Input.btnPrimary
                            { id = "new-pattern-btn"
                            , onPress = Just onCreate
                            , label = "Create a new pattern"
                            }
                        )
                    ]
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.spacing Ui.Theme.Spacing.level6
                ]
                (List.intersperse horizontalRule <|
                    List.map (viewPattern now) patternInfos
                )
            ]


horizontalRule : Element msg
horizontalRule =
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 7 0
        ]
        (Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 1)
            , Background.color Ui.Theme.Color.secondaryDark
            ]
            Element.none
        )


viewPattern : Posix -> PatternInfo msg -> Element msg
viewPattern _ { name, description, storage } =
    let
        --uuid =
        --    case storage of
        --        LocalStorage slug ->
        --            "local-storage-" ++ slug
        --        Github owner repo ->
        --            "github-" ++ owner ++ "-" ++ repo
        url =
            case storage of
                LocalStorage slug ->
                    "/browser/" ++ slug

                Github owner repo ->
                    "/github/" ++ owner ++ "/" ++ repo
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Ui.Theme.Spacing.level1
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.width Element.shrink ]
                (Ui.Theme.Focus.outline <|
                    Element.link
                        [ Font.color Ui.Theme.Color.primary
                        , Element.mouseOver
                            [ Font.color Ui.Theme.Color.primaryDark
                            ]
                        ]
                        { url = url
                        , label = Ui.Theme.Typography.bodyBold name
                        }
                )
            , Element.el
                [ Element.alignRight
                , Element.padding 7
                ]
                (viewStorage storage)
            ]
        , Element.el [ Element.padding 7 ]
            (Ui.Theme.Typography.body description)

        --, Element.row
        --    [ Element.width Element.fill ]
        --    [ Element.el [ Element.padding 7 ] <|
        --        Ui.Theme.Typography.button ("Updated " ++ Time.Distance.inWords updatedAt now)
        --    , Element.el [ Element.alignRight ]
        --        (Ui.Atom.Input.btnSecondary
        --            { id = "clone-" ++ uuid ++ "-btn"
        --            , onPress = Just onClone
        --            , label = "Clone"
        --            }
        --        )
        --    ]
        ]


viewStorage : Storage -> Element msg
viewStorage storage =
    let
        segments =
            List.intersperse "/" <|
                case storage of
                    LocalStorage slug ->
                        [ "browser"
                        , slug
                        ]

                    Github owner repo ->
                        [ "github"
                        , owner
                        , repo
                        ]
    in
    Element.row
        [ Element.spacing Ui.Theme.Spacing.level1
        , Font.color Ui.Theme.Color.grayDark
        ]
        (List.map Ui.Theme.Typography.button segments)
