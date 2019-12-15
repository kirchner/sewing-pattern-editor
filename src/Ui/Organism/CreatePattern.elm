module Ui.Organism.CreatePattern exposing (Config, StorageTag(..), view)

import Element exposing (Element)
import Element.Font as Font
import Ui.Atom.Input
import Ui.Theme.Color
import Ui.Theme.Spacing
import Ui.Theme.Typography


type alias Config msg =
    { name : String
    , onNameChange : String -> msg
    , description : String
    , onDescriptionChange : String -> msg
    , onCreate : msg
    , storageTag : StorageTag
    , onStorageChange : StorageTag -> msg
    , slug : String
    , onSlugChange : String -> msg
    , owner : String
    , onOwnerChange : String -> msg
    , repositoryName : String
    , onRepositoryNameChange : String -> msg
    }


type StorageTag
    = BrowserTag
    | GithubTag


view : Config msg -> Element msg
view cfg =
    Element.column
        [ Element.spacing Ui.Theme.Spacing.level4 ]
        [ Element.el [ Element.padding 7 ] <|
            Ui.Theme.Typography.paragraphBody
                [ Element.text
                    """A pattern contains all the information on how all details
                needed for a certain piece of clothing can be constructed from
                a set of body measurements. If you already have a pattern you
                can also """
                , Element.link
                    [ Font.color Ui.Theme.Color.primary
                    , Element.mouseOver
                        [ Font.color Ui.Theme.Color.primaryDark
                        ]
                    ]
                    { url = "/import"
                    , label = Element.text "import it"
                    }
                , Element.text "."
                ]
        , Element.column
            [ Element.spacing Ui.Theme.Spacing.level2
            , Element.width Element.fill
            ]
            [ Ui.Atom.Input.text
                { id = "pattern-name-input"
                , text = cfg.name
                , onChange = cfg.onNameChange
                , label = "Name"
                , help = Nothing
                }
            , Ui.Atom.Input.text
                { id = "pattern-decription-input"
                , text = cfg.description
                , onChange = cfg.onDescriptionChange
                , label = "Description (optional)"
                , help = Nothing
                }
            ]
        , Ui.Atom.Input.radioColumn
            { id = "storage-radio-group"
            , onChange = cfg.onStorageChange
            , options =
                [ Ui.Atom.Input.option
                    { value = BrowserTag
                    , label = "Browser cache"
                    , child =
                        Ui.Theme.Typography.paragraphButton
                            [ Element.text
                                """Store all data locally in the cache of the
                                browser you are currently using. Note, that you
                                will not have access to this pattern from
                                a different browser or machine. You will loose
                                all data when you clear the browser cache."""
                            ]
                    }
                , Ui.Atom.Input.option
                    { value = GithubTag
                    , label = "GitHub repository"
                    , child =
                        Ui.Theme.Typography.paragraphButton
                            [ Element.text
                                """Store all data within a GitHub repository.
                                You will need to log in with your GitHub
                                account and authorize the sewing pattern editor
                                to create and modify repositories within your
                                GitHub account."""
                            ]
                    }
                ]
            , selected = Just cfg.storageTag
            , label = "Storage solution"
            }
        , case cfg.storageTag of
            BrowserTag ->
                Ui.Atom.Input.text
                    { id = "localstorage-slug-input"
                    , text = cfg.slug
                    , onChange = cfg.onSlugChange
                    , label = "Slug"
                    , help = Nothing
                    }

            GithubTag ->
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing Ui.Theme.Spacing.level1
                    ]
                    [ Element.el
                        [ Element.width
                            (Element.fill
                                |> Element.maximum 240
                            )
                        ]
                        (Ui.Atom.Input.text
                            { id = "github-owner-input"
                            , text = cfg.owner
                            , onChange = cfg.onOwnerChange
                            , label = "Owner"
                            , help = Nothing
                            }
                        )
                    , Element.el
                        [ Element.alignBottom
                        , Element.paddingXY 0 17
                        ]
                        (Ui.Theme.Typography.body "/")
                    , Ui.Atom.Input.text
                        { id = "github-repository-name-input"
                        , text = cfg.repositoryName
                        , onChange = cfg.onRepositoryNameChange
                        , label = "Repository name"
                        , help = Nothing
                        }
                    ]
        , Element.el [ Element.width Element.shrink ] <|
            Ui.Atom.Input.btnPrimary
                { id = "create-pattern-btn"
                , onPress = Just cfg.onCreate
                , label = "Create pattern"
                }
        ]
