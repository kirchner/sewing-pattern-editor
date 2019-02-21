module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

{-
   Sewing pattern editor
   Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
--import Seamly2D.V0_6_0 as Seamly2D

import Api
import BoundingBox2d
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Color
import Design
import Dict exposing (Dict)
import Draw.Pattern as Pattern
import Element exposing (DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Element
import Element.Region as Region
import File exposing (File)
import File.Select
import Frame2d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import List.Nonempty as Nonempty
import Pattern exposing (Pattern)
import Ports
import Random.Pcg.Extended as Random
import RemoteData exposing (RemoteData(..), WebData)
import Route
import State
import StoredPattern exposing (StoredPattern)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy
import Task
import Those
import Uuid
import View.Icon
import View.Input
import View.Modal
import View.Navigation


type alias Model =
    { device : Element.Device
    , seed : Random.Seed
    , storedPatterns : WebData (List StoredPattern)
    , dialog : Dialog
    }


type Dialog
    = NoDialog
    | CreatePattern String
    | RenamePattern String String
    | DeletePattern String String
    | ImportPatterns
        { hover : Bool
        , previews : List Preview
        }


type alias Preview =
    { fileName : String
    , content : Result String StoredPattern
    }


init : ( Model, Cmd Msg )
init =
    ( { device =
            { class = Element.Desktop
            , orientation = Element.Landscape
            }
      , seed = Random.initialSeed 0 []
      , storedPatterns = Loading
      , dialog = NoDialog
      }
    , Cmd.batch
        [ Api.getPatterns (RemoteData.fromResult >> PatternsReceived)
        , Ports.requestSeed ()
        , Task.perform
            (\{ viewport } ->
                DeviceChanged <|
                    Element.classifyDevice
                        { width = Basics.floor viewport.width
                        , height = Basics.floor viewport.height
                        }
            )
            Browser.Dom.getViewport
        ]
    )


type Msg
    = NoOp
    | DeviceChanged Element.Device
    | SeedReceived Int (List Int)
    | PatternsReceived (WebData (List StoredPattern))
    | PatternCreateResponse (Result Http.Error ())
    | PatternCardClicked String
    | PatternCardMenuClicked String
      -- ADD PATTERN
    | AddPatternClicked
    | NewPatternNameChanged String
    | NewPatternCreateClicked
    | NewPatternCancelClicked
      -- RENAME PATTERN
    | RenamePatternPressed String
    | RenamePatternNameChanged String
    | RenamePatternRenameClicked
    | RenamePatternCancelClicked
    | PatternUpdateReceived (Result Http.Error ())
      -- DELETE PATTERN
    | DeletePatternPressed String String
    | DeletePatternDeleteClicked
    | DeletePatternCancelClicked
    | PatternDeleteResponse (Result Http.Error ())
      -- IMPORT PATTERNS
    | ImportPatternsPick
    | ImportPatternsDragEnter
    | ImportPatternsDragLeave
    | ImportPatternsGotFiles File (List File)
    | ImportPatternsGotPreview String String
    | ImportPatternsClicked
    | ImportPatternsCancelClicked
    | ImportPatternsImportClicked


update : String -> Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update prefix key msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DeviceChanged newDevice ->
            if model.device == newDevice then
                ( model, Cmd.none )

            else
                ( { model | device = newDevice }
                , Cmd.none
                )

        SeedReceived seed seedExtension ->
            ( { model | seed = Random.initialSeed seed seedExtension }
            , Cmd.none
            )

        PatternsReceived storedPatterns ->
            ( { model | storedPatterns = storedPatterns }
            , Cmd.none
            )

        PatternCreateResponse result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Api.getPatterns (RemoteData.fromResult >> PatternsReceived)
                    )

        PatternCardClicked slug ->
            ( model
            , Navigation.pushUrl key <|
                Route.toString prefix (Route.Editor slug Nothing)
            )

        PatternCardMenuClicked slug ->
            ( model, Cmd.none )

        -- ADD PATTERN
        AddPatternClicked ->
            ( { model | dialog = CreatePattern "" }
            , Browser.Dom.focus "name-input"
                |> Task.attempt (\_ -> NoOp)
            )

        NewPatternNameChanged newName ->
            case model.dialog of
                CreatePattern _ ->
                    ( { model | dialog = CreatePattern newName }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NewPatternCreateClicked ->
            case model.dialog of
                CreatePattern name ->
                    let
                        ( uuid, newSeed ) =
                            Random.step Uuid.generator model.seed
                    in
                    ( { model | dialog = NoDialog }
                    , Api.createPattern PatternCreateResponse <|
                        StoredPattern.init (Uuid.toString uuid) name
                    )

                _ ->
                    ( model, Cmd.none )

        NewPatternCancelClicked ->
            ( { model | dialog = NoDialog }
            , Cmd.none
            )

        -- RENAME PATTERN
        RenamePatternPressed slug ->
            case getPattern model slug of
                Nothing ->
                    ( model, Cmd.none )

                Just storedPattern ->
                    ( { model | dialog = RenamePattern slug storedPattern.name }
                    , Cmd.batch
                        [ Browser.Dom.focus "name-input"
                            |> Task.attempt (\_ -> NoOp)
                        , Ports.selectAllTextIn "name-input"
                        ]
                    )

        RenamePatternNameChanged newName ->
            case model.dialog of
                RenamePattern slug _ ->
                    ( { model | dialog = RenamePattern slug newName }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RenamePatternRenameClicked ->
            case model.dialog of
                RenamePattern slug newName ->
                    case getPattern model slug of
                        Just storedPattern ->
                            ( { model | dialog = NoDialog }
                            , Api.updatePattern PatternUpdateReceived
                                { storedPattern | name = newName }
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RenamePatternCancelClicked ->
            ( { model | dialog = NoDialog }
            , Cmd.none
            )

        PatternUpdateReceived result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Api.getPatterns (RemoteData.fromResult >> PatternsReceived)
                    )

        -- DELETE PATTERNS
        DeletePatternPressed slug name ->
            ( { model | dialog = DeletePattern slug name }
            , Cmd.none
            )

        DeletePatternDeleteClicked ->
            case model.dialog of
                DeletePattern slug name ->
                    ( { model | dialog = NoDialog }
                    , Api.deletePattern PatternDeleteResponse slug
                    )

                _ ->
                    ( model, Cmd.none )

        DeletePatternCancelClicked ->
            ( { model | dialog = NoDialog }, Cmd.none )

        PatternDeleteResponse result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Api.getPatterns (RemoteData.fromResult >> PatternsReceived)
                    )

        -- IMPORT PATTERNS
        ImportPatternsPick ->
            ( model
            , File.Select.files [] ImportPatternsGotFiles
            )

        ImportPatternsDragEnter ->
            case model.dialog of
                ImportPatterns data ->
                    ( { model | dialog = ImportPatterns { data | hover = True } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ImportPatternsDragLeave ->
            case model.dialog of
                ImportPatterns data ->
                    ( { model | dialog = ImportPatterns { data | hover = False } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ImportPatternsGotFiles firstFile files ->
            case model.dialog of
                ImportPatterns data ->
                    ( { model | dialog = ImportPatterns { data | hover = False } }
                    , Cmd.batch <|
                        List.map
                            (\file ->
                                Task.perform (ImportPatternsGotPreview (File.name file)) <|
                                    File.toString file
                            )
                            (firstFile :: files)
                    )

                _ ->
                    ( model, Cmd.none )

        ImportPatternsGotPreview name content ->
            case model.dialog of
                ImportPatterns data ->
                    let
                        newPreview =
                            { fileName = name
                            , content =
                                case Decode.decodeString StoredPattern.decoder content of
                                    Ok storedPattern ->
                                        Ok storedPattern

                                    Err _ ->
                                        --Seamly2D.decode content
                                        --    |> Result.toMaybe
                                        --    |> Maybe.map
                                        --        (\val ->
                                        --            let
                                        --                pattern =
                                        --                    Nonempty.head val.patterns
                                        --                storedPattern =
                                        --                    Maybe.withDefault "<no name>"
                                        --                        pattern.patternName
                                        --                        |> StoredPattern.init ""
                                        --            in
                                        --            { storedPattern
                                        --                | pattern =
                                        --                    Seamly2D.toPattern pattern
                                        --            }
                                        --        )
                                        --    |> Result.fromMaybe "This is not a valid Seamly2D file."
                                        Err "not implemented"
                            }
                    in
                    ( { model
                        | dialog =
                            ImportPatterns
                                { data | previews = newPreview :: data.previews }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ImportPatternsClicked ->
            ( { model
                | dialog =
                    ImportPatterns
                        { hover = False
                        , previews = []
                        }
              }
            , Cmd.none
            )

        ImportPatternsCancelClicked ->
            ( { model | dialog = NoDialog }
            , Cmd.none
            )

        ImportPatternsImportClicked ->
            case model.dialog of
                ImportPatterns { previews } ->
                    let
                        ( allCmds, newSeed ) =
                            List.foldl createPattern ( [], model.seed ) previews

                        createPattern { content } ( cmds, seed ) =
                            case content of
                                Err _ ->
                                    ( cmds, seed )

                                Ok storedPattern ->
                                    let
                                        ( uuid, nextSeed ) =
                                            Random.step Uuid.generator seed
                                    in
                                    ( Api.createPattern PatternCreateResponse
                                        { storedPattern | slug = Uuid.toString uuid }
                                        :: cmds
                                    , nextSeed
                                    )
                    in
                    ( { model
                        | seed = newSeed
                        , dialog = NoDialog
                      }
                    , Cmd.batch allCmds
                    )

                _ ->
                    ( model, Cmd.none )


getPattern model slug =
    let
        withSlug pattern =
            pattern.slug == slug
    in
    model.storedPatterns
        |> RemoteData.toMaybe
        |> Maybe.andThen (List.find withSlug)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.seedReceived (\( seed, seedExtension ) -> SeedReceived seed seedExtension)
        , Browser.Events.onResize
            (\width height ->
                DeviceChanged <|
                    Element.classifyDevice
                        { width = width
                        , height = height
                        }
            )
        , case model.dialog of
            NoDialog ->
                Sub.none

            _ ->
                Browser.Events.onKeyDown
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                case key of
                                    "Escape" ->
                                        Decode.succeed NewPatternCancelClicked

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                    )
        ]


view : String -> Model -> Html Msg
view prefix model =
    Element.layoutWith
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
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif
            ]
        , Element.inFront <|
            viewDialog model model.dialog
        ]
        (Element.column
            [ Element.width
                (case model.device.class of
                    Phone ->
                        Element.fill

                    Tablet ->
                        Element.fill |> Element.maximum 580

                    Desktop ->
                        Element.fill |> Element.maximum 1200

                    BigDesktop ->
                        Element.fill |> Element.maximum 1200
                )
            , Element.height Element.fill
            , Element.paddingXY 0 Design.large
            , Element.centerX
            , Element.spacing Design.normal
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ Element.el
                    [ Design.fontXXLarge
                    , Font.color Design.primary
                    , Font.bold
                    , Element.alignLeft
                    ]
                    (Element.text "Patterns")
                , Element.row
                    [ Element.spacing Design.xSmall
                    , Element.alignRight
                    ]
                    [ View.Input.btnSecondary "create-new-pattern"
                        { onPress = Just AddPatternClicked
                        , label = "Create new pattern"
                        }
                    , View.Input.btnSecondary "import-patterns"
                        { onPress = Just ImportPatternsClicked
                        , label = "Import patterns"
                        }
                    ]
                ]
            , case model.storedPatterns of
                NotAsked ->
                    Element.none

                Loading ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.padding Design.xLarge
                        ]
                        (Element.el [ Element.centerX ]
                            (Element.text "Loading patterns..")
                        )

                Failure error ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.padding Design.xLarge
                        ]
                        (Element.el [ Element.centerX ]
                            (Element.text "There was an error while loading your patterns:")
                        )

                Success storedPatterns ->
                    case model.device.class of
                        Phone ->
                            Element.column
                                [ Element.centerX
                                , Element.spacing Design.normal
                                ]
                                (List.map (viewPattern model.device prefix) storedPatterns)

                        _ ->
                            let
                                columnCount =
                                    case model.device.class of
                                        Phone ->
                                            1

                                        Tablet ->
                                            2

                                        Desktop ->
                                            3

                                        BigDesktop ->
                                            3
                            in
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing Design.large
                                ]
                                (List.map
                                    (List.map (viewPattern model.device prefix)
                                        >> List.intersperse
                                            (Element.el [ Element.width Element.fill ] Element.none)
                                        >> Element.row [ Element.width Element.fill ]
                                    )
                                    (slice columnCount storedPatterns)
                                )
            , Element.row
                [ Element.width Element.fill
                , Element.paddingXY 0 Design.large
                , Element.spacing 5
                ]
                [ Element.newTabLink
                    [ Element.alignRight
                    , Font.color Design.black
                    , Font.size Design.small
                    , Element.mouseOver
                        [ Font.color Design.primaryDark ]
                    ]
                    { url = "https://github.com/kirchner/sewing-pattern-editor"
                    , label =
                        Element.row
                            [ Element.spacing Design.xSmall ]
                            [ Element.el [ Font.underline ]
                                (Element.text "Check out the source code")
                            , View.Icon.faBrandLarge "github"
                            ]
                    }
                ]
            ]
        )


viewDialog : Model -> Dialog -> Element Msg
viewDialog model dialog =
    case dialog of
        NoDialog ->
            Element.none

        CreatePattern name ->
            View.Modal.small
                { onCancelPress = NewPatternCancelClicked
                , title = "Create new pattern"
                , content =
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Design.small
                        ]
                        [ Element.text "Create a new pattern"
                        , View.Input.text "name-input"
                            { onChange = NewPatternNameChanged
                            , text = name
                            , label = "Pick a name"
                            , help = Nothing
                            }
                        ]
                , actions =
                    [ View.Input.btnPrimary
                        { onPress = Just NewPatternCreateClicked
                        , label = "Create"
                        }
                    , Element.el [ Element.alignRight ] <|
                        View.Input.btnCancel
                            { onPress = Just NewPatternCancelClicked
                            , label = "Cancel"
                            }
                    ]
                }

        RenamePattern slug name ->
            case Maybe.map .name (getPattern model slug) of
                Nothing ->
                    Element.none

                Just oldName ->
                    View.Modal.small
                        { onCancelPress = RenamePatternCancelClicked
                        , title = "Rename the pattern «" ++ oldName ++ "»?"
                        , content =
                            Element.column
                                [ Element.width Element.fill
                                , Element.spacing Design.small
                                ]
                                [ Element.text <|
                                    "What do you want to rename the pattern «"
                                        ++ oldName
                                        ++ "» to?"
                                , View.Input.text "name-input"
                                    { onChange = RenamePatternNameChanged
                                    , text = name
                                    , label = "Pick a new name"
                                    , help = Nothing
                                    }
                                ]
                        , actions =
                            [ View.Input.btnPrimary
                                { onPress = Just RenamePatternRenameClicked
                                , label = "Rename"
                                }
                            , Element.el [ Element.alignRight ] <|
                                View.Input.btnCancel
                                    { onPress = Just RenamePatternCancelClicked
                                    , label = "Cancel"
                                    }
                            ]
                        }

        DeletePattern slug name ->
            View.Modal.small
                { onCancelPress = DeletePatternCancelClicked
                , title = "Delete «" ++ name ++ "»?"
                , content =
                    Element.paragraph
                        [ Element.htmlAttribute (Html.Attributes.id "dialog--body")
                        , Element.width Element.fill
                        , Element.padding Design.small
                        , Background.color Design.white
                        ]
                        [ Element.text "Do you want to delete the pattern "
                        , Element.el [ Font.bold ]
                            (Element.text ("«" ++ name ++ "»"))
                        , Element.text "?"
                        ]
                , actions =
                    [ View.Input.btnDanger
                        { onPress = Just DeletePatternDeleteClicked
                        , label = "Delete pattern"
                        }
                    , Element.el [ Element.alignRight ] <|
                        View.Input.btnCancel
                            { onPress = Just DeletePatternCancelClicked
                            , label = "Cancel"
                            }
                    ]
                }

        ImportPatterns { hover, previews } ->
            let
                hoverAttributes attrs =
                    if hover then
                        Border.color Design.primaryDark
                            :: Background.color Design.secondary
                            :: attrs

                    else
                        Border.color Design.primary
                            :: attrs

                viewFile { fileName, content } =
                    Element.row
                        [ Element.width Element.fill
                        , Element.spacing Design.small
                        ]
                        (case content of
                            Err error ->
                                [ Element.el
                                    [ Element.width Element.fill
                                    , Element.clip
                                    ]
                                    (Element.text fileName)
                                , Element.row
                                    [ Element.width (Element.fillPortion 1)
                                    , Element.spacing Design.xxSmall
                                    , Font.bold
                                    , Font.color Design.danger
                                    ]
                                    [ View.Icon.fa "exclamation-circle"
                                    , Element.text <|
                                        "This is not a valid pattern file: "
                                            ++ error
                                    ]
                                , Element.el [ Element.alignRight ]
                                    (View.Input.btnSecondary "remove-file-button"
                                        { onPress = Nothing
                                        , label = "Remove"
                                        }
                                    )
                                ]

                            Ok _ ->
                                [ Element.el
                                    [ Element.width Element.fill
                                    , Element.clip
                                    ]
                                    (View.Navigation.newTabLink
                                        { url = ""
                                        , label = fileName
                                        }
                                    )
                                , Element.row
                                    [ Element.width (Element.fillPortion 1)
                                    , Element.spacing Design.xxSmall
                                    , Font.bold
                                    , Font.color Design.success
                                    ]
                                    [ View.Icon.fa "check-circle"
                                    , Element.text "File can be imported."
                                    ]
                                , Element.el [ Element.alignRight ]
                                    (View.Input.btnSecondary "remove-file-button"
                                        { onPress = Nothing
                                        , label = "Remove"
                                        }
                                    )
                                ]
                        )
            in
            View.Modal.wide
                { onCancelPress = ImportPatternsCancelClicked
                , title = "Import patterns"
                , content =
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Design.small
                        ]
                        [ Element.column
                            [ Element.width Element.fill
                            , Element.spacing Design.xSmall
                            ]
                            (List.map viewFile previews)
                        , Element.el
                            ([ Element.width Element.fill
                             , Element.height (Element.px 200)
                             , Border.width 2
                             , Border.rounded Design.xSmall
                             , Border.dashed
                             , hijackOn "dragenter" (Decode.succeed ImportPatternsDragEnter)
                             , hijackOn "dragover" (Decode.succeed ImportPatternsDragEnter)
                             , hijackOn "dragleave" (Decode.succeed ImportPatternsDragLeave)
                             , hijackOn "drop"
                                (Decode.at [ "dataTransfer", "files" ]
                                    (Decode.oneOrMore ImportPatternsGotFiles File.decoder)
                                )
                             ]
                                |> hoverAttributes
                            )
                            (Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                (View.Input.btnSecondary "upload-file-button"
                                    { onPress = Just ImportPatternsPick
                                    , label = "Upload file"
                                    }
                                )
                            )
                        ]
                , actions =
                    [ View.Input.btnPrimary
                        { onPress = Just ImportPatternsImportClicked
                        , label = "Import"
                        }
                    , Element.el [ Element.alignRight ] <|
                        View.Input.btnCancel
                            { onPress = Just ImportPatternsCancelClicked
                            , label = "Cancel"
                            }
                    ]
                }


hijackOn event decoder =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn event (Decode.map hijack decoder)


hijack msg =
    ( msg, True )


viewPattern : Element.Device -> String -> StoredPattern -> Element Msg
viewPattern device prefix ({ pattern } as storedPattern) =
    Element.column
        [ Border.width 1
        , Border.rounded 4
        , Border.color Design.primary
        , Element.padding Design.small
        , Element.spacing Design.normal
        , Element.mouseOver
            [ Border.color Design.primaryDark
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 2
                , blur = 8
                , color = Design.primary
                }
            ]
        ]
        [ Element.column
            [ Element.spacing Design.normal
            , Element.pointer
            , Events.onClick (PatternCardClicked storedPattern.slug)
            ]
            [ Element.lazy2 viewPatternHelp device.class pattern
            , Element.el
                [ Region.heading 2
                , Element.alignLeft
                ]
                (Element.link []
                    { url = Route.toString prefix (Route.Editor storedPattern.slug Nothing)
                    , label =
                        Element.el
                            [ Design.fontXLarge
                            , Font.bold
                            , Font.color Design.primary
                            , Font.underline
                            , Element.mouseOver
                                [ Font.color Design.primaryDark ]
                            ]
                            (Element.text storedPattern.name)
                    }
                )
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing Design.xSmall
            ]
            [ View.Input.btnSecondary (storedPattern.slug ++ "-rename")
                { onPress = Just (RenamePatternPressed storedPattern.slug)
                , label = "Rename"
                }
            , Element.downloadAs
                [ Element.alignLeft
                , Element.paddingXY 18 10
                , Background.color Design.secondary
                , Font.color Design.black
                , Design.fontSmall
                , Element.mouseOver
                    [ Background.color Design.secondaryDark ]
                , Element.htmlAttribute <|
                    Html.Attributes.style "transition"
                        "background-color 0.2s ease-in-out 0s"
                ]
                { url =
                    "data:application/json;charset=utf-8,"
                        ++ Encode.encode 0 (StoredPattern.encode storedPattern)
                , label = Element.text "Download"
                , filename = storedPattern.slug ++ ".json"
                }
            , Element.el [ Element.alignRight ] <|
                case device.class of
                    Tablet ->
                        View.Input.btnDangerIcon
                            { onPress =
                                Just <|
                                    DeletePatternPressed storedPattern.slug storedPattern.name
                            , icon = "trash"
                            }

                    _ ->
                        View.Input.btnDanger
                            { onPress =
                                Just <|
                                    DeletePatternPressed storedPattern.slug storedPattern.name
                            , label = "Delete"
                            }
            ]
        ]


viewPatternHelp class pattern =
    let
        viewBox =
            String.join " "
                [ String.fromFloat
                    (BoundingBox2d.minX boundingBox
                        - max 0 ((max width height - width) / 2)
                    )
                , String.fromFloat
                    (BoundingBox2d.minY boundingBox
                        - max 0 ((max width height - height) / 2)
                    )
                , String.fromFloat (max width height)
                , String.fromFloat (max width height)
                ]

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox

        boundingBox =
            State.finalValue pattern
                (State.traverse Pattern.point2d (Pattern.points pattern))
                |> List.filterMap Result.toMaybe
                |> BoundingBox2d.containingPoints
                |> Maybe.map scale
                |> Maybe.withDefault
                    (BoundingBox2d.fromExtrema
                        { minX = 0
                        , maxX = maxX
                        , minY = 0
                        , maxY = maxY
                        }
                    )

        ( maxX, maxY ) =
            case class of
                Phone ->
                    ( 330, 280 )

                Tablet ->
                    ( 270 - 2 * Design.small, 220 )

                Desktop ->
                    ( 330, 280 )

                BigDesktop ->
                    ( 330, 280 )

        scale box =
            let
                centerPoint =
                    BoundingBox2d.centerPoint box
            in
            BoundingBox2d.scaleAbout centerPoint 1.1 box

        zoom =
            130 / max width height
    in
    Element.el
        [ Border.rounded 4
        , Element.width (Element.px maxX)
        , Element.height (Element.px maxY)
        , Background.color Design.secondary
        ]
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox viewBox
                , Html.Attributes.style "user-select" "none"
                , Html.Events.preventDefaultOn "dragstart" <|
                    Decode.succeed ( NoOp, True )
                ]
                [ State.finalValue pattern <|
                    Pattern.draw
                        { preview = True
                        , zoom = zoom
                        , objectHovered = always NoOp
                        , hoveredObject = Nothing
                        }
                ]
        )


noSelections =
    { points = Those.fromList []
    , lines = Those.fromList []
    , lineSegments = Those.fromList []
    , details = Those.fromList []
    }



---- HELPER


slice : Int -> List a -> List (List a)
slice size list =
    sliceHelp size [] list


sliceHelp : Int -> List (List a) -> List a -> List (List a)
sliceHelp size result list =
    case List.take size list of
        [] ->
            List.reverse result

        next ->
            sliceHelp size (next :: result) (List.drop size list)
