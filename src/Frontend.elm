module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Lamdera
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils exposing (createTeamUrl, extractTeamIdFromUrl)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        page =
            parseUrl url

        initialModel =
            { key = key
            , page = page
            , currentTeam = Nothing
            , createTeamForm = { name = "" }
            , createMatchForm = { opponent = "", date = "", time = "", venue = "", isHome = True }
            , showCreateMatchModal = False
            , matches = []
            , members = []
            , availability = []
            }

        cmd =
            case page of
                TeamPage teamId ->
                    Lamdera.sendToBackend (GetTeamRequest teamId)

                _ ->
                    Cmd.none
    in
    ( initialModel, cmd )


parseUrl : Url.Url -> Page
parseUrl url =
    case Parser.parse routeParser url of
        Just page ->
            page

        Nothing ->
            NotFoundPage


routeParser : Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomePage Parser.top
        , Parser.map CreateTeamPage (Parser.s "create")
        , Parser.map
            (\teamSlugId ->
                case extractTeamIdFromUrl ("/team/" ++ teamSlugId) of
                    Just teamId ->
                        TeamPage teamId

                    Nothing ->
                        NotFoundPage
            )
            (Parser.s "team" </> Parser.string)
        ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                newPage =
                    parseUrl url

                cmd =
                    case newPage of
                        TeamPage teamId ->
                            Lamdera.sendToBackend (GetTeamRequest teamId)

                        _ ->
                            Cmd.none
            in
            ( { model | page = newPage }, cmd )

        CreateTeamFormUpdated form ->
            ( { model | createTeamForm = form }, Cmd.none )

        CreateTeamSubmitted ->
            if String.isEmpty (String.trim model.createTeamForm.name) then
                ( model, Cmd.none )

            else
                ( model
                , Lamdera.sendToBackend (CreateTeamRequest model.createTeamForm.name)
                )

        CreateMatchFormUpdated form ->
            ( { model | createMatchForm = form }, Cmd.none )

        CreateMatchSubmitted teamId ->
            if String.isEmpty (String.trim model.createMatchForm.opponent) then
                ( model, Cmd.none )

            else
                ( { model | showCreateMatchModal = False, createMatchForm = { opponent = "", date = "", time = "", venue = "", isHome = True } }
                , Lamdera.sendToBackend (CreateMatchRequest teamId model.createMatchForm)
                )

        ShowCreateMatchModal ->
            ( { model | showCreateMatchModal = True }, Cmd.none )

        HideCreateMatchModal ->
            ( { model | showCreateMatchModal = False }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TeamCreated team ->
            let
                teamUrl =
                    createTeamUrl team.slug team.id
            in
            ( { model | currentTeam = Just team }
            , Nav.pushUrl model.key teamUrl
            )

        TeamLoaded team matches members availability ->
            ( { model
                | currentTeam = Just team
                , matches = matches
                , members = members
                , availability = availability
              }
            , Cmd.none
            )

        TeamNotFound ->
            ( { model | page = NotFoundPage }, Cmd.none )

        MatchCreated match ->
            ( { model | matches = match :: model.matches }, Cmd.none )

        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = getPageTitle model
    , body = [ viewPage model ]
    }


getPageTitle : Model -> String
getPageTitle model =
    case model.page of
        HomePage ->
            "Mannschaftsorganisation"

        CreateTeamPage ->
            "Mannschaft erstellen"

        TeamPage _ ->
            case model.currentTeam of
                Just team ->
                    team.name

                Nothing ->
                    "Lädt..."

        NotFoundPage ->
            "Seite nicht gefunden"


viewPage : Model -> Html FrontendMsg
viewPage model =
    Html.div
        [ Attr.style "min-height" "100vh"
        , Attr.style "font-family" "system-ui, -apple-system, sans-serif"
        , Attr.style "background-color" "#f8fafc"
        ]
        [ viewHeader model
        , Html.main_
            [ Attr.style "max-width" "1200px"
            , Attr.style "margin" "0 auto"
            , Attr.style "padding" "2rem"
            ]
            [ viewContent model ]
        ]


viewHeader : Model -> Html FrontendMsg
viewHeader model =
    Html.header
        [ Attr.style "background-color" "white"
        , Attr.style "border-bottom" "1px solid #e2e8f0"
        , Attr.style "padding" "1rem 2rem"
        ]
        [ Html.div
            [ Attr.style "max-width" "1200px"
            , Attr.style "margin" "0 auto"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            ]
            [ Html.h1
                [ Attr.style "font-size" "1.5rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                ]
                [ Html.text "Mannschafts-Manager" ]
            , Html.nav []
                [ Html.a
                    [ Attr.href "/"
                    , Attr.style "margin-right" "1rem"
                    , Attr.style "color" "#3b82f6"
                    , Attr.style "text-decoration" "none"
                    ]
                    [ Html.text "Startseite" ]
                , Html.a
                    [ Attr.href "/create"
                    , Attr.style "color" "#3b82f6"
                    , Attr.style "text-decoration" "none"
                    ]
                    [ Html.text "Mannschaft erstellen" ]
                ]
            ]
        ]


viewContent : Model -> Html FrontendMsg
viewContent model =
    case model.page of
        HomePage ->
            viewHomePage

        CreateTeamPage ->
            viewCreateTeamPage model

        TeamPage _ ->
            viewTeamPage model

        NotFoundPage ->
            viewNotFoundPage


viewHomePage : Html FrontendMsg
viewHomePage =
    Html.div
        [ Attr.style "text-align" "center"
        , Attr.style "padding" "3rem 0"
        ]
        [ Html.h2
            [ Attr.style "font-size" "2rem"
            , Attr.style "font-weight" "700"
            , Attr.style "color" "#1e293b"
            , Attr.style "margin-bottom" "1rem"
            ]
            [ Html.text "Organisiere deine Mannschaft" ]
        , Html.p
            [ Attr.style "font-size" "1.1rem"
            , Attr.style "color" "#64748b"
            , Attr.style "margin-bottom" "2rem"
            , Attr.style "max-width" "600px"
            , Attr.style "margin-left" "auto"
            , Attr.style "margin-right" "auto"
            ]
            [ Html.text "Verwalte Spiele, verfolge die Verfügbarkeit von Mannschaftsmitgliedern und halte alle organisiert. Keine Anmeldung erforderlich - einfach eine Mannschaft erstellen und den Link teilen." ]
        , Html.a
            [ Attr.href "/create"
            , Attr.style "display" "inline-block"
            , Attr.style "background-color" "#3b82f6"
            , Attr.style "color" "white"
            , Attr.style "padding" "0.75rem 1.5rem"
            , Attr.style "border-radius" "0.5rem"
            , Attr.style "text-decoration" "none"
            , Attr.style "font-weight" "500"
            , Attr.style "transition" "background-color 0.2s"
            ]
            [ Html.text "Deine Mannschaft erstellen" ]
        ]


viewCreateTeamPage : Model -> Html FrontendMsg
viewCreateTeamPage model =
    Html.div
        [ Attr.style "max-width" "500px"
        , Attr.style "margin" "0 auto"
        ]
        [ Html.h2
            [ Attr.style "font-size" "1.75rem"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#1e293b"
            , Attr.style "margin-bottom" "1.5rem"
            , Attr.style "text-align" "center"
            ]
            [ Html.text "Neue Mannschaft erstellen" ]
        , Html.form
            [ Events.onSubmit CreateTeamSubmitted
            , Attr.style "background-color" "white"
            , Attr.style "padding" "2rem"
            , Attr.style "border-radius" "0.5rem"
            , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
            ]
            [ Html.div
                [ Attr.style "margin-bottom" "1.5rem" ]
                [ Html.label
                    [ Attr.style "display" "block"
                    , Attr.style "font-weight" "500"
                    , Attr.style "color" "#374151"
                    , Attr.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Mannschaftsname" ]
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.value model.createTeamForm.name
                    , Events.onInput (\name -> CreateTeamFormUpdated { name = name })
                    , Attr.placeholder "Mannschaftsname eingeben"
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "0.75rem"
                    , Attr.style "border" "1px solid #d1d5db"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    []
                ]
            , Html.button
                [ Attr.type_ "submit"
                , Attr.style "width" "100%"
                , Attr.style "background-color" "#3b82f6"
                , Attr.style "color" "white"
                , Attr.style "padding" "0.75rem"
                , Attr.style "border" "none"
                , Attr.style "border-radius" "0.375rem"
                , Attr.style "font-size" "1rem"
                , Attr.style "font-weight" "500"
                , Attr.style "cursor" "pointer"
                , Attr.style "transition" "background-color 0.2s"
                ]
                [ Html.text "Mannschaft erstellen" ]
            ]
        ]


viewTeamPage : Model -> Html FrontendMsg
viewTeamPage model =
    case model.currentTeam of
        Just team ->
            Html.div []
                [ Html.h2
                    [ Attr.style "font-size" "1.75rem"
                    , Attr.style "font-weight" "600"
                    , Attr.style "color" "#1e293b"
                    , Attr.style "margin-bottom" "2rem"
                    ]
                    [ Html.text team.name ]
                , if List.isEmpty model.matches && List.isEmpty model.members then
                    Html.div
                        [ Attr.style "background-color" "white"
                        , Attr.style "padding" "2rem"
                        , Attr.style "border-radius" "0.5rem"
                        , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
                        , Attr.style "text-align" "center"
                        ]
                        [ Html.h3
                            [ Attr.style "font-size" "1.25rem"
                            , Attr.style "font-weight" "600"
                            , Attr.style "color" "#1e293b"
                            , Attr.style "margin-bottom" "1rem"
                            ]
                            [ Html.text "Mannschaft erfolgreich erstellt!" ]
                        , Html.p
                            [ Attr.style "color" "#64748b"
                            , Attr.style "margin-bottom" "1.5rem"
                            ]
                            [ Html.text "Teile diesen Link mit deinen Mannschaftsmitgliedern, damit sie Zugang zur Mannschaft haben:" ]
                        , Html.div
                            [ Attr.style "background-color" "#f1f5f9"
                            , Attr.style "padding" "1rem"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "border" "1px solid #e2e8f0"
                            , Attr.style "font-family" "monospace"
                            , Attr.style "font-size" "0.9rem"
                            , Attr.style "word-break" "break-all"
                            ]
                            [ Html.text ("https://your-domain.lamdera.app" ++ createTeamUrl team.slug team.id) ]
                        , Html.p
                            [ Attr.style "color" "#64748b"
                            , Attr.style "margin-top" "1rem"
                            , Attr.style "font-size" "0.9rem"
                            ]
                            [ Html.text "Speichere diese Seite als Lesezeichen für einfachen Zugang." ]
                        ]

                  else
                    Html.text ""
                , viewMatchesSection model team
                , if model.showCreateMatchModal then
                    viewCreateMatchModal model team

                  else
                    Html.text ""
                ]

        Nothing ->
            Html.div
                [ Attr.style "text-align" "center"
                , Attr.style "padding" "3rem 0"
                ]
                [ Html.text "Mannschaft wird geladen..." ]


viewNotFoundPage : Html FrontendMsg
viewNotFoundPage =
    Html.div
        [ Attr.style "text-align" "center"
        , Attr.style "padding" "3rem 0"
        ]
        [ Html.h2
            [ Attr.style "font-size" "1.75rem"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#1e293b"
            , Attr.style "margin-bottom" "1rem"
            ]
            [ Html.text "Seite nicht gefunden" ]
        , Html.p
            [ Attr.style "color" "#64748b"
            , Attr.style "margin-bottom" "2rem"
            ]
            [ Html.text "Die gesuchte Seite existiert nicht." ]
        , Html.a
            [ Attr.href "/"
            , Attr.style "color" "#3b82f6"
            , Attr.style "text-decoration" "none"
            ]
            [ Html.text "Zur Startseite" ]
        ]



-- FORM HELPERS


updateMatchFormOpponent : String -> CreateMatchForm -> CreateMatchForm
updateMatchFormOpponent opponent form =
    { form | opponent = opponent }


updateMatchFormDate : String -> CreateMatchForm -> CreateMatchForm
updateMatchFormDate date form =
    { form | date = date }


updateMatchFormTime : String -> CreateMatchForm -> CreateMatchForm
updateMatchFormTime time form =
    { form | time = time }


updateMatchFormVenue : String -> CreateMatchForm -> CreateMatchForm
updateMatchFormVenue venue form =
    { form | venue = venue }


updateMatchFormIsHome : Bool -> CreateMatchForm -> CreateMatchForm
updateMatchFormIsHome isHome form =
    { form | isHome = isHome }



-- TEAM PAGE HELPERS


viewMatchesSection : Model -> Team -> Html FrontendMsg
viewMatchesSection model team =
    Html.div
        [ Attr.style "margin-top" "3rem" ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "margin-bottom" "1.5rem"
            ]
            [ Html.h3
                [ Attr.style "font-size" "1.5rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                ]
                [ Html.text "Spiele" ]
            , Html.button
                [ Events.onClick ShowCreateMatchModal
                , Attr.style "background-color" "#3b82f6"
                , Attr.style "color" "white"
                , Attr.style "padding" "0.5rem 1rem"
                , Attr.style "border" "none"
                , Attr.style "border-radius" "0.375rem"
                , Attr.style "font-size" "0.875rem"
                , Attr.style "font-weight" "500"
                , Attr.style "cursor" "pointer"
                , Attr.style "transition" "background-color 0.2s"
                ]
                [ Html.text "+ Spiel hinzufügen" ]
            ]
        , if List.isEmpty model.matches then
            Html.div
                [ Attr.style "background-color" "white"
                , Attr.style "padding" "2rem"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
                , Attr.style "text-align" "center"
                ]
                [ Html.p
                    [ Attr.style "color" "#64748b" ]
                    [ Html.text "Noch keine Spiele geplant. Füge das erste Spiel hinzu!" ]
                ]

          else
            Html.div
                [ Attr.style "background-color" "white"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
                , Attr.style "overflow" "hidden"
                ]
                (List.map viewMatchItem model.matches)
        ]


viewMatchItem : Match -> Html FrontendMsg
viewMatchItem match =
    Html.div
        [ Attr.style "padding" "1rem 1.5rem"
        , Attr.style "border-bottom" "1px solid #e2e8f0"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            ]
            [ Html.div []
                [ Html.h4
                    [ Attr.style "font-weight" "600"
                    , Attr.style "color" "#1e293b"
                    , Attr.style "margin" "0 0 0.25rem 0"
                    ]
                    [ Html.text
                        (if match.isHome then
                            "vs " ++ match.opponent

                         else
                            "@ " ++ match.opponent
                        )
                    ]
                , Html.p
                    [ Attr.style "color" "#64748b"
                    , Attr.style "margin" "0"
                    , Attr.style "font-size" "0.875rem"
                    ]
                    [ Html.text (match.date ++ " um " ++ match.time ++ " - " ++ match.venue) ]
                ]
            , Html.div
                [ Attr.style "text-align" "right" ]
                [ Html.span
                    [ Attr.style "background-color"
                        (if match.isHome then
                            "#dcfce7"

                         else
                            "#fef3c7"
                        )
                    , Attr.style "color"
                        (if match.isHome then
                            "#166534"

                         else
                            "#92400e"
                        )
                    , Attr.style "padding" "0.25rem 0.5rem"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "0.75rem"
                    , Attr.style "font-weight" "500"
                    ]
                    [ Html.text
                        (if match.isHome then
                            "Heim"

                         else
                            "Auswärts"
                        )
                    ]
                ]
            ]
        ]


viewCreateMatchModal : Model -> Team -> Html FrontendMsg
viewCreateMatchModal model team =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background-color" "rgba(0,0,0,0.5)"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "z-index" "1000"
        , Events.onClick HideCreateMatchModal
        ]
        [ Html.div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.5rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "90%"
            , Attr.style "max-height" "90vh"
            , Attr.style "overflow-y" "auto"
            , Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOpFrontendMsg, True ))
            ]
            [ Html.div
                [ Attr.style "padding" "1.5rem"
                , Attr.style "border-bottom" "1px solid #e5e7eb"
                ]
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "center"
                    ]
                    [ Html.h3
                        [ Attr.style "font-size" "1.25rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin" "0"
                        ]
                        [ Html.text "Spiel hinzufügen" ]
                    , Html.button
                        [ Events.onClick HideCreateMatchModal
                        , Attr.style "background" "none"
                        , Attr.style "border" "none"
                        , Attr.style "font-size" "1.5rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "color" "#6b7280"
                        ]
                        [ Html.text "×" ]
                    ]
                ]
            , Html.form
                [ Events.onSubmit (CreateMatchSubmitted team.id)
                , Attr.style "padding" "1.5rem"
                ]
                [ Html.div
                    [ Attr.style "display" "grid"
                    , Attr.style "grid-template-columns" "1fr 1fr"
                    , Attr.style "gap" "1.5rem"
                    , Attr.style "margin-bottom" "1.5rem"
                    ]
                    [ Html.div []
                        [ Html.label
                            [ Attr.style "display" "block"
                            , Attr.style "font-weight" "500"
                            , Attr.style "color" "#374151"
                            , Attr.style "margin-bottom" "0.5rem"
                            ]
                            [ Html.text "Gegner" ]
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.value model.createMatchForm.opponent
                            , Events.onInput (\opponent -> CreateMatchFormUpdated (updateMatchFormOpponent opponent model.createMatchForm))
                            , Attr.placeholder "Gegner eingeben"
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border" "1px solid #d1d5db"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "font-size" "1rem"
                            , Attr.style "box-sizing" "border-box"
                            ]
                            []
                        ]
                    , Html.div []
                        [ Html.label
                            [ Attr.style "display" "block"
                            , Attr.style "font-weight" "500"
                            , Attr.style "color" "#374151"
                            , Attr.style "margin-bottom" "0.5rem"
                            ]
                            [ Html.text "Datum" ]
                        , Html.input
                            [ Attr.type_ "date"
                            , Attr.value model.createMatchForm.date
                            , Events.onInput (\date -> CreateMatchFormUpdated (updateMatchFormDate date model.createMatchForm))
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border" "1px solid #d1d5db"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "font-size" "1rem"
                            , Attr.style "box-sizing" "border-box"
                            ]
                            []
                        ]
                    ]
                , Html.div
                    [ Attr.style "display" "grid"
                    , Attr.style "grid-template-columns" "1fr 1fr"
                    , Attr.style "gap" "1.5rem"
                    , Attr.style "margin-bottom" "1.5rem"
                    ]
                    [ Html.div []
                        [ Html.label
                            [ Attr.style "display" "block"
                            , Attr.style "font-weight" "500"
                            , Attr.style "color" "#374151"
                            , Attr.style "margin-bottom" "0.5rem"
                            ]
                            [ Html.text "Uhrzeit" ]
                        , Html.input
                            [ Attr.type_ "time"
                            , Attr.value model.createMatchForm.time
                            , Events.onInput (\time -> CreateMatchFormUpdated (updateMatchFormTime time model.createMatchForm))
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border" "1px solid #d1d5db"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "font-size" "1rem"
                            , Attr.style "box-sizing" "border-box"
                            ]
                            []
                        ]
                    , Html.div []
                        [ Html.label
                            [ Attr.style "display" "block"
                            , Attr.style "font-weight" "500"
                            , Attr.style "color" "#374151"
                            , Attr.style "margin-bottom" "0.5rem"
                            ]
                            [ Html.text "Austragungsort" ]
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.value model.createMatchForm.venue
                            , Events.onInput (\venue -> CreateMatchFormUpdated (updateMatchFormVenue venue model.createMatchForm))
                            , Attr.placeholder "Ort/Stadion"
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border" "1px solid #d1d5db"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "font-size" "1rem"
                            , Attr.style "box-sizing" "border-box"
                            ]
                            []
                        ]
                    ]
                , Html.div
                    [ Attr.style "margin-bottom" "2rem" ]
                    [ Html.label
                        [ Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "font-weight" "500"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.input
                            [ Attr.type_ "checkbox"
                            , Attr.checked model.createMatchForm.isHome
                            , Events.onCheck (\isHome -> CreateMatchFormUpdated (updateMatchFormIsHome (not isHome) model.createMatchForm))
                            , Attr.style "margin-right" "0.5rem"
                            ]
                            []
                        , Html.text "Heimspiel"
                        ]
                    ]
                , Html.button
                    [ Attr.type_ "submit"
                    , Attr.style "background-color" "#3b82f6"
                    , Attr.style "color" "white"
                    , Attr.style "padding" "0.75rem 1.5rem"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "font-weight" "500"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "transition" "background-color 0.2s"
                    ]
                    [ Html.text "Spiel hinzufügen" ]
                ]
            ]
        ]
