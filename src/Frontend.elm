module Frontend exposing (..)

import Basics exposing (min)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Lamdera
import LocalStorage
import QRCode
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils exposing (createTeamUrl, extractTeamIdFromUrl, isoToGermanDate, separatePastAndFutureMatches, sortMatchesByDate)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> LocalStorage.fromJS LocalStorageMessage
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
            , activeMemberId = Nothing
            , createTeamForm = { name = "", creatorName = "", otherMemberNames = "", playersNeeded = "" }
            , createMatchForm = { opponent = "", date = "", time = "", venue = "", isHome = True }
            , createMemberForm = { name = "" }
            , showCreateMatchModal = False
            , showCreateMemberModal = False
            , showMemberSelectionModal = False
            , showCreateMemberInModal = False
            , showChangeMatchDateModal = False
            , changeMatchDateForm = ""
            , changeMatchDateMatchId = Nothing
            , showShareModal = False
            , expandedMatches = []
            , pastMatchesShown = 10
            , pastMatchesExpanded = False
            , matches = []
            , members = []
            , availability = []
            , hostname = Nothing
            }

        cmd =
            case page of
                TeamPage teamId ->
                    Cmd.batch
                        [ LocalStorage.toJS "GET_HOSTNAME"
                        , Lamdera.sendToBackend (GetTeamRequest teamId)
                        ]

                _ ->
                    LocalStorage.toJS "GET_HOSTNAME"
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
            if String.isEmpty (String.trim model.createTeamForm.name) || String.isEmpty (String.trim model.createTeamForm.creatorName) || String.isEmpty (String.trim model.createTeamForm.playersNeeded) then
                ( model, Cmd.none )

            else
                let
                    otherMemberNames =
                        model.createTeamForm.otherMemberNames
                            |> String.split ","
                            |> List.map String.trim
                            |> List.filter (not << String.isEmpty)

                    playersNeeded =
                        String.toInt (String.trim model.createTeamForm.playersNeeded)
                            |> Maybe.withDefault 11

                    -- Default to 11 if invalid
                in
                ( model
                , Lamdera.sendToBackend (CreateTeamRequest model.createTeamForm.name model.createTeamForm.creatorName otherMemberNames playersNeeded)
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

        CreateMemberFormUpdated form ->
            ( { model | createMemberForm = form }, Cmd.none )

        CreateMemberSubmitted teamId ->
            if String.isEmpty (String.trim model.createMemberForm.name) then
                ( model, Cmd.none )

            else
                ( { model
                    | createMemberForm = { name = "" }
                  }
                , Lamdera.sendToBackend (CreateMemberRequest teamId model.createMemberForm)
                )

        ShowCreateMemberModal ->
            ( { model | showCreateMemberModal = True }, Cmd.none )

        HideCreateMemberModal ->
            ( { model | showCreateMemberModal = False }, Cmd.none )

        ShowMemberSelectionModal ->
            ( { model | showMemberSelectionModal = True }, Cmd.none )

        HideMemberSelectionModal ->
            ( { model | showMemberSelectionModal = False }, Cmd.none )

        ShowCreateMemberInModal ->
            ( { model | showCreateMemberInModal = True }, Cmd.none )

        HideCreateMemberInModal ->
            ( { model | showCreateMemberInModal = False }, Cmd.none )

        SelectActiveMember memberId ->
            ( { model | activeMemberId = Just memberId, showMemberSelectionModal = False }
            , LocalStorage.toJS ("SET:" ++ memberId)
            )

        LocalStorageMessage message ->
            if String.startsWith "LOAD:" message then
                let
                    memberIdPart =
                        String.dropLeft 5 message

                    maybeMemberId =
                        if memberIdPart == "null" then
                            Nothing

                        else
                            Just memberIdPart
                in
                ( { model | activeMemberId = maybeMemberId }, Cmd.none )

            else if String.startsWith "HOSTNAME:" message then
                let
                    hostname =
                        String.dropLeft 9 message
                in
                ( { model | hostname = Just hostname }, Cmd.none )

            else
                ( model, Cmd.none )

        LogoutRequested ->
            ( { model | activeMemberId = Nothing, page = HomePage }
            , Cmd.batch
                [ LocalStorage.toJS "CLEAR"
                , Nav.pushUrl model.key "/"
                ]
            )

        SetAvailability memberId matchId availability ->
            case model.activeMemberId of
                Just activeMemberId ->
                    if activeMemberId == memberId then
                        ( model
                        , Lamdera.sendToBackend (UpdateAvailabilityRequest memberId matchId availability)
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleMatchDetails matchId ->
            let
                updatedExpandedMatches =
                    if List.member matchId model.expandedMatches then
                        List.filter ((/=) matchId) model.expandedMatches

                    else
                        matchId :: model.expandedMatches
            in
            ( { model | expandedMatches = updatedExpandedMatches }, Cmd.none )

        LoadMorePastMatches ->
            ( { model | pastMatchesShown = model.pastMatchesShown + 10 }, Cmd.none )

        TogglePastMatchesSection ->
            ( { model | pastMatchesExpanded = not model.pastMatchesExpanded }, Cmd.none )

        ShowChangeMatchDateModal matchId ->
            let
                currentMatch =
                    model.matches
                        |> List.filter (\match -> match.id == matchId)
                        |> List.head
            in
            ( { model
                | showChangeMatchDateModal = True
                , changeMatchDateMatchId = Just matchId
                , changeMatchDateForm = Maybe.map .date currentMatch |> Maybe.withDefault ""
              }
            , Cmd.none
            )

        HideChangeMatchDateModal ->
            ( { model
                | showChangeMatchDateModal = False
                , changeMatchDateMatchId = Nothing
                , changeMatchDateForm = ""
              }
            , Cmd.none
            )

        ChangeMatchDateFormUpdated matchId newDate ->
            ( { model | changeMatchDateForm = newDate }, Cmd.none )

        ChangeMatchDateSubmitted matchId newDate ->
            case model.currentTeam of
                Just team ->
                    ( model
                    , Lamdera.sendToBackend (ChangeMatchDateRequest matchId newDate team.id)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ShowShareModal ->
            ( { model | showShareModal = True }, Cmd.none )

        HideShareModal ->
            ( { model | showShareModal = False }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TeamCreated team creatorMemberId ->
            let
                teamUrl =
                    createTeamUrl team.slug team.id
            in
            ( { model | currentTeam = Just team, activeMemberId = Just creatorMemberId }
            , Cmd.batch
                [ Nav.pushUrl model.key teamUrl
                , LocalStorage.toJS ("SET:" ++ creatorMemberId)
                ]
            )

        TeamLoaded team matches members availability ->
            let
                shouldShowMemberSelection =
                    case model.activeMemberId of
                        Nothing ->
                            not (List.isEmpty members)

                        Just memberId ->
                            -- Check if the stored member ID is still valid for this team
                            not (List.any (\member -> member.id == memberId) members)
            in
            ( { model
                | currentTeam = Just team
                , matches = matches
                , members = members
                , availability = availability
                , showMemberSelectionModal = shouldShowMemberSelection
              }
            , Cmd.none
            )

        TeamNotFound ->
            ( { model | page = NotFoundPage }, Cmd.none )

        MatchCreated match ->
            ( { model | matches = match :: model.matches }, Cmd.none )

        MemberCreated member ->
            let
                -- If we were in the member selection modal and just created a member,
                -- automatically set them as the active member
                shouldSetAsActive =
                    model.showMemberSelectionModal || model.showCreateMemberInModal
            in
            ( { model
                | members = member :: model.members
                , activeMemberId =
                    if shouldSetAsActive then
                        Just member.id

                    else
                        model.activeMemberId
                , showMemberSelectionModal = False
                , showCreateMemberInModal = False
              }
            , if shouldSetAsActive then
                LocalStorage.toJS ("SET:" ++ member.id)

              else
                Cmd.none
            )

        AvailabilityUpdated availabilityRecord ->
            let
                updatedAvailability =
                    -- Remove any existing record for this member/match combination first
                    model.availability
                        |> List.filter (\record -> not (record.memberId == availabilityRecord.memberId && record.matchId == availabilityRecord.matchId))
                        |> (::) availabilityRecord
            in
            ( { model | availability = updatedAvailability }, Cmd.none )

        MatchDateChanged matchId newDate ->
            let
                -- Update the match date in the matches list
                updatedMatches =
                    List.map
                        (\match ->
                            if match.id == matchId then
                                { match | date = newDate }

                            else
                                match
                        )
                        model.matches

                -- Remove all availability records for this match
                updatedAvailability =
                    List.filter (\record -> record.matchId /= matchId) model.availability
            in
            ( { model
                | matches = updatedMatches
                , availability = updatedAvailability
                , showChangeMatchDateModal = False
                , changeMatchDateMatchId = Nothing
                , changeMatchDateForm = ""
              }
            , Cmd.none
            )

        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = getPageTitle model
    , body =
        [ Html.node "meta"
            [ Attr.attribute "name" "viewport"
            , Attr.attribute "content" "width=device-width, initial-scale=1.0"
            ]
            []
        , viewPage model
        ]
    }


getPageTitle : Model -> String
getPageTitle model =
    case model.page of
        HomePage ->
            "Zusammenspiel"

        CreateTeamPage ->
            "Team erstellen"

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
        [ -- Mobile viewport meta tag would go in document head, but we'll handle responsive design with CSS
          viewHeader model
        , Html.main_
            [ Attr.style "max-width" "100vw"
            , Attr.style "margin" "0"
            , Attr.style "padding" "0.5rem"
            , Attr.style "width" "100vw"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "overflow-x" "hidden"
            ]
            [ viewContent model ]
        ]


viewHeader : Model -> Html FrontendMsg
viewHeader model =
    Html.header
        [ Attr.style "background-color" "white"
        , Attr.style "border-bottom" "1px solid #e2e8f0"
        , Attr.style "padding" "0.75rem 1rem"
        , Attr.style "width" "100vw"
        , Attr.style "box-sizing" "border-box"
        ]
        [ Html.div
            [ Attr.style "max-width" "100vw"
            , Attr.style "margin" "0"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "gap" "0.5rem"
            , Attr.style "width" "100%"
            ]
            [ Html.h1
                [ Attr.style "font-size" "1.25rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                , Attr.style "flex-shrink" "0"
                ]
                [ Html.text "Zusammenspiel" ]
            , Html.nav
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.75rem"
                , Attr.style "flex-wrap" "wrap"
                ]
                [ viewUserInfo model
                ]
            ]
        ]


viewUserInfo : Model -> Html FrontendMsg
viewUserInfo model =
    case model.activeMemberId of
        Just memberId ->
            let
                memberName =
                    model.members
                        |> List.filter (\member -> member.id == memberId)
                        |> List.head
                        |> Maybe.map .name
                        |> Maybe.withDefault "Unbekannter Nutzer"
            in
            Html.div
                [ Attr.style "position" "relative"
                , Attr.style "display" "inline-block"
                ]
                [ Html.button
                    [ Events.onClick LogoutRequested
                    , Attr.style "background" "none"
                    , Attr.style "border" "none"
                    , Attr.style "color" "#374151"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "padding" "0.5rem 0.75rem"
                    , Attr.style "min-height" "44px" -- iOS minimum touch target
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "0.875rem"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "0.5rem"
                    , Attr.style "transition" "background-color 0.2s"
                    , Attr.style "background-color" "#f8fafc"
                    ]
                    [ Html.span
                        [ Attr.style "font-weight" "500" ]
                        [ Html.text memberName ]
                    , Html.span
                        [ Attr.style "color" "#64748b"
                        , Attr.style "font-size" "0.75rem"
                        ]
                        [ Html.text "Abmelden" ]
                    ]
                ]

        Nothing ->
            Html.text ""


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
            [ Html.text "Organisiere dein Team" ]
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
            [ Html.text "Dein Team erstellen" ]
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
            [ Html.text "Neues Team erstellen" ]
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
                    [ Html.text "Teamname" ]
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.value model.createTeamForm.name
                    , Events.onInput (\name -> CreateTeamFormUpdated { name = name, creatorName = model.createTeamForm.creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = model.createTeamForm.playersNeeded })
                    , Attr.placeholder "Teamname eingeben"
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "0.75rem"
                    , Attr.style "border" "1px solid #d1d5db"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    []
                ]
            , Html.div
                [ Attr.style "margin-bottom" "1.5rem" ]
                [ Html.label
                    [ Attr.style "display" "block"
                    , Attr.style "font-weight" "500"
                    , Attr.style "color" "#374151"
                    , Attr.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Dein Name *" ]
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.value model.createTeamForm.creatorName
                    , Events.onInput (\creatorName -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = model.createTeamForm.playersNeeded })
                    , Attr.placeholder "Dein Name"
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "0.75rem"
                    , Attr.style "border" "1px solid #d1d5db"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    []
                ]
            , Html.div
                [ Attr.style "margin-bottom" "2rem" ]
                [ Html.label
                    [ Attr.style "display" "block"
                    , Attr.style "font-weight" "500"
                    , Attr.style "color" "#374151"
                    , Attr.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Weitere Teammitglieder (optional)" ]
                , Html.textarea
                    [ Attr.value model.createTeamForm.otherMemberNames
                    , Events.onInput (\names -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = model.createTeamForm.creatorName, otherMemberNames = names, playersNeeded = model.createTeamForm.playersNeeded })
                    , Attr.placeholder "Namen durch Komma getrennt eingeben, z.B. Max Mustermann, Anna Schmidt, Tom Weber"
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "0.75rem"
                    , Attr.style "border" "1px solid #d1d5db"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "min-height" "100px"
                    , Attr.style "resize" "vertical"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    []
                , Html.p
                    [ Attr.style "color" "#6b7280"
                    , Attr.style "font-size" "0.875rem"
                    , Attr.style "margin-top" "0.5rem"
                    , Attr.style "margin-bottom" "0"
                    ]
                    [ Html.text "Du kannst Mitglieder auch später hinzufügen." ]
                ]
            , Html.div
                [ Attr.style "margin-bottom" "2rem" ]
                [ Html.label
                    [ Attr.style "display" "block"
                    , Attr.style "font-weight" "500"
                    , Attr.style "color" "#374151"
                    , Attr.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Anzahl benötigter Spieler pro Spiel *" ]
                , Html.input
                    [ Attr.type_ "number"
                    , Attr.value model.createTeamForm.playersNeeded
                    , Events.onInput (\playersNeeded -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = model.createTeamForm.creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = playersNeeded })
                    , Attr.placeholder "z.B. 11 für Fußball, 6 für Volleyball"
                    , Attr.min "1"
                    , Attr.max "50"
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "0.75rem"
                    , Attr.style "border" "1px solid #d1d5db"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    []
                , Html.p
                    [ Attr.style "color" "#6b7280"
                    , Attr.style "font-size" "0.875rem"
                    , Attr.style "margin-top" "0.5rem"
                    , Attr.style "margin-bottom" "0"
                    ]
                    [ Html.text "Diese Zahl bestimmt, wann ein Spiel als 'einsatzbereit' gilt." ]
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
                [ Html.text "Team erstellen" ]
            ]
        ]


viewTeamPage : Model -> Html FrontendMsg
viewTeamPage model =
    case model.currentTeam of
        Just team ->
            Html.div []
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "center"
                    , Attr.style "margin-bottom" "2rem"
                    , Attr.style "width" "100%"
                    ]
                    [ Html.h2
                        [ Attr.style "font-size" "1.75rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin" "0"
                        ]
                        [ Html.text team.name ]
                    , Html.button
                        [ Events.onClick ShowShareModal
                        , Attr.style "background-color" "#3b82f6"
                        , Attr.style "color" "white"
                        , Attr.style "border" "none"
                        , Attr.style "border-radius" "0.5rem"
                        , Attr.style "padding" "0.75rem 1rem"
                        , Attr.style "font-size" "0.875rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "transition" "background-color 0.2s"
                        , Attr.style "min-width" "44px"
                        , Attr.style "min-height" "44px"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "0.5rem"
                        ]
                        [ Html.text "↗️"
                        , Html.text "Teilen"
                        ]
                    ]
                , if List.isEmpty model.matches then
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
                            [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id) ]
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
                , if model.showCreateMemberModal then
                    viewCreateMemberModal model team

                  else
                    Html.text ""
                , if model.showMemberSelectionModal then
                    viewMemberSelectionModal model team

                  else
                    Html.text ""
                , if model.showChangeMatchDateModal then
                    viewChangeMatchDateModal model

                  else
                    Html.text ""
                , if model.showShareModal then
                    viewShareModal model team

                  else
                    Html.text ""
                ]

        Nothing ->
            Html.div
                [ Attr.style "text-align" "center"
                , Attr.style "padding" "3rem 0"
                ]
                [ Html.text "Team wird geladen..." ]


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


updateMemberFormName : String -> CreateMemberForm -> CreateMemberForm
updateMemberFormName name form =
    { form | name = name }



-- AVAILABILITY HELPERS


getMemberAvailabilityForMatch : String -> String -> List AvailabilityRecord -> Maybe Availability
getMemberAvailabilityForMatch memberId matchId availability =
    availability
        |> List.filter (\record -> record.memberId == memberId && record.matchId == matchId)
        |> List.head
        |> Maybe.map .availability


availabilityToString : Availability -> String
availabilityToString availability =
    case availability of
        Available ->
            "Verfügbar"

        NotAvailable ->
            "Nicht verfügbar"

        Maybe ->
            "Vielleicht"


availabilityToColor : Availability -> String
availabilityToColor availability =
    case availability of
        Available ->
            "#10b981"

        NotAvailable ->
            "#ef4444"

        Maybe ->
            "#f59e0b"


type alias AvailabilitySummary =
    { available : Int
    , notAvailable : Int
    , maybe : Int
    , total : Int
    }


type MatchStatus
    = Ready -- Available >= playersNeeded (Green)
    | Possible -- Available + Maybe >= playersNeeded (Yellow)
    | NotReady -- Available + Maybe < playersNeeded (Red)
    | Past -- Past matches (No color)


getMatchStatus : String -> String -> List Member -> List AvailabilityRecord -> Int -> MatchStatus
getMatchStatus matchId matchDate members availability playersNeeded =
    let
        summary =
            getMatchAvailabilitySummary matchId members availability

        today =
            "13.08.2025"

        -- Current date for testing (German format)
        -- Convert German date format (dd.mm.yyyy) to sortable format (yyyy-mm-dd)
        convertToSortable : String -> String
        convertToSortable dateStr =
            case String.split "." dateStr of
                [ day, month, year ] ->
                    year ++ "-" ++ String.padLeft 2 '0' month ++ "-" ++ String.padLeft 2 '0' day

                _ ->
                    dateStr

        -- Check if match is in the past
        isPastMatch =
            convertToSortable matchDate < convertToSortable today

        -- Check if match is less than 2 weeks in the future
        isLessThanTwoWeeksAway =
            let
                twoWeeksFromNow =
                    "27.08.2025"

                -- 2 weeks from today (German format)
            in
            convertToSortable matchDate >= convertToSortable today && convertToSortable matchDate <= convertToSortable twoWeeksFromNow
    in
    if isPastMatch then
        -- Past matches don't need status colors
        Past

    else if summary.available >= playersNeeded then
        Ready

    else if summary.available + summary.maybe >= playersNeeded then
        Possible

    else if isLessThanTwoWeeksAway then
        -- Only show NotReady for matches less than 2 weeks away
        NotReady

    else
        -- For matches more than 2 weeks away, show as Possible even if not enough votes
        Possible


matchStatusToColor : MatchStatus -> String
matchStatusToColor status =
    case status of
        Ready ->
            "#10b981"

        -- Green
        Possible ->
            "#f59e0b"

        -- Yellow/Orange
        NotReady ->
            "#ef4444"

        -- Red
        Past ->
            "#6b7280"



-- Gray (neutral)


matchStatusToBackgroundColor : MatchStatus -> String
matchStatusToBackgroundColor status =
    case status of
        Ready ->
            "#dcfce7"

        -- Light green
        Possible ->
            "#fef3c7"

        -- Light yellow
        NotReady ->
            "#fee2e2"

        -- Light red
        Past ->
            "#f9fafb"



-- Light gray (neutral)


getMatchAvailabilitySummary : String -> List Member -> List AvailabilityRecord -> AvailabilitySummary
getMatchAvailabilitySummary matchId members availability =
    let
        memberAvailability =
            members
                |> List.map
                    (\member ->
                        availability
                            |> List.filter (\record -> record.memberId == member.id && record.matchId == matchId)
                            |> List.head
                            |> Maybe.map .availability
                    )

        available =
            memberAvailability |> List.filter ((==) (Just Available)) |> List.length

        notAvailable =
            memberAvailability |> List.filter ((==) (Just NotAvailable)) |> List.length

        maybe =
            memberAvailability |> List.filter ((==) (Just Maybe)) |> List.length

        total =
            List.length members
    in
    { available = available
    , notAvailable = notAvailable
    , maybe = maybe
    , total = total
    }



-- TEAM PAGE HELPERS


viewMatchesSection : Model -> Team -> Html FrontendMsg
viewMatchesSection model team =
    let
        ( pastMatches, futureMatches ) =
            separatePastAndFutureMatches model.matches

        -- Sort past matches by oldest first (chronological order)
        sortedPastMatches =
            List.sortWith (\a b -> compare a.date b.date) pastMatches

        -- Sort future matches by earliest first (next match at top)
        sortedFutureMatches =
            List.sortWith (\a b -> compare a.date b.date) futureMatches

        pastMatchesToShow =
            List.take model.pastMatchesShown sortedPastMatches

        hasMorePastMatches =
            List.length sortedPastMatches > model.pastMatchesShown
    in
    Html.div
        [ Attr.style "margin-top" "2rem"
        , Attr.style "width" "100%"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "flex-start"
            , Attr.style "margin-bottom" "1rem"
            , Attr.style "gap" "1rem"
            , Attr.style "flex-wrap" "wrap"
            ]
            [ Html.h3
                [ Attr.style "font-size" "1.25rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                , Attr.style "flex" "1"
                , Attr.style "min-width" "0"
                ]
                [ Html.text "Spiele" ]
            , Html.button
                [ Events.onClick ShowCreateMatchModal
                , Attr.style "background-color" "#3b82f6"
                , Attr.style "color" "white"
                , Attr.style "padding" "0.75rem 1rem"
                , Attr.style "border" "none"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "font-size" "0.875rem"
                , Attr.style "font-weight" "500"
                , Attr.style "cursor" "pointer"
                , Attr.style "transition" "background-color 0.2s"
                , Attr.style "min-height" "44px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                ]
                [ Html.text "+"
                , Html.text "Spiel hinzufügen"
                ]
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
            Html.div []
                [ -- Past Matches Section
                  if List.isEmpty pastMatches then
                    Html.text ""

                  else
                    Html.div
                        [ Attr.style "margin-bottom" "2rem" ]
                        [ Html.div
                            [ Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "gap" "0.5rem"
                            , Attr.style "margin-bottom" "1rem"
                            , Attr.style "cursor" "pointer"
                            , Events.onClick TogglePastMatchesSection
                            ]
                            [ Html.h4
                                [ Attr.style "font-size" "1rem"
                                , Attr.style "font-weight" "600"
                                , Attr.style "color" "#6b7280"
                                , Attr.style "margin" "0"
                                ]
                                [ Html.text ("Vergangene Spiele (" ++ String.fromInt (List.length sortedPastMatches) ++ ")") ]
                            , Html.span
                                [ Attr.style "font-size" "0.875rem"
                                , Attr.style "color" "#9ca3af"
                                ]
                                [ Html.text
                                    (if model.pastMatchesExpanded then
                                        "▼"

                                     else
                                        "▶"
                                    )
                                ]
                            ]
                        , if model.pastMatchesExpanded then
                            Html.div []
                                [ Html.div
                                    [ Attr.style "width" "100%"
                                    ]
                                    (List.indexedMap
                                        (\index match ->
                                            viewMatchItem model team match (index == List.length pastMatchesToShow - 1)
                                        )
                                        pastMatchesToShow
                                    )
                                , if hasMorePastMatches then
                                    Html.div
                                        [ Attr.style "text-align" "center"
                                        , Attr.style "margin-top" "1rem"
                                        ]
                                        [ Html.button
                                            [ Events.onClick LoadMorePastMatches
                                            , Attr.style "background-color" "#f3f4f6"
                                            , Attr.style "color" "#374151"
                                            , Attr.style "padding" "0.5rem 1rem"
                                            , Attr.style "border" "1px solid #d1d5db"
                                            , Attr.style "border-radius" "0.375rem"
                                            , Attr.style "font-size" "0.875rem"
                                            , Attr.style "font-weight" "500"
                                            , Attr.style "cursor" "pointer"
                                            , Attr.style "transition" "background-color 0.2s"
                                            ]
                                            [ Html.text ("Weitere " ++ String.fromInt (min 10 (List.length sortedPastMatches - model.pastMatchesShown)) ++ " Spiele laden") ]
                                        ]

                                  else
                                    Html.text ""
                                ]

                          else
                            Html.text ""
                        ]
                , -- Future Matches Section
                  if List.isEmpty futureMatches then
                    Html.text ""

                  else
                    Html.div []
                        [ Html.h4
                            [ Attr.style "font-size" "1rem"
                            , Attr.style "font-weight" "600"
                            , Attr.style "color" "#6b7280"
                            , Attr.style "margin" "0 0 1rem 0"
                            ]
                            [ Html.text "Kommende Spiele" ]
                        , Html.div
                            [ Attr.style "width" "100%"
                            ]
                            (List.indexedMap
                                (\index match ->
                                    viewMatchItem model team match (index == List.length sortedFutureMatches - 1)
                                )
                                sortedFutureMatches
                            )
                        ]
                ]
        ]


viewMatchItem : Model -> Team -> Match -> Bool -> Html FrontendMsg
viewMatchItem model team match isLast =
    let
        isExpanded =
            List.member match.id model.expandedMatches

        matchStatus =
            getMatchStatus match.id match.date model.members model.availability team.playersNeeded

        statusBackgroundColor =
            matchStatusToBackgroundColor matchStatus

        statusBorderColor =
            matchStatusToColor matchStatus
    in
    Html.div
        [ Attr.style "border-left" ("4px solid " ++ statusBorderColor)
        , Attr.style "background-color" statusBackgroundColor
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "margin-bottom"
            (if isLast then
                "0"

             else
                "1rem"
            )
        , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
        ]
        [ Html.div
            [ Attr.style "padding" "1rem 1.5rem"
            ]
            [ Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "0.75rem"
                ]
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "flex-start"
                    , Attr.style "gap" "1rem"
                    , Attr.style "flex-wrap" "wrap"
                    ]
                    [ Html.div
                        [ Attr.style "flex" "1"
                        , Attr.style "min-width" "0"
                        ]
                        [ Html.h4
                            [ Attr.style "font-weight" "600"
                            , Attr.style "color" "#1e293b"
                            , Attr.style "margin" "0 0 0.25rem 0"
                            ]
                            [ Html.text
                                (if match.isHome then
                                    "🏠 " ++ match.opponent

                                 else
                                    "🚗 " ++ match.opponent
                                )
                            ]
                        , Html.p
                            [ Attr.style "color" "#64748b"
                            , Attr.style "margin" "0 0 0.5rem 0"
                            , Attr.style "font-size" "0.875rem"
                            ]
                            [ Html.text (isoToGermanDate match.date ++ " um " ++ match.time) ]
                        , Html.button
                            [ Events.onClick (ShowChangeMatchDateModal match.id)
                            , Attr.style "background-color" "#f3f4f6"
                            , Attr.style "border" "1px solid #d1d5db"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "padding" "0.5rem 0.75rem"
                            , Attr.style "font-size" "0.75rem"
                            , Attr.style "color" "#374151"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "min-height" "44px"
                            , Attr.style "white-space" "nowrap"
                            , Attr.style "transition" "background-color 0.2s"
                            , Attr.style "margin-bottom" "0.5rem"
                            ]
                            [ Html.text "📅 Datum ändern" ]
                        , Html.p
                            [ Attr.style "color" "#64748b"
                            , Attr.style "margin" "0 0 0.5rem 0"
                            , Attr.style "font-size" "0.875rem"
                            ]
                            [ Html.text match.venue ]
                        ]
                    , Html.div
                        [ Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "align-items" "flex-end"
                        , Attr.style "gap" "0.5rem"
                        ]
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
                        , case model.activeMemberId of
                            Just activeMemberId ->
                                viewAvailabilityControls activeMemberId match.id (getMemberAvailabilityForMatch activeMemberId match.id model.availability)

                            Nothing ->
                                Html.text ""
                        ]
                    ]
                , Html.div
                    [ Attr.style "margin-top" "0.5rem"
                    , Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "center"
                    ]
                    [ viewAvailabilityOverview match.id model.members model.availability
                    , Html.button
                        [ Events.onClick (ToggleMatchDetails match.id)
                        , Attr.style "background-color" "#f8fafc"
                        , Attr.style "border" "1px solid #e2e8f0"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "padding" "0.5rem"
                        , Attr.style "font-size" "1rem"
                        , Attr.style "color" "#64748b"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "min-height" "44px"
                        , Attr.style "min-width" "44px"
                        , Attr.style "transition" "background-color 0.2s"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        ]
                        [ Html.text
                            (if isExpanded then
                                "👁️"

                             else
                                "👥"
                            )
                        ]
                    ]
                ]
            ]
        , if isExpanded then
            viewMatchDetailsExpanded match.id model.members model.availability

          else
            Html.text ""
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
            , Attr.style "border-radius" "0.75rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "95%"
            , Attr.style "margin" "1rem"
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
                            [ Attr.type_ "text"
                            , Attr.value model.createMatchForm.date
                            , Events.onInput (\date -> CreateMatchFormUpdated (updateMatchFormDate date model.createMatchForm))
                            , Attr.placeholder "25.12.2024"
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
                    , Attr.style "padding" "0.875rem 1.5rem"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "0.5rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "font-weight" "500"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "transition" "background-color 0.2s"
                    , Attr.style "min-height" "48px"
                    ]
                    [ Html.text "Spiel hinzufügen" ]
                ]
            ]
        ]


viewMembersSection : Model -> Team -> Html FrontendMsg
viewMembersSection model team =
    Html.div
        [ Attr.style "margin-top" "2rem" ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "flex-start"
            , Attr.style "margin-bottom" "1rem"
            , Attr.style "gap" "1rem"
            , Attr.style "flex-wrap" "wrap"
            ]
            [ Html.h3
                [ Attr.style "font-size" "1.25rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                , Attr.style "flex" "1"
                , Attr.style "min-width" "0"
                ]
                [ Html.text "Teammitglieder" ]
            , Html.button
                [ Events.onClick ShowCreateMemberModal
                , Attr.style "background-color" "#10b981"
                , Attr.style "color" "white"
                , Attr.style "padding" "0.75rem 1rem"
                , Attr.style "border" "none"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "font-size" "0.875rem"
                , Attr.style "font-weight" "500"
                , Attr.style "cursor" "pointer"
                , Attr.style "transition" "background-color 0.2s"
                , Attr.style "min-height" "44px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                ]
                [ Html.text "+"
                , Html.text "Mitglied hinzufügen"
                ]
            ]
        , if List.isEmpty model.members then
            Html.div
                [ Attr.style "background-color" "white"
                , Attr.style "padding" "2rem"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
                , Attr.style "text-align" "center"
                ]
                [ Html.p
                    [ Attr.style "color" "#64748b" ]
                    [ Html.text "Noch keine Mitglieder hinzugefügt. Füge das erste Mitglied hinzu!" ]
                ]

          else
            Html.div
                [ Attr.style "display" "grid"
                , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(250px, 1fr))"
                , Attr.style "gap" "0.75rem"
                ]
                (List.map viewMemberCard model.members)
        ]


viewMemberCard : Member -> Html FrontendMsg
viewMemberCard member =
    Html.div
        [ Attr.style "background-color" "white"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
        , Attr.style "padding" "1rem"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "margin-bottom" "1rem"
            ]
            [ Html.div
                [ Attr.style "width" "3rem"
                , Attr.style "height" "3rem"
                , Attr.style "border-radius" "50%"
                , Attr.style "background-color" "#e5e7eb"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "margin-right" "1rem"
                ]
                [ Html.span
                    [ Attr.style "font-weight" "600"
                    , Attr.style "color" "#374151"
                    , Attr.style "font-size" "1.25rem"
                    ]
                    [ Html.text (String.left 1 member.name |> String.toUpper) ]
                ]
            , Html.div []
                [ Html.h4
                    [ Attr.style "font-weight" "600"
                    , Attr.style "color" "#1e293b"
                    , Attr.style "margin" "0 0 0.25rem 0"
                    , Attr.style "font-size" "1.125rem"
                    ]
                    [ Html.text member.name ]
                ]
            ]
        ]


viewCreateMemberModal : Model -> Team -> Html FrontendMsg
viewCreateMemberModal model team =
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
        , Events.onClick HideCreateMemberModal
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
                        [ Html.text "Mitglied hinzufügen" ]
                    , Html.button
                        [ Events.onClick HideCreateMemberModal
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
                [ Events.onSubmit (CreateMemberSubmitted team.id)
                , Attr.style "padding" "1.5rem"
                ]
                [ Html.div
                    [ Attr.style "margin-bottom" "2rem" ]
                    [ Html.label
                        [ Attr.style "display" "block"
                        , Attr.style "font-weight" "500"
                        , Attr.style "color" "#374151"
                        , Attr.style "margin-bottom" "0.5rem"
                        ]
                        [ Html.text "Name *" ]
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.value model.createMemberForm.name
                        , Events.onInput (\name -> CreateMemberFormUpdated (updateMemberFormName name model.createMemberForm))
                        , Attr.placeholder "Name eingeben"
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
                    , Attr.style "background-color" "#10b981"
                    , Attr.style "color" "white"
                    , Attr.style "padding" "0.875rem 1.5rem"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "0.5rem"
                    , Attr.style "font-size" "1rem"
                    , Attr.style "font-weight" "500"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "transition" "background-color 0.2s"
                    , Attr.style "min-height" "48px"
                    ]
                    [ Html.text "Mitglied hinzufügen" ]
                ]
            ]
        ]


viewMemberSelectionModal : Model -> Team -> Html FrontendMsg
viewMemberSelectionModal model team =
    if model.showCreateMemberInModal then
        viewCreateMemberInModal model team

    else
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
            ]
            [ Html.div
                [ Attr.style "background-color" "white"
                , Attr.style "border-radius" "0.5rem"
                , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
                , Attr.style "max-width" "500px"
                , Attr.style "width" "90%"
                , Attr.style "max-height" "90vh"
                , Attr.style "overflow-y" "auto"
                ]
                [ Html.div
                    [ Attr.style "padding" "1.5rem"
                    , Attr.style "border-bottom" "1px solid #e5e7eb"
                    ]
                    [ Html.h3
                        [ Attr.style "font-size" "1.25rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin" "0"
                        , Attr.style "text-align" "center"
                        ]
                        [ Html.text "Wer bist du?" ]
                    , Html.p
                        [ Attr.style "color" "#64748b"
                        , Attr.style "margin-top" "0.5rem"
                        , Attr.style "margin-bottom" "0"
                        , Attr.style "text-align" "center"
                        ]
                        [ Html.text "Wähle deinen Namen aus der Liste der Mannschaftsmitglieder." ]
                    ]
                , Html.div
                    [ Attr.style "padding" "1.5rem" ]
                    [ Html.div
                        [ Attr.style "display" "grid"
                        , Attr.style "gap" "0.75rem"
                        ]
                        (List.map viewMemberSelectionItem model.members)
                    , Html.div
                        [ Attr.style "margin-top" "1rem"
                        , Attr.style "padding-top" "1rem"
                        , Attr.style "border-top" "1px solid #e5e7eb"
                        ]
                        [ Html.button
                            [ Events.onClick ShowCreateMemberInModal
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border" "1px solid #3b82f6"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "background-color" "white"
                            , Attr.style "color" "#3b82f6"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "transition" "all 0.2s"
                            , Attr.style "font-weight" "500"
                            ]
                            [ Html.text "➕ Mitglied hinzufügen" ]
                        ]
                    ]
                ]
            ]


viewCreateMemberInModal : Model -> Team -> Html FrontendMsg
viewCreateMemberInModal model team =
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
        ]
        [ Html.div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.5rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "90%"
            , Attr.style "max-height" "90vh"
            , Attr.style "overflow-y" "auto"
            ]
            [ Html.div
                [ Attr.style "padding" "1.5rem"
                , Attr.style "border-bottom" "1px solid #e5e7eb"
                ]
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "0.5rem"
                    ]
                    [ Html.button
                        [ Events.onClick HideCreateMemberInModal
                        , Attr.style "background" "none"
                        , Attr.style "border" "none"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "padding" "0.25rem"
                        , Attr.style "border-radius" "0.25rem"
                        , Attr.style "color" "#6b7280"
                        ]
                        [ Html.text "←" ]
                    , Html.h3
                        [ Attr.style "font-size" "1.25rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin" "0"
                        ]
                        [ Html.text "Mitglied hinzufügen" ]
                    ]
                ]
            , Html.div
                [ Attr.style "padding" "1.5rem" ]
                [ Html.div
                    [ Attr.style "margin-bottom" "1rem" ]
                    [ Html.label
                        [ Attr.style "display" "block"
                        , Attr.style "font-weight" "500"
                        , Attr.style "color" "#374151"
                        , Attr.style "margin-bottom" "0.5rem"
                        ]
                        [ Html.text "Name *" ]
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.placeholder "Dein Name"
                        , Attr.value model.createMemberForm.name
                        , Events.onInput (\value -> CreateMemberFormUpdated { name = value })
                        , Attr.style "width" "100%"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "font-size" "1rem"
                        ]
                        []
                    ]
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "gap" "0.75rem"
                    , Attr.style "margin-top" "1.5rem"
                    ]
                    [ Html.button
                        [ Events.onClick HideCreateMemberInModal
                        , Attr.style "flex" "1"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "white"
                        , Attr.style "color" "#374151"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "font-weight" "500"
                        ]
                        [ Html.text "Abbrechen" ]
                    , Html.button
                        [ Events.onClick (CreateMemberSubmitted team.id)
                        , Attr.style "flex" "1"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "none"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "#3b82f6"
                        , Attr.style "color" "white"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "font-weight" "500"
                        , Attr.disabled (String.isEmpty model.createMemberForm.name)
                        ]
                        [ Html.text "Hinzufügen" ]
                    ]
                ]
            ]
        ]


viewChangeMatchDateModal : Model -> Html FrontendMsg
viewChangeMatchDateModal model =
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
        , Events.onClick HideChangeMatchDateModal
        ]
        [ Html.div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.75rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "95%"
            , Attr.style "margin" "1rem"
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
                        [ Html.text "Spieldatum ändern" ]
                    , Html.button
                        [ Events.onClick HideChangeMatchDateModal
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
                [ Events.onSubmit
                    (case model.changeMatchDateMatchId of
                        Just matchId ->
                            ChangeMatchDateSubmitted matchId model.changeMatchDateForm

                        Nothing ->
                            NoOpFrontendMsg
                    )
                , Attr.style "padding" "1.5rem"
                ]
                [ Html.div
                    [ Attr.style "margin-bottom" "1.5rem" ]
                    [ Html.label
                        [ Attr.style "display" "block"
                        , Attr.style "font-weight" "500"
                        , Attr.style "color" "#374151"
                        , Attr.style "margin-bottom" "0.5rem"
                        ]
                        [ Html.text "Neues Datum" ]
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.value model.changeMatchDateForm
                        , Events.onInput
                            (\newDate ->
                                case model.changeMatchDateMatchId of
                                    Just matchId ->
                                        ChangeMatchDateFormUpdated matchId newDate

                                    Nothing ->
                                        NoOpFrontendMsg
                            )
                        , Attr.placeholder "25.12.2024"
                        , Attr.style "width" "100%"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "font-size" "1rem"
                        ]
                        []
                    ]
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "gap" "1rem"
                    , Attr.style "justify-content" "flex-end"
                    ]
                    [ Html.button
                        [ Attr.type_ "button"
                        , Events.onClick HideChangeMatchDateModal
                        , Attr.style "padding" "0.75rem 1.5rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "white"
                        , Attr.style "color" "#374151"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "font-weight" "500"
                        ]
                        [ Html.text "Abbrechen" ]
                    , Html.button
                        [ Attr.type_ "submit"
                        , Attr.style "padding" "0.75rem 1.5rem"
                        , Attr.style "border" "none"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "#ef4444"
                        , Attr.style "color" "white"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "font-weight" "500"
                        , Attr.disabled (String.isEmpty model.changeMatchDateForm)
                        ]
                        [ Html.text "Datum ändern" ]
                    ]
                ]
            ]
        ]


viewMemberSelectionItem : Member -> Html FrontendMsg
viewMemberSelectionItem member =
    Html.button
        [ Events.onClick (SelectActiveMember member.id)
        , Attr.style "width" "100%"
        , Attr.style "padding" "1rem"
        , Attr.style "border" "1px solid #e5e7eb"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background-color" "white"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 0.2s"
        , Attr.style "text-align" "left"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "1rem"
        ]
        [ Html.div
            [ Attr.style "width" "2.5rem"
            , Attr.style "height" "2.5rem"
            , Attr.style "border-radius" "50%"
            , Attr.style "background-color" "#e5e7eb"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "flex-shrink" "0"
            ]
            [ Html.span
                [ Attr.style "font-weight" "600"
                , Attr.style "color" "#374151"
                , Attr.style "font-size" "1.125rem"
                ]
                [ Html.text (String.left 1 member.name |> String.toUpper) ]
            ]
        , Html.div []
            [ Html.h4
                [ Attr.style "font-weight" "600"
                , Attr.style "color" "#1e293b"
                , Attr.style "margin" "0"
                , Attr.style "font-size" "1rem"
                ]
                [ Html.text member.name ]
            ]
        ]


viewAvailabilityControls : String -> String -> Maybe Availability -> Html FrontendMsg
viewAvailabilityControls memberId matchId currentAvailability =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "0.5rem"
        , Attr.style "margin-top" "0.5rem"
        , Attr.style "justify-content" "flex-end"
        ]
        [ viewAvailabilityButton memberId matchId Available "✓" (currentAvailability == Just Available)
        , viewAvailabilityButton memberId matchId Maybe "?" (currentAvailability == Just Maybe)
        , viewAvailabilityButton memberId matchId NotAvailable "✗" (currentAvailability == Just NotAvailable)
        ]


viewAvailabilityButton : String -> String -> Availability -> String -> Bool -> Html FrontendMsg
viewAvailabilityButton memberId matchId availability icon isSelected =
    Html.button
        [ Events.stopPropagationOn "click" (Json.Decode.succeed ( SetAvailability memberId matchId availability, True ))
        , Attr.style "background-color"
            (if isSelected then
                availabilityToColor availability

             else
                "#f8fafc"
            )
        , Attr.style "color"
            (if isSelected then
                "white"

             else
                "#64748b"
            )
        , Attr.style "border"
            (if isSelected then
                "1px solid " ++ availabilityToColor availability

             else
                "1px solid #e2e8f0"
            )
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "width" "44px"
        , Attr.style "height" "44px"
        , Attr.style "min-width" "44px"
        , Attr.style "min-height" "44px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "cursor" "pointer"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "transition" "all 0.2s"
        , Attr.title (availabilityToString availability)
        ]
        [ Html.text icon ]


viewAvailabilityOverview : String -> List Member -> List AvailabilityRecord -> Html FrontendMsg
viewAvailabilityOverview matchId members availability =
    let
        summary =
            getMatchAvailabilitySummary matchId members availability

        noResponses =
            summary.total - summary.available - summary.notAvailable - summary.maybe
    in
    if summary.total == 0 then
        Html.text ""

    else
        Html.div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "margin-top" "0.5rem"
            ]
            [ Html.span
                [ Attr.style "font-size" "0.75rem"
                , Attr.style "color" "#6b7280"
                , Attr.style "font-weight" "500"
                ]
                [ Html.text "Verfügbarkeit:" ]
            , Html.div
                [ Attr.style "display" "flex"
                , Attr.style "gap" "0.25rem"
                , Attr.style "align-items" "center"
                ]
                [ if summary.available > 0 then
                    viewAvailabilitySummaryBadge "✓" (String.fromInt summary.available) "#10b981"

                  else
                    Html.text ""
                , if summary.maybe > 0 then
                    viewAvailabilitySummaryBadge "?" (String.fromInt summary.maybe) "#f59e0b"

                  else
                    Html.text ""
                , if summary.notAvailable > 0 then
                    viewAvailabilitySummaryBadge "✗" (String.fromInt summary.notAvailable) "#ef4444"

                  else
                    Html.text ""
                , if noResponses > 0 then
                    viewAvailabilitySummaryBadge "?" (String.fromInt noResponses) "#9ca3af"

                  else
                    Html.text ""
                ]
            ]


viewAvailabilitySummaryBadge : String -> String -> String -> Html FrontendMsg
viewAvailabilitySummaryBadge icon count color =
    Html.span
        [ Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "background-color" color
        , Attr.style "color" "white"
        , Attr.style "padding" "0.125rem 0.375rem"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "font-size" "0.75rem"
        , Attr.style "font-weight" "600"
        ]
        [ Html.span [] [ Html.text icon ]
        , Html.span [] [ Html.text count ]
        ]


viewMatchDetailsExpanded : String -> List Member -> List AvailabilityRecord -> Html FrontendMsg
viewMatchDetailsExpanded matchId members availability =
    let
        groupedMembers =
            members
                |> List.map
                    (\member ->
                        let
                            memberAvailability =
                                availability
                                    |> List.filter (\record -> record.memberId == member.id && record.matchId == matchId)
                                    |> List.head
                                    |> Maybe.map .availability
                        in
                        ( member, memberAvailability )
                    )
                |> List.foldl
                    (\( member, maybeAvailability ) acc ->
                        case maybeAvailability of
                            Just Available ->
                                { acc | available = member :: acc.available }

                            Just NotAvailable ->
                                { acc | notAvailable = member :: acc.notAvailable }

                            Just Maybe ->
                                { acc | maybe = member :: acc.maybe }

                            Nothing ->
                                { acc | noResponse = member :: acc.noResponse }
                    )
                    { available = [], notAvailable = [], maybe = [], noResponse = [] }
    in
    Html.div
        [ Attr.style "background-color" "#f8fafc"
        , Attr.style "padding" "1rem 1.5rem"
        , Attr.style "border-top" "1px solid #e2e8f0"
        ]
        [ Html.h5
            [ Attr.style "font-size" "0.875rem"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#374151"
            , Attr.style "margin" "0 0 1rem 0"
            ]
            [ Html.text "Verfügbarkeit der Mitglieder" ]
        , Html.div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "repeat(auto-fit, minmax(200px, 1fr))"
            , Attr.style "gap" "1rem"
            ]
            [ viewAvailabilityGroup "Verfügbar" "#10b981" "✓" groupedMembers.available
            , viewAvailabilityGroup "Vielleicht" "#f59e0b" "?" groupedMembers.maybe
            , viewAvailabilityGroup "Nicht verfügbar" "#ef4444" "✗" groupedMembers.notAvailable
            , viewAvailabilityGroup "Keine Antwort" "#9ca3af" "?" groupedMembers.noResponse
            ]
        ]


viewAvailabilityGroup : String -> String -> String -> List Member -> Html FrontendMsg
viewAvailabilityGroup title color icon members =
    Html.div
        [ Attr.style "background-color" "white"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "padding" "0.75rem"
        , Attr.style "border" ("1px solid " ++ color)
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "margin-bottom" "0.5rem"
            ]
            [ Html.span
                [ Attr.style "color" color
                , Attr.style "font-weight" "600"
                ]
                [ Html.text icon ]
            , Html.h6
                [ Attr.style "font-size" "0.75rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" color
                , Attr.style "margin" "0"
                ]
                [ Html.text (title ++ " (" ++ String.fromInt (List.length members) ++ ")") ]
            ]
        , if List.isEmpty members then
            Html.p
                [ Attr.style "color" "#9ca3af"
                , Attr.style "font-size" "0.75rem"
                , Attr.style "margin" "0"
                , Attr.style "font-style" "italic"
                ]
                [ Html.text "Keine Mitglieder" ]

          else
            Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "0.25rem"
                ]
                (List.map viewMemberInGroup members)
        ]


viewMemberInGroup : Member -> Html FrontendMsg
viewMemberInGroup member =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        ]
        [ Html.div
            [ Attr.style "width" "1.5rem"
            , Attr.style "height" "1.5rem"
            , Attr.style "border-radius" "50%"
            , Attr.style "background-color" "#e5e7eb"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "flex-shrink" "0"
            ]
            [ Html.span
                [ Attr.style "font-weight" "600"
                , Attr.style "color" "#374151"
                , Attr.style "font-size" "0.625rem"
                ]
                [ Html.text (String.left 1 member.name |> String.toUpper) ]
            ]
        , Html.span
            [ Attr.style "font-size" "0.75rem"
            , Attr.style "color" "#374151"
            ]
            [ Html.text member.name ]
        ]


viewShareSection : Model -> Team -> Html FrontendMsg
viewShareSection model team =
    Html.div
        [ Attr.style "margin-top" "2rem"
        , Attr.style "background-color" "white"
        , Attr.style "padding" "1.5rem"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
        ]
        [ Html.h3
            [ Attr.style "font-size" "1.125rem"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#1e293b"
            , Attr.style "margin" "0 0 1rem 0"
            ]
            [ Html.text "Team teilen" ]
        , Html.p
            [ Attr.style "color" "#64748b"
            , Attr.style "margin-bottom" "1rem"
            , Attr.style "font-size" "0.875rem"
            ]
            [ Html.text "Teile diesen Link mit deinen Mannschaftsmitgliedern, damit sie Zugang zur Mannschaft haben:" ]
        , Html.div
            [ Attr.style "background-color" "#f8fafc"
            , Attr.style "padding" "1rem"
            , Attr.style "border-radius" "0.375rem"
            , Attr.style "border" "1px solid #e2e8f0"
            , Attr.style "font-family" "monospace"
            , Attr.style "font-size" "0.875rem"
            , Attr.style "word-break" "break-all"
            , Attr.style "margin-bottom" "1rem"
            ]
            [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id) ]
        , Html.p
            [ Attr.style "color" "#64748b"
            , Attr.style "font-size" "0.875rem"
            , Attr.style "margin" "0"
            ]
            [ Html.text "💡 Tipp: Speichere diese Seite als Lesezeichen für einfachen Zugang." ]
        ]


viewShareModal : Model -> Team -> Html FrontendMsg
viewShareModal model team =
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
        , Events.onClick HideShareModal
        ]
        [ Html.div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.75rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "500px"
            , Attr.style "width" "95%"
            , Attr.style "margin" "1rem"
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
                        [ Html.text "Team teilen" ]
                    , Html.button
                        [ Events.onClick HideShareModal
                        , Attr.style "background" "none"
                        , Attr.style "border" "none"
                        , Attr.style "font-size" "1.5rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "color" "#6b7280"
                        ]
                        [ Html.text "×" ]
                    ]
                ]
            , Html.div
                [ Attr.style "padding" "1.5rem" ]
                [ Html.p
                    [ Attr.style "color" "#64748b"
                    , Attr.style "margin-bottom" "1rem"
                    , Attr.style "font-size" "0.875rem"
                    ]
                    [ Html.text "Teile diesen Link mit deinen Mannschaftsmitgliedern, damit sie Zugang zur Mannschaft haben:" ]
                , Html.div
                    [ Attr.style "background-color" "#f8fafc"
                    , Attr.style "padding" "1rem"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "border" "1px solid #e2e8f0"
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "0.875rem"
                    , Attr.style "word-break" "break-all"
                    , Attr.style "margin-bottom" "1rem"
                    ]
                    [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id) ]
                , Html.div
                    [ Attr.style "text-align" "center"
                    , Attr.style "margin-bottom" "1rem"
                    ]
                    [ Html.div
                        [ Attr.style "display" "inline-block"
                        , Attr.style "background-color" "white"
                        , Attr.style "padding" "1rem"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "border" "1px solid #e2e8f0"
                        ]
                        [ case QRCode.fromString (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id) of
                            Ok qrCode ->
                                QRCode.toSvg
                                    [ Attr.style "width" "200px"
                                    , Attr.style "height" "200px"
                                    , Attr.style "display" "block"
                                    , Attr.style "margin" "0 auto"
                                    ]
                                    qrCode

                            Err error ->
                                Html.div
                                    [ Attr.style "color" "red"
                                    , Attr.style "font-size" "0.875rem"
                                    ]
                                    [ Html.text "QR Code Fehler: " ]
                        ]
                    ]
                , Html.p
                    [ Attr.style "color" "#64748b"
                    , Attr.style "font-size" "0.875rem"
                    , Attr.style "margin" "0"
                    ]
                    [ Html.text "💡 Tipp: Speichere diese Seite als Lesezeichen für einfachen Zugang." ]
                ]
            ]
        ]
