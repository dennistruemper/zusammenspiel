module Frontend exposing (..)

import Basics exposing (min)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import IcsParser exposing (ParsedMatch, parseIcsToMatches)
import Json.Decode
import Lamdera
import LocalStorage
import QRCode
import Task
import Time
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils exposing (createTeamUrl, extractAccessCodeFromUrl, extractTeamIdFromUrl, isoToGermanDate, separatePastAndFutureMatches, sortMatchesByDate)
import View.Dialog as Dialog


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.batch [ LocalStorage.fromJS LocalStorageMessage, Time.every (30 * 60 * 1000) (\_ -> UpdateCurrentDate) ]
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
            , createTeamForm = { name = "", creatorName = "", otherMemberNames = "", playersNeeded = "", accessCode = "" }
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
            , showImportIcsModal = False
            , showAddMatchDropdown = False
            , icsImportUrl = ""
            , icsImportStatus = Nothing
            , parsedIcsMatches = []
            , allParsedIcsMatches = []
            , icsImportSelectedMatches = Dict.empty
            , expandedMatches = []
            , expandedPredictions = []
            , pastMatchesShown = 10
            , pastMatchesExpanded = False
            , matches = []
            , members = []
            , availability = []
            , hostname = Nothing
            , currentDate = Nothing
            , confirmedTeamCodes = Dict.empty
            , accessCodeRequired = Nothing
            , enteredAccessCode = ""
            , datePredictions = Dict.empty
            , showDatePredictionModal = False
            , datePredictionMatchId = Nothing
            , datePredictionForm = ""
            }

        ( updatedModel, cmd ) =
            case page of
                TeamPage teamId ->
                    let
                        urlAccessCode =
                            extractAccessCodeFromUrl (Url.toString url)
                                |> Maybe.withDefault ""

                        modelWithAccessCode =
                            if String.length urlAccessCode == 4 then
                                { initialModel
                                    | confirmedTeamCodes = Dict.insert teamId urlAccessCode initialModel.confirmedTeamCodes
                                }

                            else
                                initialModel
                    in
                    ( modelWithAccessCode
                    , Cmd.batch
                        [ LocalStorage.toJS "GET_HOSTNAME"
                        , LocalStorage.toJS "GET_CURRENT_DATE"
                        , if String.length urlAccessCode == 4 then
                            Lamdera.sendToBackend (GetTeamRequest teamId urlAccessCode)

                          else
                            LocalStorage.toJS ("GET_ACCESS_CODE:" ++ teamId)
                        ]
                    )

                _ ->
                    ( initialModel
                    , Cmd.batch
                        [ LocalStorage.toJS "GET_HOSTNAME"
                        , LocalStorage.toJS "GET_CURRENT_DATE"
                        ]
                    )
    in
    ( updatedModel, cmd )


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

                ( updatedModel, cmd ) =
                    case newPage of
                        TeamPage teamId ->
                            let
                                urlAccessCode =
                                    extractAccessCodeFromUrl (Url.toString url)
                                        |> Maybe.withDefault ""

                                -- Priority: URL > localStorage > manual entry
                                modelWithAccessCode =
                                    if String.length urlAccessCode == 4 then
                                        { model
                                            | accessCodeRequired = Nothing
                                            , confirmedTeamCodes = Dict.insert teamId urlAccessCode model.confirmedTeamCodes
                                        }

                                    else
                                        model
                            in
                            ( { modelWithAccessCode | page = newPage }
                            , if String.length urlAccessCode == 4 then
                                Lamdera.sendToBackend (GetTeamRequest teamId urlAccessCode)

                              else
                                LocalStorage.toJS ("GET_ACCESS_CODE:" ++ teamId)
                            )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { updatedModel | page = newPage }, cmd )

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
                , Lamdera.sendToBackend (CreateTeamRequest model.createTeamForm.name model.createTeamForm.creatorName otherMemberNames playersNeeded "")
                )

        CreateMatchFormUpdated form ->
            ( { model | createMatchForm = form }, Cmd.none )

        CreateMatchSubmitted teamId ->
            if String.isEmpty (String.trim model.createMatchForm.opponent) then
                ( model, Cmd.none )

            else
                ( { model | showCreateMatchModal = False, createMatchForm = { opponent = "", date = "", time = "", venue = "", isHome = True } }
                , Lamdera.sendToBackend (CreateMatchRequest teamId model.createMatchForm (Dict.get teamId model.confirmedTeamCodes |> Maybe.withDefault ""))
                )

        ShowCreateMatchModal ->
            ( { model | showCreateMatchModal = True, showAddMatchDropdown = False }, Cmd.none )

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
                , Lamdera.sendToBackend (CreateMemberRequest teamId model.createMemberForm (Dict.get teamId model.confirmedTeamCodes |> Maybe.withDefault ""))
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

            else if String.startsWith "CURRENT_DATE:" message then
                let
                    currentDate =
                        String.dropLeft 13 message
                in
                ( { model | currentDate = Just currentDate }, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateCurrentDate ->
            -- Periodically update the current date
            ( model, LocalStorage.toJS "GET_CURRENT_DATE" )

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
                        case model.currentTeam of
                            Just team ->
                                let
                                    accessCode =
                                        Dict.get team.id model.confirmedTeamCodes
                                            |> Maybe.withDefault ""
                                in
                                ( model
                                , Lamdera.sendToBackend (UpdateAvailabilityRequest memberId matchId availability accessCode)
                                )

                            Nothing ->
                                ( model, Cmd.none )

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

        TogglePredictionDetails matchId predictedDate ->
            let
                predictionKey =
                    ( matchId, predictedDate )

                updatedExpandedPredictions =
                    if List.member predictionKey model.expandedPredictions then
                        List.filter ((/=) predictionKey) model.expandedPredictions

                    else
                        predictionKey :: model.expandedPredictions
            in
            ( { model | expandedPredictions = updatedExpandedPredictions }, Cmd.none )

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
                    , Lamdera.sendToBackend (ChangeMatchDateRequest matchId newDate team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault ""))
                    )

                Nothing ->
                    ( model, Cmd.none )

        ShowShareModal ->
            ( { model | showShareModal = True }, Cmd.none )

        HideShareModal ->
            ( { model | showShareModal = False }, Cmd.none )

        ShowImportIcsModal ->
            ( { model | showImportIcsModal = True, icsImportStatus = Nothing, showAddMatchDropdown = False }, Cmd.none )

        HideImportIcsModal ->
            ( { model | showImportIcsModal = False, icsImportUrl = "", icsImportStatus = Nothing, parsedIcsMatches = [], allParsedIcsMatches = [], icsImportSelectedMatches = Dict.empty, showAddMatchDropdown = False }, Cmd.none )

        ToggleAddMatchDropdown ->
            ( { model | showAddMatchDropdown = not model.showAddMatchDropdown }, Cmd.none )

        IcsImportMatchToggled index selected ->
            let
                updatedSelectedMatches =
                    Dict.insert index selected model.icsImportSelectedMatches

                selectedCount =
                    model.allParsedIcsMatches
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( idx, _ ) ->
                                if Dict.get idx updatedSelectedMatches |> Maybe.withDefault False then
                                    Just idx

                                else
                                    Nothing
                            )
                        |> List.length

                totalCount =
                    List.length model.allParsedIcsMatches

                statusMessage =
                    if selectedCount == 0 then
                        "✓ " ++ String.fromInt totalCount ++ " Spiele gefunden. Bitte wählen Sie mindestens ein Spiel zum Importieren aus."

                    else
                        "✓ " ++ String.fromInt totalCount ++ " Spiele gefunden. " ++ String.fromInt selectedCount ++ " zum Importieren ausgewählt."
            in
            ( { model
                | icsImportSelectedMatches = updatedSelectedMatches
                , icsImportStatus = Just statusMessage
              }
            , Cmd.none
            )

        IcsFileSelectButtonClicked ->
            ( { model | icsImportStatus = Just "Lade Datei..." }
            , Select.file [ "text/calendar", "text/plain" ] IcsFileSelected
            )

        IcsFileSelected file ->
            ( { model | icsImportStatus = Just "Verarbeite ICS-Datei..." }
            , Task.perform IcsFileContentRead (File.toString file)
            )

        IcsFileContentRead content ->
            -- Content should be non-empty when file is read
            if String.isEmpty content then
                ( model, Cmd.none )

            else
                case model.currentTeam of
                    Just team ->
                        let
                            -- Check if ICS content looks valid
                            trimmedContent =
                                String.trim content

                            hasValidContent =
                                String.contains "BEGIN:VCALENDAR" trimmedContent || String.contains "BEGIN:VEVENT" trimmedContent

                            parsedMatches =
                                if String.isEmpty trimmedContent then
                                    []

                                else
                                    parseIcsToMatches team.name trimmedContent
                        in
                        if String.isEmpty trimmedContent then
                            ( { model | icsImportStatus = Just "Fehler: Die ICS-Datei ist leer." }, Cmd.none )

                        else if not hasValidContent then
                            ( { model | icsImportStatus = Just "Fehler: Die ICS-Datei scheint kein gültiges Format zu haben. Bitte überprüfen Sie die Datei." }, Cmd.none )

                        else if List.isEmpty parsedMatches then
                            ( { model | icsImportStatus = Just ("Keine Spiele in der ICS-Datei gefunden. Team-Name: \"" ++ team.name ++ "\". Bitte überprüfen Sie, ob der Team-Name mit dem in der ICS-Datei übereinstimmt.") }, Cmd.none )

                        else
                            let
                                -- Determine initial selection: future games checked, past games unchecked
                                ( pastMatches, futureMatches ) =
                                    case model.currentDate of
                                        Just today ->
                                            separatePastAndFutureMatches today parsedMatches

                                        Nothing ->
                                            ( [], parsedMatches )

                                -- If no current date, assume all are future
                                -- Build initial selection dict: future games = True, past games = False
                                initialSelection =
                                    parsedMatches
                                        |> List.indexedMap
                                            (\index match ->
                                                let
                                                    isFuture =
                                                        List.member match futureMatches
                                                in
                                                ( index, isFuture )
                                            )
                                        |> Dict.fromList

                                totalCount =
                                    List.length parsedMatches

                                futureCount =
                                    List.length futureMatches

                                pastCount =
                                    List.length pastMatches

                                selectedCount =
                                    List.length futureMatches
                            in
                            ( { model
                                | parsedIcsMatches = parsedMatches
                                , allParsedIcsMatches = parsedMatches
                                , icsImportSelectedMatches = initialSelection
                                , icsImportStatus =
                                    Just
                                        ("✓ "
                                            ++ String.fromInt totalCount
                                            ++ " Spiele gefunden ("
                                            ++ String.fromInt futureCount
                                            ++ " zukünftig"
                                            ++ (if pastCount > 0 then
                                                    ", " ++ String.fromInt pastCount ++ " vergangen"

                                                else
                                                    ""
                                               )
                                            ++ "). "
                                            ++ String.fromInt selectedCount
                                            ++ " werden importiert (zukünftige Spiele sind standardmäßig ausgewählt)."
                                        )
                              }
                            , Cmd.none
                            )

                    Nothing ->
                        ( { model | icsImportStatus = Just "Fehler: Kein Team ausgewählt." }, Cmd.none )

        ConfirmImportIcs teamId ->
            case model.currentTeam of
                Just team ->
                    let
                        -- Only import selected matches
                        matchesToImport =
                            model.allParsedIcsMatches
                                |> List.indexedMap Tuple.pair
                                |> List.filterMap
                                    (\( index, match ) ->
                                        if Dict.get index model.icsImportSelectedMatches |> Maybe.withDefault False then
                                            Just match

                                        else
                                            Nothing
                                    )

                        -- If no current date, import all
                        -- Create matches from filtered data
                        createMatchCmds =
                            List.map
                                (\parsedMatch ->
                                    let
                                        matchForm =
                                            { opponent = parsedMatch.opponent
                                            , date = parsedMatch.date
                                            , time = parsedMatch.time
                                            , venue = parsedMatch.venue
                                            , isHome = parsedMatch.isHome
                                            }
                                    in
                                    Lamdera.sendToBackend
                                        (CreateMatchRequest team.id matchForm (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault ""))
                                )
                                matchesToImport
                    in
                    ( { model
                        | showImportIcsModal = False
                        , icsImportUrl = ""
                        , parsedIcsMatches = []
                        , allParsedIcsMatches = []
                        , icsImportSelectedMatches = Dict.empty
                        , icsImportStatus = Just ("Erfolgreich " ++ String.fromInt (List.length matchesToImport) ++ " Spiele importiert!")
                      }
                    , Cmd.batch createMatchCmds
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmitAccessCodeRequested teamId accessCode ->
            ( { model
                | accessCodeRequired = Nothing
                , confirmedTeamCodes = Dict.insert teamId accessCode model.confirmedTeamCodes
              }
            , Lamdera.sendToBackend (SubmitAccessCode teamId accessCode)
            )

        AccessCodeInputChanged teamId accessCode ->
            let
                updatedModel =
                    { model | enteredAccessCode = accessCode }
            in
            if String.length accessCode == 4 then
                ( { updatedModel | accessCodeRequired = Nothing }
                , Cmd.batch
                    [ Lamdera.sendToBackend (SubmitAccessCode teamId accessCode)
                    , Lamdera.sendToBackend (GetTeamRequest teamId accessCode)
                    , LocalStorage.toJS ("SET_ACCESS_CODE:" ++ teamId ++ ":" ++ accessCode)
                    ]
                )

            else
                ( updatedModel, Cmd.none )

        CopyToClipboard text ->
            ( model, LocalStorage.toJS ("COPY:" ++ text) )

        GetAccessCode teamId ->
            ( model, LocalStorage.toJS ("GET_ACCESS_CODE:" ++ teamId) )

        AccessCodeLoaded teamId accessCode ->
            if String.length accessCode == 4 then
                ( { model
                    | confirmedTeamCodes = Dict.insert teamId accessCode model.confirmedTeamCodes
                    , accessCodeRequired = Nothing
                  }
                , Lamdera.sendToBackend (GetTeamRequest teamId accessCode)
                )

            else
                ( { model | accessCodeRequired = Just teamId }, Cmd.none )

        ShowDatePredictionModal matchId ->
            let
                currentMatch =
                    model.matches
                        |> List.filter (\match -> match.id == matchId)
                        |> List.head
            in
            ( { model
                | showDatePredictionModal = True
                , datePredictionMatchId = Just matchId
                , datePredictionForm = Maybe.map .date currentMatch |> Maybe.withDefault ""
              }
            , Cmd.none
            )

        HideDatePredictionModal ->
            ( { model
                | showDatePredictionModal = False
                , datePredictionMatchId = Nothing
                , datePredictionForm = ""
              }
            , Cmd.none
            )

        DatePredictionFormUpdated newDate ->
            ( { model | datePredictionForm = newDate }, Cmd.none )

        AddDatePrediction matchId predictedDate ->
            case ( model.currentTeam, model.activeMemberId ) of
                ( Just team, Just memberId ) ->
                    ( model
                    , Lamdera.sendToBackend
                        (AddDatePredictionRequest matchId
                            predictedDate
                            memberId
                            (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        RemoveDatePrediction matchId ->
            case ( model.currentTeam, model.activeMemberId ) of
                ( Just team, Just memberId ) ->
                    ( model
                    , Lamdera.sendToBackend
                        (RemoveDatePredictionRequest matchId
                            memberId
                            (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        UpdatePredictionAvailability matchId predictedDate availability ->
            case ( model.currentTeam, model.activeMemberId ) of
                ( Just team, Just memberId ) ->
                    ( model
                    , Lamdera.sendToBackend
                        (UpdatePredictionAvailabilityRequest matchId
                            predictedDate
                            memberId
                            availability
                            (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        ChoosePredictedDate matchId chosenDate ->
            case model.currentTeam of
                Just team ->
                    ( model
                    , Lamdera.sendToBackend
                        (ChoosePredictedDateRequest matchId
                            chosenDate
                            team.id
                            (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")
                        )
                    )

                Nothing ->
                    ( model, Cmd.none )

        -- Placeholder for now
        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TeamCreated team creatorMemberId accessCode ->
            let
                teamUrl =
                    createTeamUrl team.slug team.id accessCode
            in
            ( { model
                | currentTeam = Just team
                , activeMemberId = Just creatorMemberId
                , confirmedTeamCodes = Dict.insert team.id accessCode model.confirmedTeamCodes
                , accessCodeRequired = Nothing
              }
            , Cmd.batch
                [ Nav.pushUrl model.key teamUrl
                , LocalStorage.toJS ("SET:" ++ creatorMemberId)
                , LocalStorage.toJS ("SET_ACCESS_CODE:" ++ team.id ++ ":" ++ accessCode)
                ]
            )

        TeamLoaded team matches members availability predictions ->
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
                , datePredictions = predictions
                , showMemberSelectionModal = shouldShowMemberSelection
              }
            , Cmd.none
            )

        TeamNotFound ->
            ( { model | page = NotFoundPage }, Cmd.none )

        AccessCodeRequired teamId ->
            ( { model | accessCodeRequired = Just teamId }, Cmd.none )

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

                -- Don't clear availability here - AvailabilityUpdated messages will handle updates
                -- This allows prediction migrations to work correctly
            in
            ( { model
                | matches = updatedMatches
                , showChangeMatchDateModal = False
                , changeMatchDateMatchId = Nothing
                , changeMatchDateForm = ""
              }
            , Cmd.none
            )

        DatePredictionAdded prediction matchId ->
            let
                -- Get or create predictions dict for this match
                matchPredictions =
                    model.datePredictions
                        |> Dict.get matchId
                        |> Maybe.withDefault Dict.empty

                -- Get predictions for this specific date
                datePredictions =
                    matchPredictions
                        |> Dict.get prediction.predictedDate
                        |> Maybe.withDefault Dict.empty

                -- Add/update this member's prediction for this date
                updatedDatePredictions =
                    Dict.insert prediction.memberId prediction datePredictions

                -- Update match predictions dict
                updatedMatchPredictions =
                    Dict.insert prediction.predictedDate updatedDatePredictions matchPredictions

                updatedPredictions =
                    Dict.insert matchId updatedMatchPredictions model.datePredictions
            in
            ( { model
                | datePredictions = updatedPredictions
                , showDatePredictionModal = False
                , datePredictionMatchId = Nothing
                , datePredictionForm = ""
              }
            , Cmd.none
            )

        DatePredictionUpdated prediction matchId ->
            let
                -- Get or create predictions dict for this match
                matchPredictions =
                    model.datePredictions
                        |> Dict.get matchId
                        |> Maybe.withDefault Dict.empty

                -- Get predictions for this specific date
                datePredictions =
                    matchPredictions
                        |> Dict.get prediction.predictedDate
                        |> Maybe.withDefault Dict.empty

                -- Update this member's prediction for this date
                updatedDatePredictions =
                    Dict.insert prediction.memberId prediction datePredictions

                -- Update match predictions dict
                updatedMatchPredictions =
                    Dict.insert prediction.predictedDate updatedDatePredictions matchPredictions

                updatedPredictions =
                    Dict.insert matchId updatedMatchPredictions model.datePredictions
            in
            ( { model | datePredictions = updatedPredictions }, Cmd.none )

        DatePredictionRemoved matchId memberId ->
            let
                -- Remove all predictions for this member/match combination
                -- (remove member from all date predictions)
                matchPredictions =
                    model.datePredictions
                        |> Dict.get matchId
                        |> Maybe.withDefault Dict.empty

                -- Remove member from all date predictions
                updatedMatchPredictions =
                    matchPredictions
                        |> Dict.map (\date datePredictions -> Dict.remove memberId datePredictions)
                        |> Dict.filter (\_ datePredictions -> not (Dict.isEmpty datePredictions))

                updatedPredictions =
                    if Dict.isEmpty updatedMatchPredictions then
                        Dict.remove matchId model.datePredictions

                    else
                        Dict.insert matchId updatedMatchPredictions model.datePredictions
            in
            ( { model | datePredictions = updatedPredictions }, Cmd.none )

        PredictionsCleared matchId ->
            let
                updatedPredictions =
                    Dict.remove matchId model.datePredictions
            in
            ( { model | datePredictions = updatedPredictions }, Cmd.none )

        MatchOriginalDateSet matchId originalDate ->
            let
                -- Update the match's originalDate field
                updatedMatches =
                    List.map
                        (\match ->
                            if match.id == matchId then
                                { match | originalDate = Just originalDate }

                            else
                                match
                        )
                        model.matches
            in
            ( { model | matches = updatedMatches }, Cmd.none )

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
            if model.accessCodeRequired /= Nothing && model.currentTeam == Nothing then
                viewAccessCodeRequired model

            else
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
                    , Events.onInput (\name -> CreateTeamFormUpdated { name = name, creatorName = model.createTeamForm.creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = model.createTeamForm.playersNeeded, accessCode = "" })
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
                    , Events.onInput (\creatorName -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = model.createTeamForm.playersNeeded, accessCode = "" })
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
                    , Events.onInput (\names -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = model.createTeamForm.creatorName, otherMemberNames = names, playersNeeded = model.createTeamForm.playersNeeded, accessCode = "" })
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
                    , Events.onInput (\playersNeeded -> CreateTeamFormUpdated { name = model.createTeamForm.name, creatorName = model.createTeamForm.creatorName, otherMemberNames = model.createTeamForm.otherMemberNames, playersNeeded = playersNeeded, accessCode = "" })
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


viewParsedMatchesTable : Dict Int Bool -> List ParsedMatch -> Html FrontendMsg
viewParsedMatchesTable selectedMatches parsedMatches =
    Html.div
        [ Attr.style "overflow-x" "auto"
        , Attr.style "border" "1px solid #e5e7eb"
        , Attr.style "border-radius" "0.375rem"
        , Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOpFrontendMsg, True ))
        ]
        [ Html.table
            [ Attr.style "width" "100%"
            , Attr.style "border-collapse" "collapse"
            , Attr.style "font-size" "0.875rem"
            ]
            [ Html.thead []
                [ Html.tr
                    [ Attr.style "background-color" "#f9fafb"
                    , Attr.style "border-bottom" "2px solid #e5e7eb"
                    ]
                    [ Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "center"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        , Attr.style "width" "50px"
                        ]
                        [ Html.text "✓" ]
                    , Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "left"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.text "Datum" ]
                    , Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "left"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.text "Uhrzeit" ]
                    , Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "left"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.text "Gegner" ]
                    , Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "left"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.text "Spielort" ]
                    , Html.th
                        [ Attr.style "padding" "0.75rem"
                        , Attr.style "text-align" "center"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#374151"
                        ]
                        [ Html.text "Heim/Auswärts" ]
                    ]
                ]
            , Html.tbody []
                (List.indexedMap
                    (\index match ->
                        let
                            isSelected =
                                Dict.get index selectedMatches |> Maybe.withDefault False
                        in
                        Html.tr
                            [ Attr.style "border-bottom" "1px solid #e5e7eb"
                            , Attr.style "background-color"
                                (if remainderBy 2 index == 0 then
                                    "white"

                                 else
                                    "#f9fafb"
                                )
                            ]
                            [ Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "text-align" "center"
                                ]
                                [ Html.input
                                    [ Attr.type_ "checkbox"
                                    , Attr.checked isSelected
                                    , Events.onClick (IcsImportMatchToggled index (not isSelected))
                                    , Attr.style "cursor" "pointer"
                                    ]
                                    []
                                ]
                            , Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "color" "#374151"
                                ]
                                [ Html.text match.date ]
                            , Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "color" "#374151"
                                ]
                                [ Html.text match.time ]
                            , Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "color" "#374151"
                                , Attr.style "font-weight" "500"
                                ]
                                [ Html.text match.opponent ]
                            , Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "color" "#64748b"
                                , Attr.style "max-width" "200px"
                                , Attr.style "overflow" "hidden"
                                , Attr.style "text-overflow" "ellipsis"
                                , Attr.style "white-space" "nowrap"
                                ]
                                [ Html.text match.venue ]
                            , Html.td
                                [ Attr.style "padding" "0.75rem"
                                , Attr.style "text-align" "center"
                                ]
                                [ Html.span
                                    [ Attr.style "padding" "0.25rem 0.5rem"
                                    , Attr.style "border-radius" "0.25rem"
                                    , Attr.style "font-size" "0.75rem"
                                    , Attr.style "font-weight" "500"
                                    , Attr.style "background-color"
                                        (if match.isHome then
                                            "#dbeafe"

                                         else
                                            "#fef3c7"
                                        )
                                    , Attr.style "color"
                                        (if match.isHome then
                                            "#1e40af"

                                         else
                                            "#92400e"
                                        )
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
                    )
                    parsedMatches
                )
            ]
        ]


viewTeamPage : Model -> Html FrontendMsg
viewTeamPage model =
    case model.currentTeam of
        Just team ->
            Html.div []
                [ Html.node "style"
                    []
                    [ Html.text """
                        .availability-controls-desktop {
                            display: flex;
                            flex-direction: column;
                            align-items: flex-end;
                            gap: 0.5rem;
                        }
                        .availability-controls-mobile {
                            display: none;
                            justify-content: center;
                            width: 100%;
                        }
                        @media (max-width: 768px) {
                            .availability-controls-desktop {
                                display: none;
                            }
                            .availability-controls-mobile {
                                display: flex;
                            }
                        }
                    """ ]
                , Html.div
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
                            [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")) ]
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
                , if model.showImportIcsModal then
                    viewImportIcsModal model team

                  else
                    Html.text ""
                , if model.showDatePredictionModal then
                    viewDatePredictionModal model

                  else
                    Html.text ""
                ]

        Nothing ->
            Html.div
                [ Attr.style "text-align" "center"
                , Attr.style "padding" "3rem 0"
                ]
                [ Html.text "Team wird geladen..." ]


viewAccessCodeRequired : Model -> Html FrontendMsg
viewAccessCodeRequired model =
    case model.accessCodeRequired of
        Just teamId ->
            Html.div
                [ Attr.style "max-width" "500px"
                , Attr.style "margin" "0 auto"
                , Attr.style "padding" "2rem 0"
                ]
                [ Html.div
                    [ Attr.style "background-color" "white"
                    , Attr.style "padding" "2rem"
                    , Attr.style "border-radius" "0.5rem"
                    , Attr.style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
                    , Attr.style "text-align" "center"
                    ]
                    [ Html.h2
                        [ Attr.style "font-size" "1.75rem"
                        , Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin-bottom" "1rem"
                        ]
                        [ Html.text "🔐 Zugangscode erforderlich" ]
                    , Html.p
                        [ Attr.style "color" "#64748b"
                        , Attr.style "margin-bottom" "1.5rem"
                        ]
                        [ Html.text "Bitte gib den Zugangscode ein, um auf diese Mannschaft zuzugreifen:" ]
                    , Html.input
                        [ Attr.type_ "text"
                        , Attr.placeholder "Zugangscode eingeben"
                        , Attr.style "width" "100%"
                        , Attr.style "max-width" "300px"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "font-size" "1rem"
                        , Attr.style "margin-bottom" "1rem"
                        , Attr.style "text-align" "center"
                        , Attr.style "letter-spacing" "0.5em"
                        , Attr.maxlength 4
                        , Events.onInput (AccessCodeInputChanged teamId)
                        ]
                        []
                    , Html.p
                        [ Attr.style "color" "#6b7280"
                        , Attr.style "font-size" "0.875rem"
                        ]
                        [ Html.text "Der Zugangscode wurde beim Erstellen der Mannschaft generiert und sollte dir vom Teamleiter mitgeteilt worden sein." ]
                    ]
                ]

        Nothing ->
            Html.text ""


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


getMatchStatus : String -> String -> String -> List Member -> List AvailabilityRecord -> Int -> Dict String (Dict String (Dict String DatePrediction)) -> MatchStatus
getMatchStatus matchId matchDate today members availability playersNeeded datePredictions =
    let
        summary =
            getMatchAvailabilitySummary matchId members availability

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
        -- Convert both dates to sortable format and compare
        -- For simplicity, we check if match is in the same month or next month as today
        -- This is an approximation but works well for the use case
        isLessThanTwoWeeksAway =
            let
                matchSortable =
                    convertToSortable matchDate

                todaySortable =
                    convertToSortable today

                -- Extract year-month from sortable dates (yyyy-mm)
                matchYearMonth =
                    String.left 7 matchSortable

                todayYearMonth =
                    String.left 7 todaySortable

                -- Check if match is in same month or next month
                -- This approximates "within 2 weeks" reasonably well
            in
            matchSortable
                >= todaySortable
                && (matchYearMonth
                        == todayYearMonth
                        || (matchYearMonth
                                > todayYearMonth
                                && String.left 4 matchSortable
                                == String.left 4 todaySortable
                           )
                   )

        -- Check if there are any predictions for this match
        hasPredictions =
            datePredictions
                |> Dict.get matchId
                |> Maybe.map (not << Dict.isEmpty)
                |> Maybe.withDefault False
    in
    if isPastMatch then
        -- Past matches don't need status colors
        Past

    else if hasPredictions then
        -- If there are predictions, the match needs attention (should be changed)
        -- Show as Possible (yellow) even if availability is good
        Possible

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
        -- Use current date or fallback to a default date if not available yet
        today =
            model.currentDate |> Maybe.withDefault "01.01.2024"

        ( pastMatches, futureMatches ) =
            separatePastAndFutureMatches today model.matches

        -- Convert German date (dd.mm.yyyy) to sortable format (yyyy-mm-dd)
        -- IMPORTANT: Pad with zeros to ensure correct string comparison
        germanDateToSortable : String -> String
        germanDateToSortable dateStr =
            case String.split "." dateStr of
                [ day, month, year ] ->
                    year ++ "-" ++ String.padLeft 2 '0' month ++ "-" ++ String.padLeft 2 '0' day

                _ ->
                    dateStr

        -- Sort past matches by oldest first (chronological order)
        sortedPastMatches =
            List.sortWith (\a b -> compare (germanDateToSortable a.date) (germanDateToSortable b.date)) pastMatches

        -- Sort future matches by earliest first (next match at top)
        sortedFutureMatches =
            List.sortWith (\a b -> compare (germanDateToSortable a.date) (germanDateToSortable b.date)) futureMatches

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
            , Html.div
                [ Attr.style "position" "relative"
                , Attr.style "flex-shrink" "0"
                ]
                [ Html.button
                    [ Events.onClick ToggleAddMatchDropdown
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
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "0.5rem"
                    ]
                    [ Html.text "➕"
                    , Html.text "Spiel hinzufügen"
                    , Html.span
                        [ Attr.style "margin-left" "0.25rem"
                        , Attr.style "font-size" "0.75rem"
                        ]
                        [ Html.text
                            (if model.showAddMatchDropdown then
                                "▲"

                             else
                                "▼"
                            )
                        ]
                    ]
                , if model.showAddMatchDropdown then
                    Html.div
                        [ Attr.style "position" "absolute"
                        , Attr.style "top" "100%"
                        , Attr.style "right" "0"
                        , Attr.style "margin-top" "0.5rem"
                        , Attr.style "background-color" "white"
                        , Attr.style "border" "1px solid #e5e7eb"
                        , Attr.style "border-radius" "0.5rem"
                        , Attr.style "box-shadow" "0 4px 6px rgba(0,0,0,0.1)"
                        , Attr.style "min-width" "200px"
                        , Attr.style "z-index" "100"
                        ]
                        [ Html.button
                            [ Events.onClick ShowCreateMatchModal
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem 1rem"
                            , Attr.style "border" "none"
                            , Attr.style "background-color" "white"
                            , Attr.style "color" "#374151"
                            , Attr.style "text-align" "left"
                            , Attr.style "font-size" "0.875rem"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "transition" "background-color 0.2s"
                            , Attr.style "border-radius" "0.5rem 0.5rem 0 0"
                            ]
                            [ Html.text "Spiel hinzufügen" ]
                        , Html.div
                            [ Attr.style "height" "1px"
                            , Attr.style "background-color" "#e5e7eb"
                            ]
                            []
                        , Html.button
                            [ Events.onClick ShowImportIcsModal
                            , Attr.style "width" "100%"
                            , Attr.style "padding" "0.75rem 1rem"
                            , Attr.style "border" "none"
                            , Attr.style "background-color" "white"
                            , Attr.style "color" "#374151"
                            , Attr.style "text-align" "left"
                            , Attr.style "font-size" "0.875rem"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "transition" "background-color 0.2s"
                            , Attr.style "border-radius" "0 0 0.5rem 0.5rem"
                            ]
                            [ Html.text "TT-Live ICS importieren" ]
                        ]

                  else
                    Html.text ""
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
                [ -- Future Matches Section (shown first - more important)
                  if List.isEmpty futureMatches then
                    Html.text ""

                  else
                    Html.div
                        [ Attr.style "margin-bottom" "2rem" ]
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
                , -- Past Matches Section (shown second - less important)
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
                ]
        ]



-- DATE PREDICTION HELPERS


getPredictionsForMatch : String -> Model -> Dict String DatePrediction
getPredictionsForMatch matchId model =
    -- Flatten all predictions for this match into a single dict (memberId -> DatePrediction)
    -- This is used for status calculation - we take the first prediction per member
    model.datePredictions
        |> Dict.get matchId
        |> Maybe.withDefault Dict.empty
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.foldl
            (\prediction acc ->
                -- Only keep first prediction per member (for status calculation)
                if Dict.member prediction.memberId acc then
                    acc

                else
                    Dict.insert prediction.memberId prediction acc
            )
            Dict.empty


getPredictionsGroupedByDate : Dict String DatePrediction -> Dict String (Dict String DatePrediction)
getPredictionsGroupedByDate predictions =
    predictions
        |> Dict.values
        |> List.foldl
            (\prediction acc ->
                let
                    datePredictions =
                        Dict.get prediction.predictedDate acc
                            |> Maybe.withDefault Dict.empty
                in
                Dict.insert prediction.predictedDate (Dict.insert prediction.memberId prediction datePredictions) acc
            )
            Dict.empty


getPredictionStatus : Dict String DatePrediction -> List Member -> Int -> MatchStatus
getPredictionStatus datePredictions members playersNeeded =
    let
        -- Calculate summary from the date-specific predictions dict
        predictionsList =
            Dict.values datePredictions

        summary =
            List.foldl
                (\prediction acc ->
                    case prediction.availability of
                        Available ->
                            { acc | available = acc.available + 1 }

                        NotAvailable ->
                            { acc | notAvailable = acc.notAvailable + 1 }

                        Maybe ->
                            { acc | maybe = acc.maybe + 1 }
                )
                { available = 0, notAvailable = 0, maybe = 0, total = List.length members }
                predictionsList
    in
    if summary.available >= playersNeeded then
        Ready

    else
        -- Predictions always show as Possible (yellow) when not ready, never NotReady (red)
        -- This indicates attention is needed, not that something is bad
        Possible


getMemberPredictionForDate : String -> String -> Dict String DatePrediction -> Maybe DatePrediction
getMemberPredictionForDate memberId predictedDate predictions =
    predictions
        |> Dict.values
        |> List.filter (\pred -> pred.memberId == memberId && pred.predictedDate == predictedDate)
        |> List.head


viewDatePredictionSection : Model -> Match -> Html FrontendMsg
viewDatePredictionSection model match =
    let
        -- Get predictions already grouped by date: matchId -> predictedDate -> memberId -> DatePrediction
        predictionsByDate =
            model.datePredictions
                |> Dict.get match.id
                |> Maybe.withDefault Dict.empty

        hasPredictions =
            not (Dict.isEmpty predictionsByDate)

        originalDateDisplay =
            case match.originalDate of
                Just origDate ->
                    if origDate /= match.date then
                        Just origDate

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    Html.div
        [ Attr.style "margin-top" "0.75rem"
        , Attr.style "padding-top" "0.75rem"
        , Attr.style "border-top" "1px solid #e5e7eb"
        ]
        [ -- Original date display if different
          case originalDateDisplay of
            Just origDate ->
                Html.div
                    [ Attr.style "margin-bottom" "0.5rem"
                    , Attr.style "padding" "0.5rem"
                    , Attr.style "background-color" "#fef3c7"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "font-size" "0.75rem"
                    , Attr.style "color" "#92400e"
                    ]
                    [ Html.text ("Ursprüngliches Datum: " ++ isoToGermanDate origDate) ]

            Nothing ->
                Html.text ""

        -- Predictions list
        , if hasPredictions then
            Html.div
                [ Attr.style "margin-bottom" "0.75rem" ]
                (predictionsByDate
                    |> Dict.toList
                    |> List.sortWith
                        (\( dateA, _ ) ( dateB, _ ) ->
                            -- Convert German date format (dd.mm.yyyy) to sortable format (yyyy-mm-dd)
                            let
                                convertToSortable : String -> String
                                convertToSortable dateStr =
                                    case String.split "." dateStr of
                                        [ day, month, year ] ->
                                            year ++ "-" ++ String.padLeft 2 '0' month ++ "-" ++ String.padLeft 2 '0' day

                                        _ ->
                                            dateStr

                                sortableA =
                                    convertToSortable dateA

                                sortableB =
                                    convertToSortable dateB
                            in
                            compare sortableA sortableB
                        )
                    |> List.map (\( date, datePredictions ) -> viewPredictedDateCard model match date datePredictions)
                )

          else
            Html.text ""

        -- Add prediction button
        , Html.button
            [ Events.onClick (ShowDatePredictionModal match.id)
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
            ]
            [ Html.text
                (if hasPredictions then
                    "📅 Weitere Vorhersage hinzufügen"

                 else
                    "📅 Verlegung vorschlagen"
                )
            ]
        ]


viewPredictedDateCard : Model -> Match -> String -> Dict String DatePrediction -> Html FrontendMsg
viewPredictedDateCard model match predictedDate datePredictions =
    let
        predictionsList =
            Dict.values datePredictions

        status =
            getPredictionStatus datePredictions
                model.members
                (case model.currentTeam of
                    Just team ->
                        team.playersNeeded

                    Nothing ->
                        11
                )

        summary =
            List.foldl
                (\prediction acc ->
                    case prediction.availability of
                        Available ->
                            { acc | available = acc.available + 1 }

                        NotAvailable ->
                            { acc | notAvailable = acc.notAvailable + 1 }

                        Maybe ->
                            { acc | maybe = acc.maybe + 1 }
                )
                { available = 0, notAvailable = 0, maybe = 0, total = List.length model.members }
                predictionsList

        statusColor =
            matchStatusToColor status

        statusBgColor =
            matchStatusToBackgroundColor status

        -- Find current member's prediction for this specific date
        currentMemberPrediction =
            case model.activeMemberId of
                Just memberId ->
                    Dict.get memberId datePredictions

                Nothing ->
                    Nothing

        isExpanded =
            List.member ( match.id, predictedDate ) model.expandedPredictions
    in
    Html.div
        [ Attr.style "margin-bottom" "0.75rem"
        , Attr.style "padding" "0.75rem"
        , Attr.style "background-color" statusBgColor
        , Attr.style "border" ("1px solid " ++ statusColor)
        , Attr.style "border-radius" "0.375rem"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "flex-start"
            , Attr.style "margin-bottom" "0.5rem"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "gap" "0.5rem"
            ]
            [ Html.div
                [ Attr.style "flex" "1"
                , Attr.style "display" "flex"
                , Attr.style "justify-content" "space-between"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                ]
                [ Html.div
                    [ Attr.style "flex" "1" ]
                    [ Html.div
                        [ Attr.style "font-weight" "600"
                        , Attr.style "color" "#1e293b"
                        , Attr.style "margin-bottom" "0.25rem"
                        ]
                        [ Html.text ("Vorschlag: " ++ isoToGermanDate predictedDate) ]
                    , Html.div
                        [ Attr.style "font-size" "0.75rem"
                        , Attr.style "color" "#64748b"
                        ]
                        [ Html.text
                            ("✓ "
                                ++ String.fromInt summary.available
                                ++ " | ? "
                                ++ String.fromInt summary.maybe
                                ++ " | ✗ "
                                ++ String.fromInt summary.notAvailable
                            )
                        ]
                    ]
                , Html.button
                    [ Events.onClick (TogglePredictionDetails match.id predictedDate)
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
            , if status == Ready then
                Html.button
                    [ Events.onClick (ChoosePredictedDate match.id predictedDate)
                    , Attr.style "background-color" "#10b981"
                    , Attr.style "color" "white"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "padding" "0.5rem 1rem"
                    , Attr.style "font-size" "0.75rem"
                    , Attr.style "font-weight" "500"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "min-height" "44px"
                    ]
                    [ Html.text "Datum wählen" ]

              else
                Html.text ""
            ]
        , -- Current member's prediction controls
          case ( model.activeMemberId, currentMemberPrediction ) of
            ( Just memberId, Just prediction ) ->
                Html.div
                    [ Attr.style "margin-top" "0.5rem"
                    , Attr.style "padding-top" "0.5rem"
                    , Attr.style "border-top" "1px solid rgba(0,0,0,0.1)"
                    ]
                    [ Html.div
                        [ Attr.style "font-size" "0.75rem"
                        , Attr.style "color" "#64748b"
                        , Attr.style "margin-bottom" "0.25rem"
                        ]
                        [ Html.text "Deine Verfügbarkeit:" ]
                    , viewPredictionAvailabilityControls memberId match.id predictedDate (Just prediction.availability)
                    ]

            ( Just memberId, Nothing ) ->
                Html.div
                    [ Attr.style "margin-top" "0.5rem"
                    , Attr.style "padding-top" "0.5rem"
                    , Attr.style "border-top" "1px solid rgba(0,0,0,0.1)"
                    ]
                    [ Html.div
                        [ Attr.style "font-size" "0.75rem"
                        , Attr.style "color" "#64748b"
                        , Attr.style "margin-bottom" "0.25rem"
                        ]
                        [ Html.text "Deine Verfügbarkeit:" ]
                    , viewPredictionAvailabilityControls memberId match.id predictedDate Nothing
                    ]

            _ ->
                Html.text ""
        , if isExpanded then
            viewPredictionDetailsExpanded match.id predictedDate datePredictions model.members

          else
            Html.text ""
        ]


viewDatePredictionModal : Model -> Html FrontendMsg
viewDatePredictionModal model =
    case model.datePredictionMatchId of
        Just matchId ->
            let
                currentMatch =
                    model.matches
                        |> List.filter (\match -> match.id == matchId)
                        |> List.head

                -- Check if the entered date is already predicted by someone
                enteredDate =
                    String.trim model.datePredictionForm

                dateAlreadyPredicted =
                    if String.isEmpty enteredDate then
                        False

                    else
                        model.datePredictions
                            |> Dict.get matchId
                            |> Maybe.withDefault Dict.empty
                            |> Dict.member enteredDate

                -- Check if current member already predicted this date
                currentMemberHasPrediction =
                    case model.activeMemberId of
                        Just memberId ->
                            model.datePredictions
                                |> Dict.get matchId
                                |> Maybe.withDefault Dict.empty
                                |> Dict.get enteredDate
                                |> Maybe.andThen (Dict.get memberId)
                                |> Maybe.map (\_ -> True)
                                |> Maybe.withDefault False

                        Nothing ->
                            False
            in
            Html.div
                [ Attr.style "position" "fixed"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" "0"
                , Attr.style "background-color" "rgba(0,0,0,0.5)"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "z-index" "1000"
                , Events.onClick HideDatePredictionModal
                ]
                [ Html.div
                    [ Attr.style "background-color" "white"
                    , Attr.style "border-radius" "0.5rem"
                    , Attr.style "max-width" "500px"
                    , Attr.style "width" "90%"
                    , Attr.style "max-height" "90vh"
                    , Attr.style "overflow-y" "auto"
                    , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.2)"
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
                                [ Html.text "Verlegung vorschlagen" ]
                            , Html.button
                                [ Events.onClick HideDatePredictionModal
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
                            (case model.datePredictionMatchId of
                                Just mId ->
                                    if String.isEmpty (String.trim model.datePredictionForm) then
                                        NoOpFrontendMsg

                                    else
                                        AddDatePrediction mId model.datePredictionForm

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
                                [ Html.text "Vorhergesagtes Datum" ]
                            , Html.input
                                [ Attr.type_ "text"
                                , Attr.value model.datePredictionForm
                                , Events.onInput DatePredictionFormUpdated
                                , Attr.placeholder "25.12.2024"
                                , Attr.style "width" "100%"
                                , Attr.style "padding" "0.75rem"
                                , Attr.style "border"
                                    ("1px solid "
                                        ++ (if dateAlreadyPredicted then
                                                "#f59e0b"

                                            else
                                                "#d1d5db"
                                           )
                                    )
                                , Attr.style "border-radius" "0.375rem"
                                , Attr.style "font-size" "1rem"
                                ]
                                []
                            , if dateAlreadyPredicted then
                                Html.div
                                    [ Attr.style "margin-top" "0.5rem"
                                    , Attr.style "padding" "0.5rem"
                                    , Attr.style "background-color" "#fef3c7"
                                    , Attr.style "border-radius" "0.375rem"
                                    , Attr.style "font-size" "0.875rem"
                                    , Attr.style "color" "#92400e"
                                    ]
                                    [ Html.text
                                        (if currentMemberHasPrediction then
                                            "Du hast dieses Datum bereits vorgeschlagen. Deine Verfügbarkeit wird aktualisiert."

                                         else
                                            "Dieses Datum wurde bereits von einem anderen Mitglied vorgeschlagen. Du kannst deine Verfügbarkeit dafür setzen."
                                        )
                                    ]

                              else
                                Html.text ""
                            ]
                        , Html.div
                            [ Attr.style "display" "flex"
                            , Attr.style "gap" "1rem"
                            , Attr.style "justify-content" "flex-end"
                            ]
                            [ Html.button
                                [ Attr.type_ "button"
                                , Events.onClick HideDatePredictionModal
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
                                , Attr.style "background-color" "#3b82f6"
                                , Attr.style "color" "white"
                                , Attr.style "cursor" "pointer"
                                , Attr.style "font-weight" "500"
                                , Attr.disabled (String.isEmpty (String.trim model.datePredictionForm))
                                ]
                                [ Html.text "Hinzufügen" ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""


viewMatchItem : Model -> Team -> Match -> Bool -> Html FrontendMsg
viewMatchItem model team match isLast =
    let
        isExpanded =
            List.member match.id model.expandedMatches

        today =
            model.currentDate |> Maybe.withDefault "01.01.2024"

        matchStatus =
            getMatchStatus match.id match.date today model.members model.availability team.playersNeeded model.datePredictions

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
                        , Attr.style "width" "100%"
                        , Attr.style "max-width" "100%"
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
                            [ Html.text
                                ((case match.originalDate of
                                    Just origDate ->
                                        if origDate /= match.date then
                                            "Geplanter Termin: "

                                        else
                                            ""

                                    Nothing ->
                                        ""
                                 )
                                    ++ match.date
                                    ++ " um "
                                    ++ match.time
                                )
                            ]
                        , viewDatePredictionSection model match
                        , Html.p
                            [ Attr.style "color" "#64748b"
                            , Attr.style "margin" "0 0 0.5rem 0"
                            , Attr.style "font-size" "0.875rem"
                            , Attr.style "line-height" "1.5"
                            , Attr.style "overflow-wrap" "break-word"
                            , Attr.style "word-break" "normal"
                            , Attr.style "width" "100%"
                            , Attr.style "max-width" "100%"
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
                                Html.div
                                    [ Attr.class "availability-controls-desktop"
                                    ]
                                    [ viewAvailabilityControls activeMemberId match.id (getMemberAvailabilityForMatch activeMemberId match.id model.availability)
                                    ]

                            Nothing ->
                                Html.text ""
                        ]
                    ]
                , Html.div
                    [ Attr.style "margin-top" "0.5rem"
                    , Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "gap" "0.75rem"
                    ]
                    [ case model.activeMemberId of
                        Just activeMemberId ->
                            Html.div
                                [ Attr.class "availability-controls-mobile"
                                ]
                                [ viewAvailabilityControls activeMemberId match.id (getMemberAvailabilityForMatch activeMemberId match.id model.availability)
                                ]

                        Nothing ->
                            Html.text ""
                    , Html.div
                        [ Attr.style "display" "flex"
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
        , Attr.style "justify-content" "center"
        , Attr.style "width" "100%"
        ]
        [ viewAvailabilityButton memberId matchId Available "✓" (currentAvailability == Just Available)
        , viewAvailabilityButton memberId matchId Maybe "?" (currentAvailability == Just Maybe)
        , viewAvailabilityButton memberId matchId NotAvailable "✗" (currentAvailability == Just NotAvailable)
        ]


viewPredictionAvailabilityControls : String -> String -> String -> Maybe Availability -> Html FrontendMsg
viewPredictionAvailabilityControls memberId matchId predictedDate currentAvailability =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "0.5rem"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "100%"
        ]
        [ viewPredictionAvailabilityButton memberId matchId predictedDate Available "✓" (currentAvailability == Just Available)
        , viewPredictionAvailabilityButton memberId matchId predictedDate Maybe "?" (currentAvailability == Just Maybe)
        , viewPredictionAvailabilityButton memberId matchId predictedDate NotAvailable "✗" (currentAvailability == Just NotAvailable)
        ]


viewPredictionAvailabilityButton : String -> String -> String -> Availability -> String -> Bool -> Html FrontendMsg
viewPredictionAvailabilityButton memberId matchId predictedDate availability icon isSelected =
    Html.button
        [ Events.stopPropagationOn "click" (Json.Decode.succeed ( UpdatePredictionAvailability matchId predictedDate availability, True ))
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


viewPredictionDetailsExpanded : String -> String -> Dict String DatePrediction -> List Member -> Html FrontendMsg
viewPredictionDetailsExpanded matchId predictedDate datePredictions members =
    let
        -- Group members by their prediction availability
        groupedMembers =
            members
                |> List.map
                    (\member ->
                        let
                            memberPrediction =
                                Dict.get member.id datePredictions
                                    |> Maybe.map .availability
                        in
                        ( member, memberPrediction )
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
        , Attr.style "padding" "0.75rem"
        , Attr.style "border-top" "1px solid #e2e8f0"
        , Attr.style "margin-top" "0.5rem"
        ]
        [ Html.h6
            [ Attr.style "font-size" "0.75rem"
            , Attr.style "font-weight" "600"
            , Attr.style "color" "#374151"
            , Attr.style "margin" "0 0 0.75rem 0"
            ]
            [ Html.text "Verfügbarkeit für dieses Datum" ]
        , Html.div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "repeat(auto-fit, minmax(200px, 1fr))"
            , Attr.style "gap" "0.75rem"
            ]
            [ viewAvailabilityGroup "Verfügbar" "#10b981" "✓" groupedMembers.available
            , viewAvailabilityGroup "Vielleicht" "#f59e0b" "?" groupedMembers.maybe
            , viewAvailabilityGroup "Nicht verfügbar" "#ef4444" "✗" groupedMembers.notAvailable
            , viewAvailabilityGroup "Keine Antwort" "#9ca3af" "?" groupedMembers.noResponse
            ]
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
            [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")) ]
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
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "share-modal-title"
        , Attr.style "position" "fixed"
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
                        [ Attr.id "share-modal-title"
                        , Attr.style "font-size" "1.25rem"
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
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "0.5rem"
                    , Attr.style "margin-bottom" "1rem"
                    ]
                    [ Html.div
                        [ Attr.style "flex" "1"
                        , Attr.style "background-color" "#f8fafc"
                        , Attr.style "padding" "1rem"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "border" "1px solid #e2e8f0"
                        , Attr.style "font-family" "monospace"
                        , Attr.style "font-size" "0.875rem"
                        , Attr.style "word-break" "break-all"
                        ]
                        [ Html.text (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")) ]
                    , Html.button
                        [ Events.onClick (CopyToClipboard (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")))
                        , Attr.style "background-color" "#3b82f6"
                        , Attr.style "color" "white"
                        , Attr.style "border" "none"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "font-size" "0.875rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "transition" "background-color 0.2s"
                        , Attr.style "min-width" "44px"
                        , Attr.style "min-height" "44px"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        ]
                        [ Html.text "📋" ]
                    ]
                , Html.div
                    [ Attr.style "background-color" "#f0f9ff"
                    , Attr.style "padding" "1rem"
                    , Attr.style "border-radius" "0.375rem"
                    , Attr.style "border" "1px solid #0ea5e9"
                    , Attr.style "margin-bottom" "1rem"
                    ]
                    [ Html.p
                        [ Attr.style "color" "#0c4a6e"
                        , Attr.style "font-weight" "500"
                        , Attr.style "margin-bottom" "0.5rem"
                        , Attr.style "margin-top" "0"
                        ]
                        [ Html.text "🔐 Zugangscode für dein Team:" ]
                    , Html.div
                        [ Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "gap" "0.5rem"
                        ]
                        [ Html.div
                            [ Attr.style "flex" "1"
                            , Attr.style "background-color" "white"
                            , Attr.style "padding" "0.75rem"
                            , Attr.style "border-radius" "0.25rem"
                            , Attr.style "border" "1px solid #0ea5e9"
                            , Attr.style "font-family" "monospace"
                            , Attr.style "font-size" "1.25rem"
                            , Attr.style "font-weight" "600"
                            , Attr.style "text-align" "center"
                            , Attr.style "letter-spacing" "0.25em"
                            , Attr.style "color" "#0c4a6e"
                            ]
                            [ Html.text (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault (extractAccessCodeFromUrl (Maybe.withDefault "" model.hostname ++ createTeamUrl team.slug team.id "") |> Maybe.withDefault "????")) ]
                        , Html.button
                            [ Events.onClick (CopyToClipboard (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault (extractAccessCodeFromUrl (Maybe.withDefault "" model.hostname ++ createTeamUrl team.slug team.id "") |> Maybe.withDefault "????")))
                            , Attr.style "background-color" "#0ea5e9"
                            , Attr.style "color" "white"
                            , Attr.style "border" "none"
                            , Attr.style "border-radius" "0.25rem"
                            , Attr.style "padding" "0.5rem"
                            , Attr.style "font-size" "0.875rem"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "transition" "background-color 0.2s"
                            , Attr.style "min-width" "40px"
                            , Attr.style "min-height" "40px"
                            , Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "justify-content" "center"
                            ]
                            [ Html.text "📋" ]
                        ]
                    , Html.p
                        [ Attr.style "color" "#0c4a6e"
                        , Attr.style "font-size" "0.875rem"
                        , Attr.style "margin-top" "0.5rem"
                        , Attr.style "margin-bottom" "0"
                        ]
                        [ Html.text "Teile diesen Code mit deinen Teammitgliedern, damit sie Zugang erhalten." ]
                    ]
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
                        [ case QRCode.fromString (Maybe.withDefault "https://localhost:8000" model.hostname ++ createTeamUrl team.slug team.id (Dict.get team.id model.confirmedTeamCodes |> Maybe.withDefault "")) of
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


viewImportIcsModal : Model -> Team -> Html FrontendMsg
viewImportIcsModal model team =
    Html.div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "import-ics-modal-title"
        , Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background-color" "rgba(0,0,0,0.5)"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "z-index" "1000"
        , Events.onClick HideImportIcsModal
        ]
        [ Html.div
            [ Attr.style "background-color" "white"
            , Attr.style "border-radius" "0.75rem"
            , Attr.style "box-shadow" "0 10px 25px rgba(0,0,0,0.25)"
            , Attr.style "max-width" "600px"
            , Attr.style "width" "95%"
            , Attr.style "margin" "1rem"
            , Attr.style "max-height" "90vh"
            , Attr.style "overflow-y" "auto"
            , Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOpFrontendMsg, True ))
            ]
            [ Html.div
                [ Attr.style "padding" "1.5rem"
                , Attr.style "border-bottom" "1px solid #e5e7eb"
                , Attr.style "display" "flex"
                , Attr.style "justify-content" "space-between"
                , Attr.style "align-items" "center"
                ]
                [ Html.h3
                    [ Attr.id "import-ics-modal-title"
                    , Attr.style "font-size" "1.25rem"
                    , Attr.style "font-weight" "600"
                    , Attr.style "color" "#1e293b"
                    , Attr.style "margin" "0"
                    ]
                    [ Html.text "ICS-Datei importieren" ]
                , Html.button
                    [ Events.onClick HideImportIcsModal
                    , Attr.style "background" "none"
                    , Attr.style "border" "none"
                    , Attr.style "font-size" "1.5rem"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "color" "#6b7280"
                    ]
                    [ Html.text "×" ]
                ]
            , Html.div
                [ Attr.style "padding" "1.5rem" ]
                [ Html.p
                    [ Attr.style "color" "#64748b"
                    , Attr.style "margin-bottom" "1rem"
                    , Attr.style "font-size" "0.875rem"
                    ]
                    [ Html.text "Wähle eine ICS-Datei aus, um alle Spiele automatisch zu importieren. Die ICS-Datei sollte im iCalendar-Format vorliegen." ]
                , Html.div
                    [ Attr.style "margin-bottom" "1rem" ]
                    [ Html.label
                        [ Attr.style "display" "block"
                        , Attr.style "font-weight" "500"
                        , Attr.style "color" "#374151"
                        , Attr.style "margin-bottom" "0.5rem"
                        ]
                        [ Html.text "ICS-Datei *" ]
                    , Html.button
                        [ Events.onClick IcsFileSelectButtonClicked
                        , Attr.style "width" "100%"
                        , Attr.style "padding" "0.75rem"
                        , Attr.style "border" "2px dashed #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "#f9fafb"
                        , Attr.style "color" "#374151"
                        , Attr.style "font-size" "1rem"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "0.5rem"
                        , Attr.style "transition" "all 0.2s"
                        ]
                        [ Html.text "📁 Datei auswählen" ]
                    ]
                , case model.icsImportStatus of
                    Just status ->
                        Html.div
                            [ Attr.style "padding" "0.75rem"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "margin-bottom" "1rem"
                            , Attr.style "background-color"
                                (if String.startsWith "Erfolgreich" status then
                                    "#d1fae5"

                                 else if String.startsWith "Fehler" status then
                                    "#fee2e2"

                                 else
                                    "#fef3c7"
                                )
                            , Attr.style "color"
                                (if String.startsWith "Erfolgreich" status then
                                    "#065f46"

                                 else if String.startsWith "Fehler" status then
                                    "#991b1b"

                                 else
                                    "#92400e"
                                )
                            , Attr.style "font-size" "0.875rem"
                            ]
                            [ Html.text status ]

                    Nothing ->
                        Html.text ""
                , if List.isEmpty model.parsedIcsMatches then
                    Html.text ""

                  else
                    Html.div
                        [ Attr.style "margin-top" "1.5rem"
                        , Attr.style "margin-bottom" "1rem"
                        ]
                        [ Html.div
                            [ Attr.style "display" "flex"
                            , Attr.style "justify-content" "space-between"
                            , Attr.style "align-items" "center"
                            , Attr.style "margin-bottom" "1rem"
                            ]
                            [ Html.h4
                                [ Attr.style "font-size" "1rem"
                                , Attr.style "font-weight" "600"
                                , Attr.style "color" "#1e293b"
                                , Attr.style "margin" "0"
                                ]
                                [ Html.text ("Gefundene Spiele (" ++ String.fromInt (List.length model.allParsedIcsMatches) ++ ")") ]
                            ]
                        , viewParsedMatchesTable model.icsImportSelectedMatches model.allParsedIcsMatches
                        ]
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "gap" "0.75rem"
                    , Attr.style "justify-content" "flex-end"
                    ]
                    [ Html.button
                        [ Events.onClick HideImportIcsModal
                        , Attr.style "padding" "0.75rem 1.5rem"
                        , Attr.style "border" "1px solid #d1d5db"
                        , Attr.style "border-radius" "0.375rem"
                        , Attr.style "background-color" "white"
                        , Attr.style "color" "#374151"
                        , Attr.style "font-weight" "500"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "font-size" "0.875rem"
                        ]
                        [ Html.text "Abbrechen" ]
                    , let
                        selectedCount =
                            model.allParsedIcsMatches
                                |> List.indexedMap Tuple.pair
                                |> List.filterMap
                                    (\( index, _ ) ->
                                        if Dict.get index model.icsImportSelectedMatches |> Maybe.withDefault False then
                                            Just index

                                        else
                                            Nothing
                                    )
                                |> List.length
                      in
                      if selectedCount == 0 then
                        Html.text ""

                      else
                        Html.button
                            [ Events.onClick (ConfirmImportIcs team.id)
                            , Attr.style "padding" "0.75rem 1.5rem"
                            , Attr.style "border" "none"
                            , Attr.style "border-radius" "0.375rem"
                            , Attr.style "background-color" "#10b981"
                            , Attr.style "color" "white"
                            , Attr.style "font-weight" "500"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "font-size" "0.875rem"
                            ]
                            [ Html.text ("Importieren (" ++ String.fromInt selectedCount ++ ")") ]
                    ]
                ]
            ]
        ]
