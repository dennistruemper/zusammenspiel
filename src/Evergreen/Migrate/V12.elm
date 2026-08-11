module Evergreen.Migrate.V12 exposing (..)

{-| Migration from V11 to V12.

Changes:
  - Match stores canonical UTC start time (`startUtc`) instead of local date/time strings
  - ICS import UI caches local display times in `utcToLocalMap`
  - Manual match creation converts local input to UTC before sending to backend

-}

import Dict
import Evergreen.V11.IcsParser
import Evergreen.V11.Types
import Evergreen.V12.IcsParser
import Evergreen.V12.Types
import Lamdera.Migrations exposing (..)
import List
import Utils exposing (germanDateTimeToStartUtc)


frontendModel : Evergreen.V11.Types.FrontendModel -> ModelMigration Evergreen.V12.Types.FrontendModel Evergreen.V12.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V11.Types.BackendModel -> ModelMigration Evergreen.V12.Types.BackendModel Evergreen.V12.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V11.Types.FrontendMsg -> MsgMigration Evergreen.V12.Types.FrontendMsg Evergreen.V12.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V11.Types.ToBackend -> MsgMigration Evergreen.V12.Types.ToBackend Evergreen.V12.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V11.Types.BackendMsg -> MsgMigration Evergreen.V12.Types.BackendMsg Evergreen.V12.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V11.Types.ToFrontend -> MsgMigration Evergreen.V12.Types.ToFrontend Evergreen.V12.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V11.Types.BackendModel -> Evergreen.V12.Types.BackendModel
migrate_Types_BackendModel old =
    { teams = old.teams |> Dict.map (\_ -> migrate_Types_TeamData)
    , nextId = old.nextId
    , randomSeed = old.randomSeed
    , teamSessions = old.teamSessions
    }


migrate_Types_FrontendModel : Evergreen.V11.Types.FrontendModel -> Evergreen.V12.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , page = old.page |> migrate_Types_Page
    , currentTeam = old.currentTeam
    , activeMemberId = old.activeMemberId
    , createTeamForm = old.createTeamForm |> migrate_Types_CreateTeamForm
    , createMatchForm = old.createMatchForm |> migrate_Types_CreateMatchForm
    , createMemberForm = old.createMemberForm |> migrate_Types_CreateMemberForm
    , matches = old.matches |> List.map migrate_Types_Match
    , members = old.members
    , availability = old.availability |> List.map migrate_Types_AvailabilityRecord
    , showCreateMatchModal = old.showCreateMatchModal
    , showCreateMemberModal = old.showCreateMemberModal
    , showMemberSelectionModal = old.showMemberSelectionModal
    , showCreateMemberInModal = old.showCreateMemberInModal
    , showChangeMatchDateModal = old.showChangeMatchDateModal
    , changeMatchDateForm = old.changeMatchDateForm
    , changeMatchDateMatchId = old.changeMatchDateMatchId
    , showShareModal = old.showShareModal
    , showImportIcsModal = old.showImportIcsModal
    , showAddMatchDropdown = old.showAddMatchDropdown
    , icsImportUrl = old.icsImportUrl
    , icsImportStatus = old.icsImportStatus
    , parsedIcsMatches = old.parsedIcsMatches |> List.map migrate_IcsParser_ParsedMatch
    , allParsedIcsMatches = old.allParsedIcsMatches |> List.map migrate_IcsParser_ParsedMatch
    , icsImportSelectedMatches = old.icsImportSelectedMatches
    , expandedMatches = old.expandedMatches
    , expandedPredictions = old.expandedPredictions
    , pastMatchesShown = old.pastMatchesShown
    , pastMatchesExpanded = old.pastMatchesExpanded
    , hostname = old.hostname
    , currentDate = old.currentDate
    , confirmedTeamCodes = old.confirmedTeamCodes
    , accessCodeRequired = old.accessCodeRequired
    , enteredAccessCode = old.enteredAccessCode
    , datePredictions = old.datePredictions |> Dict.map (\_ -> Dict.map (\_ -> Dict.map (\_ -> migrate_Types_DatePrediction)))
    , showDatePredictionModal = old.showDatePredictionModal
    , datePredictionMatchId = old.datePredictionMatchId
    , datePredictionForm = old.datePredictionForm
    , reservePlayers = old.reservePlayers
    , showAddReservePlayerModal = old.showAddReservePlayerModal
    , addReservePlayerMatchId = old.addReservePlayerMatchId
    , addReservePlayerForm = old.addReservePlayerForm
    , utcToLocalMap = Dict.empty
    , icsImportConverting = False
    , pendingLocalToUtc = Nothing
    }


migrate_IcsParser_ParsedMatch : Evergreen.V11.IcsParser.ParsedMatch -> Evergreen.V12.IcsParser.ParsedMatch
migrate_IcsParser_ParsedMatch old =
    { opponent = old.opponent
    , date = old.date
    , time = old.time
    , venue = old.venue
    , isHome = old.isHome
    , startUtc = ""
    }


migrate_Types_Availability : Evergreen.V11.Types.Availability -> Evergreen.V12.Types.Availability
migrate_Types_Availability old =
    case old of
        Evergreen.V11.Types.Available ->
            Evergreen.V12.Types.Available

        Evergreen.V11.Types.NotAvailable ->
            Evergreen.V12.Types.NotAvailable

        Evergreen.V11.Types.Maybe ->
            Evergreen.V12.Types.Maybe


migrate_Types_AvailabilityRecord : Evergreen.V11.Types.AvailabilityRecord -> Evergreen.V12.Types.AvailabilityRecord
migrate_Types_AvailabilityRecord old =
    { memberId = old.memberId
    , matchId = old.matchId
    , availability = old.availability |> migrate_Types_Availability
    }


migrate_Types_CreateMatchForm : Evergreen.V11.Types.CreateMatchForm -> Evergreen.V12.Types.CreateMatchForm
migrate_Types_CreateMatchForm old =
    old


migrate_Types_CreateMatchPayload : Evergreen.V11.Types.CreateMatchForm -> Evergreen.V12.Types.CreateMatchPayload
migrate_Types_CreateMatchPayload old =
    { opponent = old.opponent
    , startUtc = germanDateTimeToStartUtc old.date old.time
    , venue = old.venue
    , isHome = old.isHome
    }


migrate_Types_CreateMemberForm : Evergreen.V11.Types.CreateMemberForm -> Evergreen.V12.Types.CreateMemberForm
migrate_Types_CreateMemberForm old =
    old


migrate_Types_CreateTeamForm : Evergreen.V11.Types.CreateTeamForm -> Evergreen.V12.Types.CreateTeamForm
migrate_Types_CreateTeamForm old =
    old


migrate_Types_DatePrediction : Evergreen.V11.Types.DatePrediction -> Evergreen.V12.Types.DatePrediction
migrate_Types_DatePrediction old =
    { predictedDate = old.predictedDate
    , memberId = old.memberId
    , availability = old.availability |> migrate_Types_Availability
    }


migrate_Types_Match : Evergreen.V11.Types.Match -> Evergreen.V12.Types.Match
migrate_Types_Match old =
    { id = old.id
    , opponent = old.opponent
    , startUtc = germanDateTimeToStartUtc old.date old.time
    , isHome = old.isHome
    , venue = old.venue
    , season = old.season
    , seasonHalf = old.seasonHalf |> migrate_Types_SeasonHalf
    , matchday = old.matchday
    , originalStartUtc =
        case old.originalDate of
            Just originalDate ->
                Just (germanDateTimeToStartUtc originalDate old.time)

            Nothing ->
                Nothing
    }


migrate_Types_Member : Evergreen.V11.Types.Member -> Evergreen.V12.Types.Member
migrate_Types_Member old =
    old


migrate_Types_Page : Evergreen.V11.Types.Page -> Evergreen.V12.Types.Page
migrate_Types_Page old =
    case old of
        Evergreen.V11.Types.HomePage ->
            Evergreen.V12.Types.HomePage

        Evergreen.V11.Types.CreateTeamPage ->
            Evergreen.V12.Types.CreateTeamPage

        Evergreen.V11.Types.TeamPage p0 ->
            Evergreen.V12.Types.TeamPage p0

        Evergreen.V11.Types.NotFoundPage ->
            Evergreen.V12.Types.NotFoundPage


migrate_Types_SeasonData : Evergreen.V11.Types.SeasonData -> Evergreen.V12.Types.SeasonData
migrate_Types_SeasonData old =
    { hinrunde = old.hinrunde |> List.map migrate_Types_Match
    , rückrunde = old.rückrunde |> List.map migrate_Types_Match
    }


migrate_Types_SeasonHalf : Evergreen.V11.Types.SeasonHalf -> Evergreen.V12.Types.SeasonHalf
migrate_Types_SeasonHalf old =
    case old of
        Evergreen.V11.Types.Hinrunde ->
            Evergreen.V12.Types.Hinrunde

        Evergreen.V11.Types.Rückrunde ->
            Evergreen.V12.Types.Rückrunde


migrate_Types_Team : Evergreen.V11.Types.Team -> Evergreen.V12.Types.Team
migrate_Types_Team old =
    old


migrate_Types_TeamData : Evergreen.V11.Types.TeamData -> Evergreen.V12.Types.TeamData
migrate_Types_TeamData old =
    { team = old.team |> migrate_Types_Team
    , seasons = old.seasons |> Dict.map (\_ -> migrate_Types_SeasonData)
    , members = old.members
    , availability = old.availability |> Dict.map (\_ -> Dict.map (\_ -> migrate_Types_Availability))
    , datePredictions = old.datePredictions |> Dict.map (\_ -> Dict.map (\_ -> Dict.map (\_ -> migrate_Types_DatePrediction)))
    , reservePlayers = old.reservePlayers
    }


migrate_Types_ToBackend : Evergreen.V11.Types.ToBackend -> Evergreen.V12.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V11.Types.CreateTeamRequest p0 p1 p2 p3 p4 ->
            Evergreen.V12.Types.CreateTeamRequest p0 p1 p2 p3 p4

        Evergreen.V11.Types.GetTeamRequest p0 p1 ->
            Evergreen.V12.Types.GetTeamRequest p0 p1

        Evergreen.V11.Types.SubmitAccessCode p0 p1 ->
            Evergreen.V12.Types.SubmitAccessCode p0 p1

        Evergreen.V11.Types.CreateMatchRequest p0 p1 p2 ->
            Evergreen.V12.Types.CreateMatchRequest p0 (p1 |> migrate_Types_CreateMatchPayload) p2

        Evergreen.V11.Types.CreateMemberRequest p0 p1 p2 ->
            Evergreen.V12.Types.CreateMemberRequest p0 (p1 |> migrate_Types_CreateMemberForm) p2

        Evergreen.V11.Types.UpdateAvailabilityRequest p0 p1 p2 p3 ->
            Evergreen.V12.Types.UpdateAvailabilityRequest p0 p1 (p2 |> migrate_Types_Availability) p3

        Evergreen.V11.Types.ChangeMatchDateRequest matchId newDate teamId accessCode ->
            Evergreen.V12.Types.ChangeMatchDateRequest matchId (germanDateTimeToStartUtc newDate "00:00") teamId accessCode

        Evergreen.V11.Types.AddDatePredictionRequest p0 p1 p2 p3 ->
            Evergreen.V12.Types.AddDatePredictionRequest p0 p1 p2 p3

        Evergreen.V11.Types.UpdatePredictionAvailabilityRequest p0 p1 p2 p3 p4 ->
            Evergreen.V12.Types.UpdatePredictionAvailabilityRequest p0 p1 p2 (p3 |> migrate_Types_Availability) p4

        Evergreen.V11.Types.RemoveDatePredictionRequest p0 p1 p2 ->
            Evergreen.V12.Types.RemoveDatePredictionRequest p0 p1 p2

        Evergreen.V11.Types.ChoosePredictedDateRequest matchId chosenDate teamId accessCode ->
            Evergreen.V12.Types.ChoosePredictedDateRequest matchId chosenDate (germanDateTimeToStartUtc chosenDate "00:00") teamId accessCode

        Evergreen.V11.Types.AddReservePlayerRequest p0 p1 p2 p3 ->
            Evergreen.V12.Types.AddReservePlayerRequest p0 p1 p2 p3

        Evergreen.V11.Types.RemoveReservePlayerRequest p0 p1 p2 p3 ->
            Evergreen.V12.Types.RemoveReservePlayerRequest p0 p1 p2 p3

        Evergreen.V11.Types.NoOpToBackend ->
            Evergreen.V12.Types.NoOpToBackend


migrate_Types_ToFrontend : Evergreen.V11.Types.ToFrontend -> Evergreen.V12.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V11.Types.TeamCreated p0 p1 p2 ->
            Evergreen.V12.Types.TeamCreated (p0 |> migrate_Types_Team) p1 p2

        Evergreen.V11.Types.TeamLoaded p0 p1 p2 p3 p4 p5 ->
            Evergreen.V12.Types.TeamLoaded (p0 |> migrate_Types_Team)
                (p1 |> List.map migrate_Types_Match)
                p2
                (p3 |> List.map migrate_Types_AvailabilityRecord)
                (p4 |> Dict.map (\_ -> Dict.map (\_ -> Dict.map (\_ -> migrate_Types_DatePrediction))))
                p5

        Evergreen.V11.Types.TeamNotFound ->
            Evergreen.V12.Types.TeamNotFound

        Evergreen.V11.Types.AccessCodeRequired p0 ->
            Evergreen.V12.Types.AccessCodeRequired p0

        Evergreen.V11.Types.MatchCreated p0 ->
            Evergreen.V12.Types.MatchCreated (p0 |> migrate_Types_Match)

        Evergreen.V11.Types.MemberCreated p0 ->
            Evergreen.V12.Types.MemberCreated (p0 |> migrate_Types_Member)

        Evergreen.V11.Types.AvailabilityUpdated p0 ->
            Evergreen.V12.Types.AvailabilityUpdated (p0 |> migrate_Types_AvailabilityRecord)

        Evergreen.V11.Types.MatchDateChanged matchId newDate ->
            Evergreen.V12.Types.MatchDateChanged matchId (germanDateTimeToStartUtc newDate "00:00")

        Evergreen.V11.Types.DatePredictionAdded p0 p1 ->
            Evergreen.V12.Types.DatePredictionAdded (p0 |> migrate_Types_DatePrediction) p1

        Evergreen.V11.Types.DatePredictionUpdated p0 p1 ->
            Evergreen.V12.Types.DatePredictionUpdated (p0 |> migrate_Types_DatePrediction) p1

        Evergreen.V11.Types.DatePredictionRemoved p0 p1 ->
            Evergreen.V12.Types.DatePredictionRemoved p0 p1

        Evergreen.V11.Types.PredictionsCleared p0 ->
            Evergreen.V12.Types.PredictionsCleared p0

        Evergreen.V11.Types.MatchOriginalDateSet matchId originalDate ->
            Evergreen.V12.Types.MatchOriginalDateSet matchId (germanDateTimeToStartUtc originalDate "00:00")

        Evergreen.V11.Types.ReservePlayerAdded p0 p1 p2 ->
            Evergreen.V12.Types.ReservePlayerAdded p0 p1 p2

        Evergreen.V11.Types.ReservePlayerRemoved p0 p1 p2 ->
            Evergreen.V12.Types.ReservePlayerRemoved p0 p1 p2

        Evergreen.V11.Types.NoOpToFrontend ->
            Evergreen.V12.Types.NoOpToFrontend
