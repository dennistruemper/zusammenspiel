module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import File exposing (File)
import IcsParser exposing (ParsedMatch)
import Lamdera exposing (SessionId)
import Random
import Url exposing (Url)



-- SHARED TYPES


type alias TeamId =
    String


type alias Team =
    { id : TeamId
    , name : String
    , slug : String
    , playersNeeded : Int
    , createdAt : Int
    , accessCode : String
    }


type alias Season =
    String



-- e.g., "2024/25"


type SeasonHalf
    = Hinrunde -- First half of season
    | Rückrunde -- Second half of season


type alias Match =
    { id : String
    , opponent : String
    , date : String -- German format for display: "25.12.2024"
    , time : String -- "14:30"
    , isHome : Bool
    , venue : String
    , season : Season
    , seasonHalf : SeasonHalf
    , matchday : Int -- Spieltag number within the half
    , originalDate : Maybe String -- Original date before predictions (Nothing if never changed)
    }


type alias Member =
    { id : String
    , teamId : TeamId
    , name : String
    }


type Availability
    = Available
    | NotAvailable
    | Maybe


type alias AvailabilityRecord =
    { memberId : String
    , matchId : String
    , availability : Availability
    }


type alias DatePrediction =
    { predictedDate : String -- German format: "25.12.2024"
    , memberId : String
    , availability : Availability
    }


type alias SeasonData =
    { hinrunde : List Match
    , rückrunde : List Match
    }


type alias TeamData =
    { team : Team
    , seasons : Dict Season SeasonData -- season -> matches organized by half
    , members : Dict String Member -- memberId -> Member
    , availability : Dict String (Dict String Availability) -- memberId -> (matchId -> Availability)
    , datePredictions : Dict String (Dict String (Dict String DatePrediction)) -- matchId -> (predictedDate -> (memberId -> DatePrediction))
    }



-- FRONTEND MODEL


type alias FrontendModel =
    { key : Key
    , page : Page
    , currentTeam : Maybe Team
    , activeMemberId : Maybe String
    , createTeamForm : CreateTeamForm
    , createMatchForm : CreateMatchForm
    , createMemberForm : CreateMemberForm
    , matches : List Match
    , members : List Member
    , availability : List AvailabilityRecord
    , showCreateMatchModal : Bool
    , showCreateMemberModal : Bool
    , showMemberSelectionModal : Bool
    , showCreateMemberInModal : Bool
    , showChangeMatchDateModal : Bool
    , changeMatchDateForm : String -- New date for the match being edited
    , changeMatchDateMatchId : Maybe String -- ID of the match being edited
    , showShareModal : Bool
    , showImportIcsModal : Bool
    , showAddMatchDropdown : Bool -- Whether the "Add Match" dropdown menu is open
    , icsImportUrl : String -- URL for ICS file import
    , icsImportStatus : Maybe String -- Status message for ICS import (error or success)
    , parsedIcsMatches : List ParsedMatch -- Parsed matches waiting for confirmation
    , allParsedIcsMatches : List ParsedMatch -- All parsed matches before filtering
    , icsImportSelectedMatches : Dict Int Bool -- Index -> selected state for import (Int is index in allParsedIcsMatches)
    , expandedMatches : List String -- Match IDs that are expanded
    , expandedPredictions : List ( String, String ) -- (matchId, predictedDate) pairs that are expanded
    , pastMatchesShown : Int -- Number of past matches currently shown
    , pastMatchesExpanded : Bool -- Whether past matches section is expanded
    , hostname : Maybe String -- Current hostname from JavaScript
    , currentDate : Maybe String -- Current date in German format (dd.mm.yyyy)
    , confirmedTeamCodes : Dict TeamId String -- teamId -> accessCode
    , accessCodeRequired : Maybe TeamId
    , enteredAccessCode : String -- Track the currently entered access code
    , datePredictions : Dict String (Dict String (Dict String DatePrediction)) -- matchId -> (predictedDate -> (memberId -> DatePrediction))
    , showDatePredictionModal : Bool
    , datePredictionMatchId : Maybe String -- ID of match for which we're adding/editing predictions
    , datePredictionForm : String -- Date input for new prediction
    }


type Page
    = HomePage
    | CreateTeamPage
    | TeamPage TeamId
    | NotFoundPage


type alias CreateTeamForm =
    { name : String
    , creatorName : String
    , otherMemberNames : String
    , playersNeeded : String
    , accessCode : String
    }


type alias CreateMatchForm =
    { opponent : String
    , date : String
    , time : String
    , venue : String
    , isHome : Bool
    }


type alias CreateMemberForm =
    { name : String
    }



-- BACKEND MODEL


type alias BackendModel =
    { teams : Dict TeamId TeamData -- teamId -> TeamData
    , nextId : Int
    , randomSeed : Random.Seed
    , teamSessions : Dict TeamId (List SessionId) -- Track which sessions are viewing each team
    }



-- MESSAGES


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | CreateTeamFormUpdated CreateTeamForm
    | CreateTeamSubmitted
    | CreateMatchFormUpdated CreateMatchForm
    | CreateMatchSubmitted TeamId
    | ShowCreateMatchModal
    | HideCreateMatchModal
    | CreateMemberFormUpdated CreateMemberForm
    | CreateMemberSubmitted TeamId
    | ShowCreateMemberModal
    | HideCreateMemberModal
    | ShowMemberSelectionModal
    | HideMemberSelectionModal
    | SelectActiveMember String
    | ShowCreateMemberInModal
    | HideCreateMemberInModal
    | LocalStorageMessage String
    | UpdateCurrentDate -- Periodic update to refresh current date
    | LogoutRequested
    | SetAvailability String String Availability
    | ToggleMatchDetails String
    | TogglePredictionDetails String String -- matchId, predictedDate
    | LoadMorePastMatches
    | TogglePastMatchesSection
    | ShowChangeMatchDateModal String
    | HideChangeMatchDateModal
    | ChangeMatchDateFormUpdated String String
    | ChangeMatchDateSubmitted String String
    | ShowShareModal
    | HideShareModal
    | SubmitAccessCodeRequested TeamId String -- teamId, accessCode
    | AccessCodeInputChanged TeamId String -- teamId, accessCode (for input field)
    | GetAccessCode TeamId -- Request access code from localStorage
    | AccessCodeLoaded TeamId String -- Access code loaded from localStorage (teamId, accessCode)
    | CopyToClipboard String -- Copy text to clipboard
    | ShowImportIcsModal
    | HideImportIcsModal
    | ToggleAddMatchDropdown -- Toggle the add match dropdown menu
    | IcsFileSelectButtonClicked -- User clicked file select button
    | IcsFileSelected File -- File was selected (File type from elm/file)
    | IcsFileContentRead String -- File content was read
    | IcsImportMatchToggled Int Bool -- Toggle selection for a specific match (index, selected)
    | ConfirmImportIcs TeamId -- Confirm and import the parsed matches
    | ShowDatePredictionModal String -- matchId
    | HideDatePredictionModal
    | DatePredictionFormUpdated String -- new date string
    | AddDatePrediction String String -- matchId, predictedDate
    | RemoveDatePrediction String -- matchId
    | UpdatePredictionAvailability String String Availability -- matchId, predictedDate, availability
    | ChoosePredictedDate String String -- matchId, chosenDate
    | NoOpFrontendMsg


type ToBackend
    = CreateTeamRequest String String (List String) Int String
    | GetTeamRequest TeamId String -- teamId, accessCode
    | SubmitAccessCode TeamId String -- teamId, accessCode
    | CreateMatchRequest TeamId CreateMatchForm String -- teamId, form, accessCode
    | CreateMemberRequest TeamId CreateMemberForm String -- teamId, form, accessCode
    | UpdateAvailabilityRequest String String Availability String -- memberId, matchId, availability, accessCode
    | ChangeMatchDateRequest String String String String -- matchId, newDate, teamId, accessCode
    | AddDatePredictionRequest String String String String -- matchId, predictedDate, memberId, accessCode
    | UpdatePredictionAvailabilityRequest String String String Availability String -- matchId, predictedDate, memberId, availability, accessCode
    | RemoveDatePredictionRequest String String String -- matchId, memberId, accessCode
    | ChoosePredictedDateRequest String String String String -- matchId, chosenDate, teamId, accessCode
    | NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TeamCreated Team String String -- Team, creator member ID, and access code
    | TeamLoaded Team (List Match) (List Member) (List AvailabilityRecord) (Dict String (Dict String (Dict String DatePrediction))) -- Team, matches, members, availability, datePredictions
    | TeamNotFound
    | AccessCodeRequired TeamId -- Request access code for team access
    | MatchCreated Match
    | MemberCreated Member
    | AvailabilityUpdated AvailabilityRecord
    | MatchDateChanged String String -- matchId, newDate
    | DatePredictionAdded DatePrediction String -- DatePrediction, matchId
    | DatePredictionUpdated DatePrediction String -- DatePrediction, matchId
    | DatePredictionRemoved String String -- matchId, memberId
    | PredictionsCleared String -- matchId
    | MatchOriginalDateSet String String -- matchId, originalDate
    | NoOpToFrontend
