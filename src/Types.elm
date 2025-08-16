module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
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


type alias SeasonData =
    { hinrunde : List Match
    , rückrunde : List Match
    }


type alias TeamData =
    { team : Team
    , seasons : Dict Season SeasonData -- season -> matches organized by half
    , members : Dict String Member -- memberId -> Member
    , availability : Dict String (Dict String Availability) -- memberId -> (matchId -> Availability)
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
    , expandedMatches : List String -- Match IDs that are expanded
    , pastMatchesShown : Int -- Number of past matches currently shown
    , pastMatchesExpanded : Bool -- Whether past matches section is expanded
    , hostname : Maybe String -- Current hostname from JavaScript
    , confirmedTeamCodes : Dict TeamId String -- teamId -> accessCode
    , accessCodeRequired : Maybe TeamId
    , enteredAccessCode : String -- Track the currently entered access code
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
    | LogoutRequested
    | SetAvailability String String Availability
    | ToggleMatchDetails String
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
    | NoOpFrontendMsg


type ToBackend
    = CreateTeamRequest String String (List String) Int String
    | GetTeamRequest TeamId String -- teamId, accessCode
    | SubmitAccessCode TeamId String -- teamId, accessCode
    | CreateMatchRequest TeamId CreateMatchForm String -- teamId, form, accessCode
    | CreateMemberRequest TeamId CreateMemberForm String -- teamId, form, accessCode
    | UpdateAvailabilityRequest String String Availability String -- memberId, matchId, availability, accessCode
    | ChangeMatchDateRequest String String String String -- matchId, newDate, teamId, accessCode
    | NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TeamCreated Team String String -- Team, creator member ID, and access code
    | TeamLoaded Team (List Match) (List Member) (List AvailabilityRecord)
    | TeamNotFound
    | AccessCodeRequired TeamId -- Request access code for team access
    | MatchCreated Match
    | MemberCreated Member
    | AvailabilityUpdated AvailabilityRecord
    | MatchDateChanged String String -- matchId, newDate
    | NoOpToFrontend
