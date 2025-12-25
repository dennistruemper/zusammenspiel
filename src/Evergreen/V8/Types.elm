module Evergreen.V8.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V8.IcsParser
import File
import Lamdera
import Random
import Url


type alias TeamId =
    String


type Page
    = HomePage
    | CreateTeamPage
    | TeamPage TeamId
    | NotFoundPage


type alias Team =
    { id : TeamId
    , name : String
    , slug : String
    , playersNeeded : Int
    , createdAt : Int
    , accessCode : String
    }


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


type alias Season =
    String


type SeasonHalf
    = Hinrunde
    | Rückrunde


type alias Match =
    { id : String
    , opponent : String
    , date : String
    , time : String
    , isHome : Bool
    , venue : String
    , season : Season
    , seasonHalf : SeasonHalf
    , matchday : Int
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


type alias FrontendModel =
    { key : Browser.Navigation.Key
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
    , changeMatchDateForm : String
    , changeMatchDateMatchId : Maybe String
    , showShareModal : Bool
    , showImportIcsModal : Bool
    , showAddMatchDropdown : Bool
    , icsImportUrl : String
    , icsImportStatus : Maybe String
    , parsedIcsMatches : List Evergreen.V8.IcsParser.ParsedMatch
    , allParsedIcsMatches : List Evergreen.V8.IcsParser.ParsedMatch
    , icsImportSelectedMatches : Dict.Dict Int Bool
    , expandedMatches : List String
    , pastMatchesShown : Int
    , pastMatchesExpanded : Bool
    , hostname : Maybe String
    , currentDate : Maybe String
    , confirmedTeamCodes : Dict.Dict TeamId String
    , accessCodeRequired : Maybe TeamId
    , enteredAccessCode : String
    }


type alias SeasonData =
    { hinrunde : List Match
    , rückrunde : List Match
    }


type alias TeamData =
    { team : Team
    , seasons : Dict.Dict Season SeasonData
    , members : Dict.Dict String Member
    , availability : Dict.Dict String (Dict.Dict String Availability)
    }


type alias BackendModel =
    { teams : Dict.Dict TeamId TeamData
    , nextId : Int
    , randomSeed : Random.Seed
    , teamSessions : Dict.Dict TeamId (List Lamdera.SessionId)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | UpdateCurrentDate
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
    | SubmitAccessCodeRequested TeamId String
    | AccessCodeInputChanged TeamId String
    | GetAccessCode TeamId
    | AccessCodeLoaded TeamId String
    | CopyToClipboard String
    | ShowImportIcsModal
    | HideImportIcsModal
    | ToggleAddMatchDropdown
    | IcsFileSelectButtonClicked
    | IcsFileSelected File.File
    | IcsFileContentRead String
    | IcsImportMatchToggled Int Bool
    | ConfirmImportIcs TeamId
    | NoOpFrontendMsg


type ToBackend
    = CreateTeamRequest String String (List String) Int String
    | GetTeamRequest TeamId String
    | SubmitAccessCode TeamId String
    | CreateMatchRequest TeamId CreateMatchForm String
    | CreateMemberRequest TeamId CreateMemberForm String
    | UpdateAvailabilityRequest String String Availability String
    | ChangeMatchDateRequest String String String String
    | NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TeamCreated Team String String
    | TeamLoaded Team (List Match) (List Member) (List AvailabilityRecord)
    | TeamNotFound
    | AccessCodeRequired TeamId
    | MatchCreated Match
    | MemberCreated Member
    | AvailabilityUpdated AvailabilityRecord
    | MatchDateChanged String String
    | NoOpToFrontend
