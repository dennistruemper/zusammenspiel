module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Random
import Url exposing (Url)



-- SHARED TYPES


type alias TeamId =
    String


type alias Team =
    { id : TeamId
    , name : String
    , slug : String
    , createdAt : Int
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
    , date : String -- ISO format for easy sorting: "2024-12-25"
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
    , createTeamForm : CreateTeamForm
    , createMatchForm : CreateMatchForm
    , matches : List Match
    , members : List Member
    , availability : List AvailabilityRecord
    , showCreateMatchModal : Bool
    }


type Page
    = HomePage
    | CreateTeamPage
    | TeamPage TeamId
    | NotFoundPage


type alias CreateTeamForm =
    { name : String
    }


type alias CreateMatchForm =
    { opponent : String
    , date : String
    , time : String
    , venue : String
    , isHome : Bool
    }



-- BACKEND MODEL


type alias BackendModel =
    { teams : Dict TeamId TeamData -- teamId -> TeamData
    , nextId : Int
    , randomSeed : Random.Seed
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
    | NoOpFrontendMsg


type ToBackend
    = CreateTeamRequest String
    | GetTeamRequest TeamId
    | CreateMatchRequest TeamId CreateMatchForm
    | NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TeamCreated Team
    | TeamLoaded Team (List Match) (List Member) (List AvailabilityRecord)
    | TeamNotFound
    | MatchCreated Match
    | NoOpToFrontend
