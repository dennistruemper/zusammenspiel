module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Random
import Time
import Types exposing (..)
import Utils exposing (createSlug, generateMatchId, generateRandomTeamId, getAllMatches, getCurrentSeason, getSeasonHalf)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { teams = Dict.empty
      , nextId = 1
      , randomSeed = Random.initialSeed 42 -- Will be updated with real time
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateTeamRequest name ->
            let
                timestamp =
                    Time.posixToMillis (Time.millisToPosix model.nextId)

                ( teamId, newSeed ) =
                    generateRandomTeamId model.randomSeed

                slug =
                    createSlug name

                newTeam =
                    { id = teamId
                    , name = name
                    , slug = slug
                    , createdAt = timestamp
                    }

                newTeamData =
                    { team = newTeam
                    , seasons = Dict.empty
                    , members = Dict.empty
                    , availability = Dict.empty
                    }

                updatedModel =
                    { model
                        | teams = Dict.insert teamId newTeamData model.teams
                        , nextId = model.nextId + 1
                        , randomSeed = newSeed
                    }
            in
            ( updatedModel
            , sendToFrontend clientId (TeamCreated newTeam)
            )

        GetTeamRequest teamId ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    let
                        -- Convert all matches from all seasons to a flat list for frontend compatibility
                        teamMatches =
                            teamData.seasons
                                |> Dict.values
                                |> List.map getAllMatches
                                |> List.concat

                        teamMembers =
                            Dict.values teamData.members

                        -- Convert nested availability Dict to flat list
                        teamAvailability =
                            teamData.availability
                                |> Dict.toList
                                |> List.concatMap
                                    (\( memberId, matchAvailability ) ->
                                        matchAvailability
                                            |> Dict.toList
                                            |> List.map
                                                (\( matchId, availability ) ->
                                                    { memberId = memberId
                                                    , matchId = matchId
                                                    , availability = availability
                                                    }
                                                )
                                    )
                    in
                    ( model
                    , sendToFrontend clientId (TeamLoaded teamData.team teamMatches teamMembers teamAvailability)
                    )

                Nothing ->
                    ( model
                    , sendToFrontend clientId TeamNotFound
                    )

        CreateMatchRequest teamId matchForm ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    let
                        matchId =
                            generateMatchId model.nextId

                        -- Determine season and half from date
                        season =
                            case String.split "-" matchForm.date of
                                [ yearStr, _, _ ] ->
                                    case String.toInt yearStr of
                                        Just year ->
                                            getCurrentSeason year

                                        Nothing ->
                                            getCurrentSeason 2024

                                _ ->
                                    getCurrentSeason 2024

                        seasonHalf =
                            if getSeasonHalf matchForm.date == "Hinrunde" then
                                Hinrunde

                            else
                                Rückrunde

                        newMatch =
                            { id = matchId
                            , opponent = matchForm.opponent
                            , date = matchForm.date
                            , time = matchForm.time
                            , isHome = matchForm.isHome
                            , venue = matchForm.venue
                            , season = season
                            , seasonHalf = seasonHalf
                            , matchday = 1 -- TODO: Calculate proper matchday
                            }

                        -- Add match to appropriate season/half
                        updatedSeasonData =
                            case Dict.get season teamData.seasons of
                                Just existingSeasonData ->
                                    case seasonHalf of
                                        Hinrunde ->
                                            { existingSeasonData | hinrunde = newMatch :: existingSeasonData.hinrunde }

                                        Rückrunde ->
                                            { existingSeasonData | rückrunde = newMatch :: existingSeasonData.rückrunde }

                                Nothing ->
                                    case seasonHalf of
                                        Hinrunde ->
                                            { hinrunde = [ newMatch ], rückrunde = [] }

                                        Rückrunde ->
                                            { hinrunde = [], rückrunde = [ newMatch ] }

                        updatedTeamData =
                            { teamData | seasons = Dict.insert season updatedSeasonData teamData.seasons }

                        updatedModel =
                            { model
                                | teams = Dict.insert teamId updatedTeamData model.teams
                                , nextId = model.nextId + 1
                            }
                    in
                    ( updatedModel
                    , sendToFrontend clientId (MatchCreated newMatch)
                    )

                Nothing ->
                    ( model
                    , sendToFrontend clientId TeamNotFound
                    )

        NoOpToBackend ->
            ( model, Cmd.none )
