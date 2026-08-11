module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Random
import Time
import Types exposing (..)
import Utils exposing (createSlug, generateMatchId, generateMemberId, generateRandomAccessCode, generateRandomTeamId, getAllMatches, getCurrentSeason, getSeasonHalfFromStartUtc, getSeasonYearFromStartUtc)


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
      , randomSeed = Random.initialSeed 42
      , teamSessions = Dict.empty
      }
    , Cmd.none
    )



-- HELPER FUNCTIONS


updateMatchInSeasons matchId updateFn seasons =
    Dict.map
        (\season seasonData ->
            let
                updatedHinrunde =
                    List.map
                        (\match ->
                            if match.id == matchId then
                                updateFn match

                            else
                                match
                        )
                        seasonData.hinrunde

                updatedRückrunde =
                    List.map
                        (\match ->
                            if match.id == matchId then
                                updateFn match

                            else
                                match
                        )
                        seasonData.rückrunde
            in
            { seasonData | hinrunde = updatedHinrunde, rückrunde = updatedRückrunde }
        )
        seasons


sendToTeamSessions : TeamId -> ToFrontend -> Model -> Cmd BackendMsg
sendToTeamSessions teamId message model =
    case Dict.get teamId model.teamSessions of
        Just sessions ->
            sessions
                |> List.map (\sessionId -> sendToFrontend sessionId message)
                |> Cmd.batch

        Nothing ->
            Cmd.none


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateTeamRequest name creatorName otherMemberNames playersNeeded accessCode ->
            let
                timestamp =
                    model.nextId

                ( teamId, newSeed ) =
                    generateRandomTeamId model.randomSeed

                slug =
                    createSlug name

                ( finalAccessCode, finalSeed ) =
                    generateRandomAccessCode newSeed

                newTeam =
                    { id = teamId
                    , name = name
                    , slug = slug
                    , playersNeeded = playersNeeded
                    , createdAt = timestamp
                    , accessCode = finalAccessCode
                    }

                -- Create creator member first
                creatorMemberId =
                    generateMemberId model.nextId

                creatorMember =
                    { id = creatorMemberId
                    , teamId = teamId
                    , name = creatorName
                    }

                -- Create other members
                ( otherMembers, nextId ) =
                    List.foldl
                        (\memberName ( memberDict, idCounter ) ->
                            let
                                memberId =
                                    generateMemberId idCounter

                                member =
                                    { id = memberId
                                    , teamId = teamId
                                    , name = memberName
                                    }
                            in
                            ( Dict.insert memberId member memberDict, idCounter + 1 )
                        )
                        ( Dict.empty, model.nextId + 1 )
                        otherMemberNames

                -- Combine all members (creator + others)
                allMembers =
                    Dict.insert creatorMemberId creatorMember otherMembers

                newTeamData =
                    { team = newTeam
                    , seasons = Dict.empty
                    , members = allMembers
                    , availability = Dict.empty
                    , datePredictions = Dict.empty
                    , reservePlayers = Dict.empty
                    }

                updatedModel =
                    { model
                        | teams = Dict.insert teamId newTeamData model.teams
                        , nextId = nextId
                        , randomSeed = finalSeed
                    }
            in
            ( updatedModel
            , sendToFrontend sessionId (TeamCreated newTeam creatorMemberId finalAccessCode)
            )

        GetTeamRequest teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code or check if this is the creator
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Track this session as viewing this team
                            currentTeamSessions =
                                Dict.get teamId model.teamSessions
                                    |> Maybe.withDefault []

                            updatedTeamSessions =
                                if List.member sessionId currentTeamSessions then
                                    currentTeamSessions

                                else
                                    sessionId :: currentTeamSessions

                            updatedModel =
                                { model | teamSessions = Dict.insert teamId updatedTeamSessions model.teamSessions }

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

                            -- Include date predictions
                            teamPredictions =
                                teamData.datePredictions

                            -- Include reserve players
                            teamReservePlayers =
                                teamData.reservePlayers
                        in
                        ( updatedModel
                        , sendToFrontend sessionId (TeamLoaded teamData.team teamMatches teamMembers teamAvailability teamPredictions teamReservePlayers)
                        )

                    else
                        ( model
                        , sendToFrontend sessionId (AccessCodeRequired teamId)
                        )

                Nothing ->
                    ( model
                    , sendToFrontend clientId TeamNotFound
                    )

        CreateMatchRequest teamId matchPayload accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            matchId =
                                generateMatchId model.nextId

                            season =
                                getCurrentSeason (getSeasonYearFromStartUtc matchPayload.startUtc)

                            seasonHalf =
                                if getSeasonHalfFromStartUtc matchPayload.startUtc == "Hinrunde" then
                                    Hinrunde

                                else
                                    Rückrunde

                            newMatch =
                                { id = matchId
                                , opponent = matchPayload.opponent
                                , startUtc = matchPayload.startUtc
                                , isHome = matchPayload.isHome
                                , venue = matchPayload.venue
                                , season = season
                                , seasonHalf = seasonHalf
                                , matchday = 1 -- TODO: Calculate proper matchday
                                , originalStartUtc = Nothing
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
                        , sendToTeamSessions teamId (MatchCreated newMatch) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model
                    , sendToFrontend sessionId TeamNotFound
                    )

        CreateMemberRequest teamId memberForm accessCode ->
            case Dict.get teamId model.teams of
                Just teamCode ->
                    -- Validate access code
                    if accessCode == teamCode.team.accessCode then
                        let
                            memberId =
                                generateMemberId model.nextId

                            newMember =
                                { id = memberId
                                , teamId = teamId
                                , name = memberForm.name
                                }

                            updatedTeamData =
                                { teamCode | members = Dict.insert memberId newMember teamCode.members }

                            updatedModel =
                                { model
                                    | teams = Dict.insert teamId updatedTeamData model.teams
                                    , nextId = model.nextId + 1
                                }
                        in
                        ( updatedModel
                        , sendToTeamSessions teamId (MemberCreated newMember) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model
                    , sendToFrontend sessionId TeamNotFound
                    )

        UpdateAvailabilityRequest memberId matchId availability accessCode ->
            -- Find the team that contains this match
            let
                findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                findTeamWithMatch ( _, teamData ) =
                    teamData.seasons
                        |> Dict.values
                        |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                        |> List.any (\match -> match.id == matchId)

                maybeTeamData =
                    model.teams
                        |> Dict.toList
                        |> List.filter findTeamWithMatch
                        |> List.head
            in
            case maybeTeamData of
                Just ( teamId, teamData ) ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Get current member's availability dict or create empty one
                            memberAvailability =
                                Dict.get memberId teamData.availability
                                    |> Maybe.withDefault Dict.empty

                            -- Update the specific match availability
                            updatedMemberAvailability =
                                Dict.insert matchId availability memberAvailability

                            -- Update the overall availability structure
                            updatedAvailability =
                                Dict.insert memberId updatedMemberAvailability teamData.availability

                            updatedTeamData =
                                { teamData | availability = updatedAvailability }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }

                            -- Create response record for frontend
                            newAvailabilityRecord =
                                { memberId = memberId
                                , matchId = matchId
                                , availability = availability
                                }
                        in
                        ( updatedModel
                        , sendToTeamSessions teamId (AvailabilityUpdated newAvailabilityRecord) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeMatchDateRequest matchId newStartUtc teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Remove all availability records for this match
                            clearMatchAvailability availability =
                                Dict.map
                                    (\memberId memberAvailability ->
                                        Dict.remove matchId memberAvailability
                                    )
                                    availability

                            updatedTeamData =
                                { teamData
                                    | seasons = updateMatchInSeasons matchId (\match -> { match | startUtc = newStartUtc }) teamData.seasons
                                    , availability = clearMatchAvailability teamData.availability
                                }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }
                        in
                        ( updatedModel
                        , sendToTeamSessions teamId (MatchDateChanged matchId newStartUtc) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SubmitAccessCode teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Track this session as viewing this team
                            currentTeamSessions =
                                Dict.get teamId model.teamSessions
                                    |> Maybe.withDefault []

                            updatedTeamSessions =
                                if List.member sessionId currentTeamSessions then
                                    currentTeamSessions

                                else
                                    sessionId :: currentTeamSessions

                            updatedModel =
                                { model | teamSessions = Dict.insert teamId updatedTeamSessions model.teamSessions }

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

                            -- Include date predictions
                            teamPredictions =
                                teamData.datePredictions

                            -- Include reserve players
                            teamReservePlayers =
                                teamData.reservePlayers
                        in
                        ( updatedModel
                        , sendToFrontend sessionId (TeamLoaded teamData.team teamMatches teamMembers teamAvailability teamPredictions teamReservePlayers)
                        )

                    else
                        ( model
                        , sendToFrontend sessionId (AccessCodeRequired teamId)
                        )

                Nothing ->
                    ( model
                    , sendToFrontend sessionId TeamNotFound
                    )

        AddDatePredictionRequest matchId predictedDate memberId accessCode ->
            -- Find the team that contains this match
            let
                findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                findTeamWithMatch ( _, teamData ) =
                    teamData.seasons
                        |> Dict.values
                        |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                        |> List.any (\match -> match.id == matchId)
            in
            case model.teams |> Dict.toList |> List.filter findTeamWithMatch |> List.head of
                Just ( teamId, teamData ) ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Get the match to check if we need to set originalStartUtc
                            allMatches =
                                teamData.seasons
                                    |> Dict.values
                                    |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)

                            currentMatch =
                                allMatches
                                    |> List.filter (\match -> match.id == matchId)
                                    |> List.head

                            -- Set originalStartUtc if not already set
                            ( updatedMatch, shouldSetOriginalStartUtc ) =
                                case currentMatch of
                                    Just match ->
                                        if match.originalStartUtc == Nothing then
                                            ( { match | originalStartUtc = Just match.startUtc }, True )

                                        else
                                            ( match, False )

                                    Nothing ->
                                        -- Match not found, can't update
                                        ( Maybe.withDefault
                                            { id = matchId
                                            , opponent = ""
                                            , startUtc = ""
                                            , isHome = False
                                            , venue = ""
                                            , season = ""
                                            , seasonHalf = Hinrunde
                                            , matchday = 0
                                            , originalStartUtc = Nothing
                                            }
                                            currentMatch
                                        , False
                                        )

                            -- Get predictions for this match
                            matchPredictions =
                                teamData.datePredictions
                                    |> Dict.get matchId
                                    |> Maybe.withDefault Dict.empty

                            -- Get predictions for this specific date (if any exist)
                            datePredictions =
                                matchPredictions
                                    |> Dict.get predictedDate
                                    |> Maybe.withDefault Dict.empty

                            -- Check if this member already has a prediction for this date
                            existingMemberPrediction =
                                Dict.get memberId datePredictions

                            -- Create or update this member's prediction for this date
                            newPrediction =
                                case existingMemberPrediction of
                                    Just existing ->
                                        -- Member already predicted this date, keep existing availability
                                        existing

                                    Nothing ->
                                        -- New prediction for this member/date
                                        { predictedDate = predictedDate
                                        , memberId = memberId
                                        , availability = Maybe
                                        }

                            -- Update the date's predictions dict with this member's prediction
                            updatedDatePredictions =
                                Dict.insert memberId newPrediction datePredictions

                            -- Update match predictions dict
                            updatedMatchPredictions =
                                Dict.insert predictedDate updatedDatePredictions matchPredictions

                            updatedPredictions =
                                Dict.insert matchId updatedMatchPredictions teamData.datePredictions

                            -- Update match in seasons if needed
                            updatedSeasons =
                                if shouldSetOriginalStartUtc then
                                    updateMatchInSeasons matchId (\_ -> updatedMatch) teamData.seasons

                                else
                                    teamData.seasons

                            updatedTeamData =
                                { teamData
                                    | datePredictions = updatedPredictions
                                    , seasons = updatedSeasons
                                }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }

                            -- Send original date set message if we set it
                            originalStartUtcCmd =
                                case updatedMatch.originalStartUtc of
                                    Just startUtc ->
                                        sendToTeamSessions teamId (MatchOriginalDateSet matchId startUtc) updatedModel

                                    Nothing ->
                                        Cmd.none
                        in
                        ( updatedModel
                        , Cmd.batch
                            [ sendToTeamSessions teamId (DatePredictionAdded newPrediction matchId) updatedModel
                            , originalStartUtcCmd
                            ]
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdatePredictionAvailabilityRequest matchId predictedDate memberId availability accessCode ->
            -- Find the team that contains this match
            let
                findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                findTeamWithMatch ( _, teamData ) =
                    teamData.seasons
                        |> Dict.values
                        |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                        |> List.any (\match -> match.id == matchId)
            in
            case model.teams |> Dict.toList |> List.filter findTeamWithMatch |> List.head of
                Just ( teamId, teamData ) ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Get predictions for this match
                            matchPredictions =
                                teamData.datePredictions
                                    |> Dict.get matchId
                                    |> Maybe.withDefault Dict.empty

                            -- Get predictions for this specific date
                            datePredictions =
                                matchPredictions
                                    |> Dict.get predictedDate
                                    |> Maybe.withDefault Dict.empty

                            -- Get current prediction for this member/date
                            currentPrediction =
                                Dict.get memberId datePredictions

                            -- Update prediction availability
                            updatedPrediction =
                                case currentPrediction of
                                    Just existing ->
                                        { existing | availability = availability }

                                    Nothing ->
                                        { predictedDate = predictedDate
                                        , memberId = memberId
                                        , availability = availability
                                        }

                            -- Update the date's predictions dict
                            updatedDatePredictions =
                                Dict.insert memberId updatedPrediction datePredictions

                            -- Update match predictions dict
                            updatedMatchPredictions =
                                Dict.insert predictedDate updatedDatePredictions matchPredictions

                            updatedPredictions =
                                Dict.insert matchId updatedMatchPredictions teamData.datePredictions

                            updatedTeamData =
                                { teamData | datePredictions = updatedPredictions }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }
                        in
                        ( updatedModel
                        , sendToTeamSessions teamId (DatePredictionUpdated updatedPrediction matchId) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveDatePredictionRequest matchId memberId accessCode ->
            -- Find the team that contains this match
            let
                findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                findTeamWithMatch ( _, teamData ) =
                    teamData.seasons
                        |> Dict.values
                        |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                        |> List.any (\match -> match.id == matchId)
            in
            case model.teams |> Dict.toList |> List.filter findTeamWithMatch |> List.head of
                Just ( teamId, teamData ) ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Remove all predictions for this member/match combination
                            -- (remove member from all date predictions)
                            matchPredictions =
                                teamData.datePredictions
                                    |> Dict.get matchId
                                    |> Maybe.withDefault Dict.empty

                            -- Remove member from all date predictions
                            updatedMatchPredictions =
                                matchPredictions
                                    |> Dict.map (\date datePredictions -> Dict.remove memberId datePredictions)
                                    |> Dict.filter (\_ datePredictions -> not (Dict.isEmpty datePredictions))

                            updatedPredictions =
                                if Dict.isEmpty updatedMatchPredictions then
                                    Dict.remove matchId teamData.datePredictions

                                else
                                    Dict.insert matchId updatedMatchPredictions teamData.datePredictions

                            updatedTeamData =
                                { teamData | datePredictions = updatedPredictions }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }
                        in
                        ( updatedModel
                        , sendToTeamSessions teamId (DatePredictionRemoved matchId memberId) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChoosePredictedDateRequest matchId chosenDate newStartUtc teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Get predictions for this match
                            matchPredictions =
                                teamData.datePredictions
                                    |> Dict.get matchId
                                    |> Maybe.withDefault Dict.empty

                            -- Get predictions for the chosen date
                            predictionsForChosenDate =
                                matchPredictions
                                    |> Dict.get chosenDate
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.values

                            -- First, remove all existing availability for this match
                            -- (to clear any old availability from the original date)
                            clearedAvailability =
                                teamData.availability
                                    |> Dict.map (\memberId memberAvailabilities -> Dict.remove matchId memberAvailabilities)
                                    |> Dict.filter (\_ memberAvailabilities -> not (Dict.isEmpty memberAvailabilities))

                            -- Migrate availability from predictions to main availability
                            migrateAvailability currentAvailability =
                                List.foldl
                                    (\prediction acc ->
                                        let
                                            memberAvailability =
                                                Dict.get prediction.memberId acc
                                                    |> Maybe.withDefault Dict.empty

                                            updatedMemberAvailability =
                                                Dict.insert matchId prediction.availability memberAvailability
                                        in
                                        Dict.insert prediction.memberId updatedMemberAvailability acc
                                    )
                                    currentAvailability
                                    predictionsForChosenDate

                            -- Update match date
                            -- Clear all predictions for this match
                            updatedPredictions =
                                Dict.remove matchId teamData.datePredictions

                            updatedAvailability =
                                migrateAvailability clearedAvailability

                            updatedTeamData =
                                { teamData
                                    | seasons = updateMatchInSeasons matchId (\match -> { match | startUtc = newStartUtc }) teamData.seasons
                                    , datePredictions = updatedPredictions
                                    , availability = updatedAvailability
                                }

                            updatedModel =
                                { model | teams = Dict.insert teamId updatedTeamData model.teams }

                            -- Send availability updates for each migrated record
                            availabilityUpdateCmds =
                                predictionsForChosenDate
                                    |> List.map
                                        (\prediction ->
                                            sendToTeamSessions teamId
                                                (AvailabilityUpdated
                                                    { memberId = prediction.memberId
                                                    , matchId = matchId
                                                    , availability = prediction.availability
                                                    }
                                                )
                                                updatedModel
                                        )
                                    |> Cmd.batch
                        in
                        ( updatedModel
                        , Cmd.batch
                            [ sendToTeamSessions teamId (MatchDateChanged matchId newStartUtc) updatedModel
                            , sendToTeamSessions teamId (PredictionsCleared matchId) updatedModel
                            , availabilityUpdateCmds
                            ]
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        AddReservePlayerRequest matchId reservePlayerName teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Find the team that contains this match
                            findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                            findTeamWithMatch ( _, td ) =
                                td.seasons
                                    |> Dict.values
                                    |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                                    |> List.any (\match -> match.id == matchId)

                            currentTeamData =
                                if findTeamWithMatch ( teamId, teamData ) then
                                    teamData

                                else
                                    -- Match not found in this team, find the correct team
                                    model.teams
                                        |> Dict.toList
                                        |> List.filter findTeamWithMatch
                                        |> List.head
                                        |> Maybe.map Tuple.second
                                        |> Maybe.withDefault teamData

                            -- Get current reserve players for this match
                            currentReservePlayers =
                                currentTeamData.reservePlayers
                                    |> Dict.get matchId
                                    |> Maybe.withDefault []

                            -- Add reserve player if not already present
                            updatedReservePlayers =
                                if List.member reservePlayerName currentReservePlayers then
                                    currentReservePlayers

                                else
                                    reservePlayerName :: currentReservePlayers

                            -- Update reserve players dict
                            updatedReservePlayersDict =
                                Dict.insert matchId updatedReservePlayers currentTeamData.reservePlayers

                            -- Find the correct team ID
                            correctTeamId =
                                if findTeamWithMatch ( teamId, teamData ) then
                                    teamId

                                else
                                    model.teams
                                        |> Dict.toList
                                        |> List.filter findTeamWithMatch
                                        |> List.head
                                        |> Maybe.map Tuple.first
                                        |> Maybe.withDefault teamId

                            updatedTeamData =
                                { currentTeamData | reservePlayers = updatedReservePlayersDict }

                            updatedModel =
                                { model | teams = Dict.insert correctTeamId updatedTeamData model.teams }
                        in
                        ( updatedModel
                        , sendToTeamSessions correctTeamId (ReservePlayerAdded matchId reservePlayerName correctTeamId) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveReservePlayerRequest matchId reservePlayerName teamId accessCode ->
            case Dict.get teamId model.teams of
                Just teamData ->
                    -- Validate access code
                    if accessCode == teamData.team.accessCode then
                        let
                            -- Find the team that contains this match
                            findTeamWithMatch : ( TeamId, TeamData ) -> Bool
                            findTeamWithMatch ( _, td ) =
                                td.seasons
                                    |> Dict.values
                                    |> List.concatMap (\seasonData -> seasonData.hinrunde ++ seasonData.rückrunde)
                                    |> List.any (\match -> match.id == matchId)

                            currentTeamData =
                                if findTeamWithMatch ( teamId, teamData ) then
                                    teamData

                                else
                                    -- Match not found in this team, find the correct team
                                    model.teams
                                        |> Dict.toList
                                        |> List.filter findTeamWithMatch
                                        |> List.head
                                        |> Maybe.map Tuple.second
                                        |> Maybe.withDefault teamData

                            -- Get current reserve players for this match
                            currentReservePlayers =
                                currentTeamData.reservePlayers
                                    |> Dict.get matchId
                                    |> Maybe.withDefault []

                            -- Remove reserve player
                            updatedReservePlayers =
                                List.filter (\name -> name /= reservePlayerName) currentReservePlayers

                            -- Update reserve players dict
                            updatedReservePlayersDict =
                                if List.isEmpty updatedReservePlayers then
                                    Dict.remove matchId currentTeamData.reservePlayers

                                else
                                    Dict.insert matchId updatedReservePlayers currentTeamData.reservePlayers

                            -- Find the correct team ID
                            correctTeamId =
                                if findTeamWithMatch ( teamId, teamData ) then
                                    teamId

                                else
                                    model.teams
                                        |> Dict.toList
                                        |> List.filter findTeamWithMatch
                                        |> List.head
                                        |> Maybe.map Tuple.first
                                        |> Maybe.withDefault teamId

                            updatedTeamData =
                                { currentTeamData | reservePlayers = updatedReservePlayersDict }

                            updatedModel =
                                { model | teams = Dict.insert correctTeamId updatedTeamData model.teams }
                        in
                        ( updatedModel
                        , sendToTeamSessions correctTeamId (ReservePlayerRemoved matchId reservePlayerName correctTeamId) updatedModel
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOpToBackend ->
            ( model, Cmd.none )
