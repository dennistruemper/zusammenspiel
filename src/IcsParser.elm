module IcsParser exposing (parseIcs, IcsEvent, parseIcsEvents, parseIcsToMatches, ParsedMatch)

import Dict exposing (Dict)
import String


type alias IcsEvent =
    { summary : String
    , dtStart : String -- ISO format: "20250922T180000Z"
    , dtEnd : String
    , location : String
    , description : String
    }


type alias ParsedMatch =
    { opponent : String
    , date : String -- German format: "22.09.2025"
    , time : String -- German format: "20:00"
    , venue : String
    , isHome : Bool
    }


-- Parse ICS file content and extract VEVENT entries
parseIcs : String -> List IcsEvent
parseIcs icsContent =
    let
        lines =
            String.split "\n" icsContent
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        events =
            extractEvents lines []
    in
    List.reverse events


extractEvents : List String -> List IcsEvent -> List IcsEvent
extractEvents lines acc =
    case lines of
        [] ->
            acc

        "BEGIN:VEVENT" :: rest ->
            let
                ( event, remaining ) =
                    parseEvent rest
            in
            case event of
                Just e ->
                    extractEvents remaining (e :: acc)

                Nothing ->
                    extractEvents remaining acc

        _ :: rest ->
            extractEvents rest acc


parseEvent : List String -> ( Maybe IcsEvent, List String )
parseEvent lines =
    parseEventHelper lines
        { summary = ""
        , dtStart = ""
        , dtEnd = ""
        , location = ""
        , description = ""
        }
        []


parseEventHelper : List String -> IcsEvent -> List String -> ( Maybe IcsEvent, List String )
parseEventHelper lines event acc =
    case lines of
        [] ->
            ( Nothing, [] )

        "END:VEVENT" :: rest ->
            if String.isEmpty event.dtStart then
                ( Nothing, rest )

            else
                ( Just event, rest )

        line :: rest ->
            if String.startsWith "SUMMARY:" line then
                parseEventHelper rest { event | summary = String.dropLeft 8 line } acc

            else if String.startsWith "DTSTART:" line then
                parseEventHelper rest { event | dtStart = String.dropLeft 8 line } acc

            else if String.startsWith "DTEND:" line then
                parseEventHelper rest { event | dtEnd = String.dropLeft 6 line } acc

            else if String.startsWith "LOCATION:" line then
                parseEventHelper rest { event | location = String.dropLeft 9 line } acc

            else if String.startsWith "DESCRIPTION:" line then
                parseEventHelper rest { event | description = String.dropLeft 12 line } acc

            else
                parseEventHelper rest event acc


-- Parse ICS events and convert to list of events (for debugging/testing)
parseIcsEvents : String -> List IcsEvent
parseIcsEvents =
    parseIcs


-- Convert ISO date/time string (20250922T180000Z) to German date format (22.09.2025)
isoDateTimeToGermanDate : String -> String
isoDateTimeToGermanDate isoDateTime =
    if String.length isoDateTime >= 8 then
        let
            year =
                String.slice 0 4 isoDateTime

            month =
                String.slice 4 6 isoDateTime

            day =
                String.slice 6 8 isoDateTime
        in
        day ++ "." ++ month ++ "." ++ year

    else
        ""


-- Convert ISO date/time string (20250922T180000Z) to German time format (20:00)
isoDateTimeToGermanTime : String -> String
isoDateTimeToGermanTime isoDateTime =
    if String.length isoDateTime >= 13 then
        let
            hour =
                String.slice 9 11 isoDateTime

            minute =
                String.slice 11 13 isoDateTime
        in
        hour ++ ":" ++ minute

    else
        ""


-- Extract team names from SUMMARY field
-- Format: "Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)"
-- Returns: (homeTeam, awayTeam) or Nothing if format is invalid
extractTeams : String -> Maybe ( String, String )
extractTeams summary =
    let
        -- Remove league info in parentheses
        summaryWithoutLeague =
            case String.split "(" summary of
                mainPart :: _ ->
                    String.trim mainPart

                _ ->
                    summary

        -- Split by " - " to get teams
        teams =
            String.split " - " summaryWithoutLeague
    in
    case teams of
        [ homeTeam, awayTeam ] ->
            Just ( String.trim homeTeam, String.trim awayTeam )

        _ ->
            Nothing


-- Count how many times each team appears as home team
countHomeTeamOccurrences : List IcsEvent -> Dict String Int
countHomeTeamOccurrences events =
    events
        |> List.filterMap (\event -> extractTeams event.summary)
        |> List.map Tuple.first
        |> List.foldl
            (\teamName dict ->
                Dict.update teamName
                    (\maybeCount ->
                        case maybeCount of
                            Just count ->
                                Just (count + 1)

                            Nothing ->
                                Just 1
                    )
                    dict
            )
            Dict.empty


-- Find the team that appears most often as home team
findMostFrequentHomeTeam : Dict String Int -> Maybe String
findMostFrequentHomeTeam teamCounts =
    Dict.toList teamCounts
        |> List.foldl
            (\( teamName, count ) maybeBest ->
                case maybeBest of
                    Just ( bestTeam, bestCount ) ->
                        if count > bestCount then
                            Just ( teamName, count )

                        else
                            Just ( bestTeam, bestCount )

                    Nothing ->
                        Just ( teamName, count )
            )
            Nothing
        |> Maybe.map Tuple.first


-- Extract opponent name from SUMMARY field
-- Uses the most frequent home team to determine home/away
extractOpponent : String -> String -> ( String, Bool )
extractOpponent summary mostFrequentHomeTeam =
    case extractTeams summary of
        Just ( homeTeam, awayTeam ) ->
            if homeTeam == mostFrequentHomeTeam then
                ( awayTeam, True )

            else
                ( homeTeam, False )

        Nothing ->
            ( summary, True )


-- Convert ICS event to ParsedMatch
icsEventToMatch : String -> IcsEvent -> ParsedMatch
icsEventToMatch mostFrequentHomeTeam event =
    let
        ( opponent, isHome ) =
            extractOpponent event.summary mostFrequentHomeTeam

        date =
            isoDateTimeToGermanDate event.dtStart

        time =
            isoDateTimeToGermanTime event.dtStart
    in
    { opponent = opponent
    , date = date
    , time = time
    , venue = event.location
    , isHome = isHome
    }


-- Parse ICS and convert to matches
-- Determines home team by counting which team appears most often as home
parseIcsToMatches : String -> String -> List ParsedMatch
parseIcsToMatches teamName icsContent =
    let
        events =
            parseIcs icsContent

        -- Count home team occurrences
        homeTeamCounts =
            countHomeTeamOccurrences events

        -- Find the most frequent home team
        mostFrequentHomeTeam =
            findMostFrequentHomeTeam homeTeamCounts
                |> Maybe.withDefault teamName -- Fallback to provided teamName if no matches found
    in
    events
        |> List.map (icsEventToMatch mostFrequentHomeTeam)

