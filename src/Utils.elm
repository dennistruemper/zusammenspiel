module Utils exposing (createSlug, createTeamUrl, extractAccessCodeFromUrl, extractTeamIdFromUrl, generateMatchId, generateMemberId, generateRandomAccessCode, generateRandomTeamId, getAllMatches, getCurrentSeason, getSeasonHalf, isoToGermanDate, separatePastAndFutureMatches, sortMatchesByDate)

import Char
import Dict exposing (Dict)
import Random
import String
import Time



-- Generate a random team ID using Random.Seed


generateRandomTeamId : Random.Seed -> ( String, Random.Seed )
generateRandomTeamId seed =
    let
        -- Generate random characters for a more secure ID
        charGenerator =
            Random.map
                (\n ->
                    let
                        chars =
                            "abcdefghijklmnopqrstuvwxyz0123456789"

                        index =
                            remainderBy (String.length chars) n
                    in
                    String.slice index (index + 1) chars
                )
                (Random.int 0 999999)

        -- Generate 8 random characters
        randomStringGenerator =
            Random.list 8 charGenerator
                |> Random.map (String.join "")

        ( randomString, newSeed ) =
            Random.step randomStringGenerator seed
    in
    ( randomString, newSeed )



-- Generate a match ID (simpler than team ID)


generateMatchId : Int -> String
generateMatchId counter =
    "match-" ++ String.fromInt counter



-- Generate a member ID


generateMemberId : Int -> String
generateMemberId counter =
    "member-" ++ String.fromInt counter



-- Create a URL-friendly slug from team name


createSlug : String -> String
createSlug name =
    name
        |> String.toLower
        |> String.replace " " "-"
        |> String.replace "." ""
        |> String.replace "," ""
        |> String.replace "'" ""
        |> String.replace "\"" ""
        |> String.replace "!" ""
        |> String.replace "?" ""
        |> String.replace "&" "and"
        |> String.filter (\char -> Char.isAlphaNum char || char == '-')
        |> String.split "-"
        |> List.filter (not << String.isEmpty)
        |> String.join "-"



-- Combine slug and ID for team URL


createTeamUrl : String -> String -> String -> String
createTeamUrl slug teamId accessCode =
    "/team/" ++ slug ++ "-" ++ teamId ++ "?code=" ++ accessCode



-- Extract team ID from URL
-- URL format: /team/slug-teamid where teamid is 8 random chars


extractTeamIdFromUrl : String -> Maybe String
extractTeamIdFromUrl url =
    case String.split "/" url of
        [ "", "team", teamSlugId ] ->
            -- Split by "-" and take the last part (the 8-char team ID)
            case String.split "-" teamSlugId |> List.reverse of
                teamId :: _ ->
                    -- Verify it looks like our 8-char random ID (alphanumeric)
                    if String.length teamId == 8 then
                        Just teamId

                    else
                        Nothing

                [] ->
                    Nothing

        _ ->
            Nothing



-- Extract access code from URL query parameter
-- URL format: /team/slug-teamid?code=1234


extractAccessCodeFromUrl : String -> Maybe String
extractAccessCodeFromUrl url =
    let
        queryStart =
            String.indexes "?code=" url
                |> List.head

        codeStart =
            Maybe.map (\start -> start + 6) queryStart
    in
    case codeStart of
        Just start ->
            let
                codeEnd =
                    String.indexes "&" (String.dropLeft start url)
                        |> List.head
                        |> Maybe.withDefault (String.length url - start)

                accessCode =
                    String.dropLeft start url
                        |> String.left codeEnd
            in
            if String.length accessCode == 4 then
                Just accessCode

            else
                Nothing

        Nothing ->
            Nothing



-- MATCH UTILITIES
-- Get current season based on month (e.g., "2024/25")


getCurrentSeason : Int -> String
getCurrentSeason currentYear =
    -- Assuming season starts in August (month 8)
    -- and ends in July the following year
    let
        nextYear =
            currentYear + 1

        nextYearShort =
            remainderBy 100 nextYear

        nextYearStr =
            String.fromInt nextYearShort |> String.padLeft 2 '0'
    in
    String.fromInt currentYear ++ "/" ++ nextYearStr



-- Determine season half based on date


getSeasonHalf :
    String
    -> String -- SeasonHalf would be imported, but avoiding circular import
getSeasonHalf dateStr =
    case String.split "." dateStr of
        [ day, month, year ] ->
            case String.toInt month of
                Just monthInt ->
                    if monthInt >= 8 || monthInt <= 1 then
                        "Hinrunde"

                    else
                        "Rückrunde"

                Nothing ->
                    "Hinrunde"

        _ ->
            "Hinrunde"



-- Sort matches by date (newest first for past, earliest first for future)


sortMatchesByDate : List { a | date : String } -> List { a | date : String }
sortMatchesByDate matches =
    let
        -- Convert German date format (dd.mm.yyyy) to sortable format (yyyy-mm-dd)
        convertToSortable : String -> String
        convertToSortable dateStr =
            case String.split "." dateStr of
                [ day, month, year ] ->
                    year ++ "-" ++ String.padLeft 2 '0' month ++ "-" ++ String.padLeft 2 '0' day

                _ ->
                    dateStr
    in
    List.sortWith (\a b -> compare (convertToSortable b.date) (convertToSortable a.date)) matches



-- Separate past and future matches


separatePastAndFutureMatches : List { a | date : String } -> ( List { a | date : String }, List { a | date : String } )
separatePastAndFutureMatches matches =
    let
        -- For now, use a hardcoded date. In a real app, you'd get this from Time.now
        -- This separates matches into past (before today) and future (today and after)
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
    in
    List.partition (\match -> convertToSortable match.date < convertToSortable today) matches



-- Get all matches from SeasonData as a flat list


getAllMatches : { hinrunde : List a, rückrunde : List a } -> List a
getAllMatches seasonData =
    seasonData.hinrunde ++ seasonData.rückrunde



-- Convert ISO date format (yyyy-mm-dd) to German format (dd.mm.yyyy)


isoToGermanDate : String -> String
isoToGermanDate isoDate =
    case String.split "-" isoDate of
        [ year, month, day ] ->
            day ++ "." ++ month ++ "." ++ year

        _ ->
            isoDate


generateRandomAccessCode : Random.Seed -> ( String, Random.Seed )
generateRandomAccessCode seed =
    let
        -- Generate random digits for access code
        digitGenerator =
            Random.map
                (\n ->
                    let
                        digits =
                            "0123456789"

                        index =
                            remainderBy (String.length digits) n
                    in
                    String.slice index (index + 1) digits
                )
                (Random.int 0 999999)

        -- Generate 4 random digits
        randomCodeGenerator =
            Random.list 4 digitGenerator
                |> Random.map (String.join "")

        ( randomCode, newSeed ) =
            Random.step randomCodeGenerator seed
    in
    ( randomCode, newSeed )
