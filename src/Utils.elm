module Utils exposing (createSlug, createTeamUrl, displayLocalTime, extractAccessCodeFromUrl, extractTeamIdFromUrl, formatGermanDateWithWeekday, formatIsoDateWithWeekday, formatLocalDateTimeDisplay, generateMatchId, generateMemberId, generateRandomAccessCode, generateRandomTeamId, germanDateTimeToStartUtc, getAllMatches, getCurrentSeason, getSeasonHalfFromStartUtc, getSeasonYearFromStartUtc, isoToGermanDate, separatePastAndFutureMatches, sortMatchesByStartUtc, startUtcMonth, startUtcYear)

import Char
import Dict exposing (Dict)
import LocalStorage
import Random
import String
import Time



-- Display helpers for UTC timestamps


displayLocalTime : Dict String { date : String, time : String } -> String -> { date : String, time : String }
displayLocalTime cache startUtc =
    Dict.get startUtc cache
        |> Maybe.withDefault { date = "?", time = "?" }



-- Parse ICS UTC timestamp components


startUtcYear : String -> Maybe Int
startUtcYear startUtc =
    if String.length startUtc >= 4 then
        String.toInt (String.left 4 startUtc)

    else
        Nothing


startUtcMonth : String -> Maybe Int
startUtcMonth startUtc =
    if String.length startUtc >= 6 then
        String.toInt (String.slice 4 6 startUtc)

    else
        Nothing


getSeasonYearFromStartUtc : String -> Int
getSeasonYearFromStartUtc startUtc =
    case ( startUtcYear startUtc, startUtcMonth startUtc ) of
        ( Just year, Just month ) ->
            if month >= 8 then
                year

            else
                year - 1

        _ ->
            2024


getSeasonHalfFromStartUtc : String -> String
getSeasonHalfFromStartUtc startUtc =
    case startUtcMonth startUtc of
        Just monthInt ->
            if monthInt >= 8 || monthInt <= 1 then
                "Hinrunde"

            else
                "Rückrunde"

        Nothing ->
            "Hinrunde"



-- Convert legacy German date/time (Europe/Berlin wall clock) to ICS UTC format.
-- Uses a simple DST heuristic for migration of existing data.


germanDateTimeToStartUtc : String -> String -> String
germanDateTimeToStartUtc germanDate time =
    case ( String.split "." germanDate, String.split ":" time ) of
        ( [ day, month, year ], [ hour, minute ] ) ->
            let
                pad2 str =
                    if String.length str == 1 then
                        "0" ++ str

                    else
                        str

                dayInt =
                    String.toInt day

                monthInt =
                    String.toInt month

                yearInt =
                    String.toInt year

                hourInt =
                    String.toInt hour |> Maybe.withDefault 0

                minuteInt =
                    String.toInt minute |> Maybe.withDefault 0

                berlinOffsetMinutes =
                    case monthInt of
                        Just m ->
                            if m >= 4 && m <= 10 then
                                120

                            else
                                60

                        Nothing ->
                            60

                normalized =
                    case ( dayInt, monthInt, yearInt ) of
                        ( Just d, Just mo, Just y ) ->
                            normalizeDateTime d mo y ((hourInt * 60 + minuteInt) - berlinOffsetMinutes)

                        _ ->
                            { day = 1, month = 1, year = 2024, hour = 0, minute = 0 }
            in
            String.fromInt normalized.year
                ++ pad2 (String.fromInt normalized.month)
                ++ pad2 (String.fromInt normalized.day)
                ++ "T"
                ++ pad2 (String.fromInt normalized.hour)
                ++ pad2 (String.fromInt normalized.minute)
                ++ "00Z"

        _ ->
            ""


normalizeDateTime : Int -> Int -> Int -> Int -> { day : Int, month : Int, year : Int, hour : Int, minute : Int }
normalizeDateTime day month year totalMinutes =
    let
        daysInMonth m y =
            if List.member m [ 4, 6, 9, 11 ] then
                30

            else if m == 2 then
                if modBy 4 y == 0 && (modBy 100 y /= 0 || modBy 400 y == 0) then
                    29

                else
                    28

            else
                31

        rec d m y mins =
            if mins < 0 then
                let
                    prevMonth =
                        if m == 1 then
                            12

                        else
                            m - 1

                    prevYear =
                        if m == 1 then
                            y - 1

                        else
                            y

                    prevDays =
                        daysInMonth prevMonth prevYear
                in
                rec (d - 1) prevMonth prevYear (mins + 24 * 60)

            else if mins >= 24 * 60 then
                let
                    daysInCurrentMonth =
                        daysInMonth m y

                    nextDay =
                        d + 1

                    nextMonth =
                        if nextDay > daysInCurrentMonth then
                            if m == 12 then
                                1

                            else
                                m + 1

                        else
                            m

                    nextYear =
                        if nextDay > daysInCurrentMonth then
                            if m == 12 then
                                y + 1

                            else
                                y

                        else
                            y

                    adjustedDay =
                        if nextDay > daysInCurrentMonth then
                            1

                        else
                            nextDay
                in
                rec adjustedDay nextMonth nextYear (mins - 24 * 60)

            else
                { day = d, month = m, year = y, hour = mins // 60, minute = modBy 60 mins }
    in
    rec day month year totalMinutes



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



-- Sort matches by start time (earliest UTC first)


sortMatchesByStartUtc : List { a | startUtc : String } -> List { a | startUtc : String }
sortMatchesByStartUtc matches =
    List.sortWith (\a b -> compare a.startUtc b.startUtc) matches



-- Separate past and future matches using converted local dates


separatePastAndFutureMatches : String -> Dict String { date : String, time : String } -> List { a | startUtc : String } -> ( List { a | startUtc : String }, List { a | startUtc : String } )
separatePastAndFutureMatches today cache matches =
    let
        convertToSortable : String -> String
        convertToSortable dateStr =
            case String.split "." dateStr of
                [ day, month, year ] ->
                    year ++ "-" ++ String.padLeft 2 '0' month ++ "-" ++ String.padLeft 2 '0' day

                _ ->
                    dateStr

        localDate : { a | startUtc : String } -> String
        localDate match =
            displayLocalTime cache match.startUtc |> .date
    in
    List.partition (\match -> convertToSortable (localDate match) < convertToSortable today) matches



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



-- Weekday helpers for German dates (dd.mm.yyyy)


weekdayNamesGerman : List String
weekdayNamesGerman =
    [ "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So" ]


dayOfWeekMonFirst : Int -> Int -> Int -> Int
dayOfWeekMonFirst day month year =
    let
        adjustedMonth =
            if month < 3 then
                month + 12

            else
                month

        adjustedYear =
            if month < 3 then
                year - 1

            else
                year

        k =
            adjustedYear // 100

        j =
            modBy 100 adjustedYear

        h =
            modBy 7 (day + (13 * (adjustedMonth + 1) // 5) + j + (j // 4) + (k // 4) - (2 * k))
    in
    modBy 7 (h + 5)


germanWeekdayShort : String -> String
germanWeekdayShort dateStr =
    case String.split "." dateStr of
        [ dayStr, monthStr, yearStr ] ->
            case ( String.toInt dayStr, String.toInt monthStr, String.toInt yearStr ) of
                ( Just day, Just month, Just year ) ->
                    weekdayNamesGerman
                        |> List.drop (dayOfWeekMonFirst day month year)
                        |> List.head
                        |> Maybe.withDefault ""

                _ ->
                    ""

        _ ->
            ""


formatGermanDateWithWeekday : String -> String
formatGermanDateWithWeekday dateStr =
    let
        weekday =
            germanWeekdayShort dateStr
    in
    if String.isEmpty weekday then
        dateStr

    else
        weekday ++ ", " ++ dateStr


formatIsoDateWithWeekday : String -> String
formatIsoDateWithWeekday isoDate =
    isoDate
        |> isoToGermanDate
        |> formatGermanDateWithWeekday


formatLocalDateTimeDisplay : { date : String, time : String } -> String
formatLocalDateTimeDisplay local =
    formatGermanDateWithWeekday local.date ++ " um " ++ local.time


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
