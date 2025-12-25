module IcsParserTests exposing (suite)

import Expect
import IcsParser exposing (IcsEvent, ParsedMatch, parseIcs, parseIcsToMatches)
import Test exposing (Test, describe, test)



-- Sample ICS content (2 events for basic tests)


sampleIcsContent : String
sampleIcsContent =
    """BEGIN:VCALENDAR
VERSION:2.0
PRODID:https://Rendsburg-Eckernfoerde.tischtennislive.de
METHOD:PUBLISH
X-WR-CALNAME:TischtennisLive-Spieltermine
X-WR-TIMEZONE:Europe/Berlin
BEGIN:VEVENT
UID: W4S_96_993032
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20250922T180000Z
DTEND:20250922T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993041
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:SV Fockbek 3 - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:SV Fockbek 3 - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Mehrzweckhalle Schulzentrum, Friedhofsweg, 24787, Fockbek
CLASS:PUBLIC
DTSTART:20251009T174500Z
DTEND:20251009T201500Z
DTSTAMP:20251225T114257Z
END:VEVENT
END:VCALENDAR"""



-- Full ICS content from test-output-1.ics (17 events)


fullIcsContent : String
fullIcsContent =
    """BEGIN:VCALENDAR
VERSION:2.0
PRODID:https://Rendsburg-Eckernfoerde.tischtennislive.de
METHOD:PUBLISH
X-WR-CALNAME:TischtennisLive-Spieltermine
X-WR-TIMEZONE:Europe/Berlin
BEGIN:VEVENT
UID: W4S_96_993032
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20250922T180000Z
DTEND:20250922T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993037
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Osterrönfelder TSV (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Osterrönfelder TSV (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20250929T180000Z
DTEND:20250929T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993041
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:SV Fockbek 3 - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:SV Fockbek 3 - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Mehrzweckhalle Schulzentrum, Friedhofsweg, 24787, Fockbek
CLASS:PUBLIC
DTSTART:20251009T174500Z
DTEND:20251009T201500Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993047
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Brügger SV (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Brügger SV (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20251013T180000Z
DTEND:20251013T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993023
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - TSV Vineta Audorf 2 (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - TSV Vineta Audorf 2 (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20251020T180000Z
DTEND:20251020T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993050
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:TSV Owschlag - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:TSV Owschlag - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Mehrzweckhalle, Sportallee, 24811, Owschlag
CLASS:PUBLIC
DTSTART:20251103T190000Z
DTEND:20251103T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993057
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Osdorfer SV (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Osdorfer SV (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20251110T190000Z
DTEND:20251110T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993059
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:SG Eiderblick - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:SG Eiderblick - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Astrid-Lindgren-Schule - Standort Neue Dorfstraße, Akazienstrasse 17, 24782, Büdelsdorf
CLASS:PUBLIC
DTSTART:20251117T180000Z
DTEND:20251117T203000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993067
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - Barkelsbyer SV (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - Barkelsbyer SV (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20251124T190000Z
DTEND:20251124T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993068
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:TSV Vineta Audorf 2 - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:TSV Vineta Audorf 2 - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Sporthalle neues Sportgelände, Zum Sportplatz 1, 24790, Schacht-Audorf
CLASS:PUBLIC
DTSTART:20260116T190000Z
DTEND:20260116T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993077
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Osterbyer SV 2 - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:Osterbyer SV 2 - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Sporth. Schulstr. 23   Tel. 04351-44540, Schulstraße 23, 24367 , Osterby
CLASS:PUBLIC
DTSTART:20260121T190000Z
DTEND:20260121T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993082
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Osterrönfelder TSV - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:Osterrönfelder TSV - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Sporthalle Osterrönfeld, Fehmarnstr. 2, 24783, Osterrönfeld
CLASS:PUBLIC
DTSTART:20260130T191500Z
DTEND:20260130T214500Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993086
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - SV Fockbek 3 (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - SV Fockbek 3 (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20260202T190000Z
DTEND:20260202T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993092
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Brügger SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:Brügger SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Turnhalle, Dorfstr. 43, 24582, Brügge
CLASS:PUBLIC
DTSTART:20260209T190000Z
DTEND:20260209T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993095
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - TSV Owschlag (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - TSV Owschlag (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20260216T190000Z
DTEND:20260216T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993102
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Osdorfer SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:Osdorfer SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Sporthalle, Zur Schule, 24251, Osdorf
CLASS:PUBLIC
DTSTART:20260227T190000Z
DTEND:20260227T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993104
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Rendsburger TSV 4 - SG Eiderblick (Kreisliga Erwachsene)
DESCRIPTION:Rendsburger TSV 4 - SG Eiderblick (Kreisliga Erwachsene)
LOCATION:Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg
CLASS:PUBLIC
DTSTART:20260302T190000Z
DTEND:20260302T213000Z
DTSTAMP:20251225T114257Z
END:VEVENT
BEGIN:VEVENT
UID: W4S_96_993112
ORGANIZER;CN="Max Mustermann":MAILTO:NoReply@TischtennisLive.de
SUMMARY:Barkelsbyer SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
DESCRIPTION:Barkelsbyer SV - Rendsburger TSV 4 (Kreisliga Erwachsene)
LOCATION:Mehrzweckhalle, Riesebyer Str. 5, 24360, Barkelsby
CLASS:PUBLIC
DTSTART:20260309T191500Z
DTEND:20260309T214500Z
DTSTAMP:20251225T114257Z
END:VEVENT
END:VCALENDAR"""


teamName : String
teamName =
    "Rendsburger TSV 4"


suite : Test
suite =
    describe "IcsParser"
        [ describe "parseIcs"
            [ test "parses ICS content and extracts VEVENT entries" <|
                \_ ->
                    let
                        events =
                            parseIcs sampleIcsContent
                    in
                    Expect.equal (List.length events) 2
            , test "parses first event correctly" <|
                \_ ->
                    let
                        events =
                            parseIcs sampleIcsContent

                        firstEvent =
                            List.head events
                    in
                    case firstEvent of
                        Just event ->
                            Expect.all
                                [ \e -> Expect.equal e.summary "Rendsburger TSV 4 - Osterbyer SV 2 (Kreisliga Erwachsene)"
                                , \e -> Expect.equal e.dtStart "20250922T180000Z"
                                , \e -> Expect.equal e.dtEnd "20250922T203000Z"
                                , \e -> Expect.equal e.location "Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg"
                                ]
                                event

                        Nothing ->
                            Expect.fail "Expected first event to be parsed"
            , test "parses second event correctly (away game)" <|
                \_ ->
                    let
                        events =
                            parseIcs sampleIcsContent

                        secondEvent =
                            events
                                |> List.drop 1
                                |> List.head
                    in
                    case secondEvent of
                        Just event ->
                            Expect.all
                                [ \e -> Expect.equal e.summary "SV Fockbek 3 - Rendsburger TSV 4 (Kreisliga Erwachsene)"
                                , \e -> Expect.equal e.dtStart "20251009T174500Z"
                                , \e -> Expect.equal e.dtEnd "20251009T201500Z"
                                , \e -> Expect.equal e.location "Mehrzweckhalle Schulzentrum, Friedhofsweg, 24787, Fockbek"
                                ]
                                event

                        Nothing ->
                            Expect.fail "Expected second event to be parsed"
            ]
        , describe "parseIcsToMatches"
            [ test "converts ICS events to ParsedMatch list" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName sampleIcsContent
                    in
                    Expect.equal (List.length matches) 2
            , test "parses home game correctly" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName sampleIcsContent

                        firstMatch =
                            List.head matches
                    in
                    case firstMatch of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "Osterbyer SV 2"
                                , \m -> Expect.equal m.date "22.09.2025"
                                , \m -> Expect.equal m.time "18:00"
                                , \m -> Expect.equal m.venue "Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg"
                                , \m -> Expect.equal m.isHome True
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected first match to be parsed"
            , test "parses away game correctly" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName sampleIcsContent

                        secondMatch =
                            matches
                                |> List.drop 1
                                |> List.head
                    in
                    case secondMatch of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "SV Fockbek 3"
                                , \m -> Expect.equal m.date "09.10.2025"
                                , \m -> Expect.equal m.time "17:45"
                                , \m -> Expect.equal m.venue "Mehrzweckhalle Schulzentrum, Friedhofsweg, 24787, Fockbek"
                                , \m -> Expect.equal m.isHome False
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected second match to be parsed"
            ]
        , describe "full ICS file parsing (test-output-1.ics)"
            [ test "parses all 18 events from test-output-1.ics" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent
                    in
                    Expect.equal (List.length matches) 18
            , test "correctly identifies home and away games from full file" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        homeGames =
                            matches
                                |> List.filter (\m -> m.isHome)

                        awayGames =
                            matches
                                |> List.filter (\m -> not m.isHome)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length homeGames) 9
                        , \_ -> Expect.equal (List.length awayGames) 9
                        ]
                        ()
            , test "parses first match correctly (home game)" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        firstMatch =
                            List.head matches
                    in
                    case firstMatch of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "Osterbyer SV 2"
                                , \m -> Expect.equal m.date "22.09.2025"
                                , \m -> Expect.equal m.time "18:00"
                                , \m -> Expect.equal m.venue "Turnhalle Nobiskrug, Nobiskrüger Allee 116, 24768, Rendsburg"
                                , \m -> Expect.equal m.isHome True
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected first match to be parsed"
            , test "parses third match correctly (away game with different time)" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        thirdMatch =
                            matches
                                |> List.drop 2
                                |> List.head
                    in
                    case thirdMatch of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "SV Fockbek 3"
                                , \m -> Expect.equal m.date "09.10.2025"
                                , \m -> Expect.equal m.time "17:45"
                                , \m -> Expect.equal m.venue "Mehrzweckhalle Schulzentrum, Friedhofsweg, 24787, Fockbek"
                                , \m -> Expect.equal m.isHome False
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected third match to be parsed"
            , test "parses match with 19:15 time correctly" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        -- Find the match with 19:15 time (TSV Owschlag on 20251103)
                        matchWith1915 =
                            matches
                                |> List.filter (\m -> m.time == "19:00")
                                |> List.head
                    in
                    case matchWith1915 of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "TSV Owschlag"
                                , \m -> Expect.equal m.date "03.11.2025"
                                , \m -> Expect.equal m.time "19:00"
                                , \m -> Expect.equal m.isHome False
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected match with 19:00 time to be parsed"
            , test "parses match with 19:15 time correctly (Osterrönfelder TSV)" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        -- Find the match with 19:15 time (Osterrönfelder TSV on 20260130)
                        matchWith1915 =
                            matches
                                |> List.filter (\m -> m.time == "19:15")
                                |> List.head
                    in
                    case matchWith1915 of
                        Just match ->
                            Expect.all
                                [ \m -> Expect.equal m.opponent "Osterrönfelder TSV"
                                , \m -> Expect.equal m.date "30.01.2026"
                                , \m -> Expect.equal m.time "19:15"
                                , \m -> Expect.equal m.isHome False
                                ]
                                match

                        Nothing ->
                            Expect.fail "Expected match with 19:15 time to be parsed"
            , test "parses venue with special characters correctly" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        -- Find match with venue containing special characters (Astrid-Lindgren-Schule)
                        matchWithSpecialVenue =
                            matches
                                |> List.filter (\m -> String.contains "Astrid-Lindgren-Schule" m.venue)
                                |> List.head
                    in
                    case matchWithSpecialVenue of
                        Just match ->
                            Expect.equal match.venue "Astrid-Lindgren-Schule - Standort Neue Dorfstraße, Akazienstrasse 17, 24782, Büdelsdorf"

                        Nothing ->
                            Expect.fail "Expected match with special venue to be parsed"
            , test "parses venue with phone number correctly" <|
                \_ ->
                    let
                        matches =
                            parseIcsToMatches teamName fullIcsContent

                        -- Find match with venue containing phone number (Osterbyer SV 2 away)
                        matchWithPhone =
                            matches
                                |> List.filter (\m -> String.contains "Tel." m.venue)
                                |> List.head
                    in
                    case matchWithPhone of
                        Just match ->
                            Expect.equal match.venue "Sporth. Schulstr. 23   Tel. 04351-44540, Schulstraße 23, 24367 , Osterby"

                        Nothing ->
                            Expect.fail "Expected match with phone number in venue to be parsed"
            ]
        ]
