module TimeHelpers exposing (duration, format, posixToDate, posixToHumanString, posixToMonthNameYear, posixToNameDate, posixToString, posixToTime, posixToTimeWithSeconds, posixToUrlString)

import DateFormat as Format
import DateFormat.Language exposing (Language, english)
import Parser
import Time exposing (Posix, utc)


duration : Posix -> Posix -> String
duration from to =
    let
        secs =
            (Time.posixToMillis to - Time.posixToMillis from) // 1000

        mins =
            toFloat secs / 60 |> round

        hours =
            toFloat mins / 60 |> round
    in
    if secs < 2 then
        "1 sekund"

    else if secs < 91 then
        String.fromInt secs ++ " sekunder"

    else if mins < 151 then
        String.fromInt mins ++ " minuter"

    else
        String.fromInt hours ++ " timmar"


format :
    List Format.Token
    -> Language
    -> Time.Zone
    -> Posix
    -> String
format tokens language tz posix =
    Format.formatWithLanguage language tokens tz posix


posixToDate : Language -> Time.Zone -> Posix -> String
posixToDate language tz posix =
    format
        [ Format.yearNumber
        , Format.text "-"
        , Format.monthFixed
        , Format.text "-"
        , Format.dayOfMonthFixed
        ]
        language
        tz
        posix


posixToNameDate : Language -> Time.Zone -> Posix -> String
posixToNameDate language tz posix =
    format
        [ Format.dayOfMonthNumber
        , Format.text " "
        , Format.monthNameAbbreviated
        ]
        language
        tz
        posix


posixToMonthNameYear : Language -> Time.Zone -> Posix -> String
posixToMonthNameYear language tz posix =
    format
        [ Format.monthNameAbbreviated
        , Format.text " "
        , Format.yearNumber
        ]
        language
        tz
        posix


posixToTime : Language -> Time.Zone -> Posix -> String
posixToTime language tz posix =
    format
        [ Format.hourMilitaryFixed
        , Format.text ":"
        , Format.minuteFixed
        ]
        language
        tz
        posix


posixToTimeWithSeconds : Language -> Time.Zone -> Posix -> String
posixToTimeWithSeconds language tz posix =
    posixToTime language tz posix
        ++ format
            [ Format.text ":"
            , Format.secondFixed
            ]
            language
            tz
            posix


posixToString : String -> Posix -> String
posixToString separator posix =
    posixToDate english utc posix
        ++ separator
        ++ posixToTimeWithSeconds english utc posix


posixToUrlString : Posix -> String
posixToUrlString posix =
    posixToString "T" posix


posixToHumanString : Posix -> String
posixToHumanString posix =
    posixToString " - " posix
