module TimeHelpers exposing (..)

import Time exposing (Posix, utc)
import DateFormat as Format
import DateFormat.Language exposing (swedish)
import Parser


duration : Posix -> Posix -> String
duration from to =
    let
        secs =
            ((Time.posixToMillis to) - (Time.posixToMillis from)) // 1000

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


sweFormat : List Format.Token -> Posix -> String
sweFormat tokens posix =
    Format.formatWithLanguage swedish tokens utc posix


posixToDate : Posix -> String
posixToDate posix =
    sweFormat
        [ Format.yearNumber
        , Format.text "-"
        , Format.monthFixed
        , Format.text "-"
        , Format.dayOfMonthFixed
        ]
        posix


posixToTime : Posix -> String
posixToTime posix =
    sweFormat
        [ Format.hourMilitaryFixed
        , Format.text ":"
        , Format.minuteFixed
        ]
        posix


posixToTimeWithSeconds : Posix -> String
posixToTimeWithSeconds posix =
    posixToTime posix
        ++ sweFormat
            [ Format.text ":"
            , Format.secondFixed
            ]
            posix


posixToString : String -> Posix -> String
posixToString separator posix =
    posixToDate posix
        ++ separator
        ++ posixToTimeWithSeconds posix


posixToUrlString : Posix -> String
posixToUrlString posix =
    posixToString "T" posix


posixToHumanString : Posix -> String
posixToHumanString posix =
    posixToString " - " posix
