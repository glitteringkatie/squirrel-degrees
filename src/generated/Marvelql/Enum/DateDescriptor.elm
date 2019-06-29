-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Enum.DateDescriptor exposing (DateDescriptor(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Returns a resource within a predefined date range.
-}
type DateDescriptor
    = LastWeek
    | ThisWeek
    | NextWeek
    | ThisMonth


list : List DateDescriptor
list =
    [ LastWeek, ThisWeek, NextWeek, ThisMonth ]


decoder : Decoder DateDescriptor
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "lastWeek" ->
                        Decode.succeed LastWeek

                    "thisWeek" ->
                        Decode.succeed ThisWeek

                    "nextWeek" ->
                        Decode.succeed NextWeek

                    "thisMonth" ->
                        Decode.succeed ThisMonth

                    _ ->
                        Decode.fail ("Invalid DateDescriptor type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : DateDescriptor -> String
toString enum =
    case enum of
        LastWeek ->
            "lastWeek"

        ThisWeek ->
            "thisWeek"

        NextWeek ->
            "nextWeek"

        ThisMonth ->
            "thisMonth"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe DateDescriptor
fromString enumString =
    case enumString of
        "lastWeek" ->
            Just LastWeek

        "thisWeek" ->
            Just ThisWeek

        "nextWeek" ->
            Just NextWeek

        "thisMonth" ->
            Just ThisMonth

        _ ->
            Nothing