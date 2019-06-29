-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Enum.ComicFormat exposing (ComicFormat(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Return only series containing one or more comics with the specified format.

  - Magazine - Return only series containing one or more comics with the specified format of a magazine.
  - Trade\_paperback - Return only series containing one or more comics with the specified format of a trade paperback.
  - Hardcover - Return only series containing one or more comics with the specified format of a hardcover.
  - Digest - Return only series containing one or more comics with the specified format of a digest.
  - Graphic\_novel - Return only series containing one or more comics with the specified format of a graphic novel.
  - Comic - Return only series containing one or more comics with the specified format of a comic.
  - Digital\_comic - Return only series containing one or more comics with the specified format of a digital comic.
  - Infinite\_comic - Return only series containing one or more comics with the specified format of an infinite comic.

-}
type ComicFormat
    = Magazine
    | Trade_paperback
    | Hardcover
    | Digest
    | Graphic_novel
    | Comic
    | Digital_comic
    | Infinite_comic


list : List ComicFormat
list =
    [ Magazine, Trade_paperback, Hardcover, Digest, Graphic_novel, Comic, Digital_comic, Infinite_comic ]


decoder : Decoder ComicFormat
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "magazine" ->
                        Decode.succeed Magazine

                    "trade_paperback" ->
                        Decode.succeed Trade_paperback

                    "hardcover" ->
                        Decode.succeed Hardcover

                    "digest" ->
                        Decode.succeed Digest

                    "graphic_novel" ->
                        Decode.succeed Graphic_novel

                    "comic" ->
                        Decode.succeed Comic

                    "digital_comic" ->
                        Decode.succeed Digital_comic

                    "infinite_comic" ->
                        Decode.succeed Infinite_comic

                    _ ->
                        Decode.fail ("Invalid ComicFormat type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : ComicFormat -> String
toString enum =
    case enum of
        Magazine ->
            "magazine"

        Trade_paperback ->
            "trade_paperback"

        Hardcover ->
            "hardcover"

        Digest ->
            "digest"

        Graphic_novel ->
            "graphic_novel"

        Comic ->
            "comic"

        Digital_comic ->
            "digital_comic"

        Infinite_comic ->
            "infinite_comic"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ComicFormat
fromString enumString =
    case enumString of
        "magazine" ->
            Just Magazine

        "trade_paperback" ->
            Just Trade_paperback

        "hardcover" ->
            Just Hardcover

        "digest" ->
            Just Digest

        "graphic_novel" ->
            Just Graphic_novel

        "comic" ->
            Just Comic

        "digital_comic" ->
            Just Digital_comic

        "infinite_comic" ->
            Just Infinite_comic

        _ ->
            Nothing