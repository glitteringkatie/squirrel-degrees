module Api exposing
    ( Comic
    , ComicApiCache
    , ComicInfo
    , ComicsForCharacter
    , SummaryComicsForCharacter
    , comicQuery
    , startQuery
    )

import Dict exposing (Dict)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Http
import Json.Decode as Json
import Marvelql.InputObject exposing (buildCharacterWhereInput, buildComicWhereInput)
import Marvelql.Object exposing (Character(..), Comic(..))
import Marvelql.Object.Character as CharacterApi
import Marvelql.Object.Comic as ComicApi
import Marvelql.Object.Summary as Summary
import Marvelql.Query as Query
import Marvelql.Scalar as Scalar
import Marvelql.ScalarCodecs
import Maybe.Extra as Maybe


type alias ComicApiCache =
    Dict Int (List ComicsForCharacter)


type alias SummaryData =
    { name : Maybe String
    , resourceUri : Maybe String
    }


type alias SummaryComicsForCharacter =
    { id : Maybe Scalar.Id
    , comics : Maybe (List SummaryData)
    }


unwrapComicNames : Maybe (List (Maybe String)) -> List String
unwrapComicNames comics =
    Maybe.unwrap
        []
        (List.map (\comic -> Maybe.withDefault "" comic))
        comics


startQuery : SelectionSet (Maybe (List SummaryComicsForCharacter)) RootQuery
startQuery =
    let
        whereClause =
            buildCharacterWhereInput
                (\optionals ->
                    { optionals | name = Present "Squirrel Girl" }
                )
    in
    Query.characters
        (\optionals ->
            { optionals | where_ = Present whereClause }
        )
        (SelectionSet.map2 SummaryComicsForCharacter
            CharacterApi.id
            (CharacterApi.comics
                (SelectionSet.map2 SummaryData
                    Summary.name
                    Summary.resourceURI
                )
            )
        )


comicQuery : (Result.Result Http.Error (List ComicsForCharacter) -> msg) -> Json.Decoder (List ComicsForCharacter) -> String -> Cmd msg
comicQuery message decoder url =
    Http.get
        { url = url ++ "/characters?ts=1&apikey=91cd822df1814786af8af9eb2fbaa1b3&hash=85451d29757f46077d38b315d32990b2"
        , expect = Http.expectJson message decoder
        }


type alias ComicsForCharacter =
    { id : Int
    , name : String
    , comics : ComicInfo
    }


type alias ComicInfo =
    { available : Int, items : List Comic }



-- Comic Stuff


type alias Comic =
    { name : String
    , resource : String
    , parents : List Int
    }
