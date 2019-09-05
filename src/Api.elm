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
import Secrets exposing (marvelHash, marvelPublicKey)
import Time


type alias ComicApiCache =
    Dict Int (List ComicsForCharacter)



-- GraphQL Types


type alias SummaryData =
    { name : Maybe String
    , resourceUri : Maybe String
    }


type alias SummaryComicsForCharacter =
    { id : Maybe Scalar.Id
    , comics : Maybe (List SummaryData)
    }



-- Json Decoder Types


type alias ComicsForCharacter =
    { id : Int
    , name : String
    , comics : ComicInfo
    }


type alias ComicInfo =
    { available : Int, items : List Comic }



-- GraphQL Query


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



-- REST Query


comicQuery : (Result.Result Http.Error (List ComicsForCharacter) -> msg) -> Json.Decoder (List ComicsForCharacter) -> String -> Cmd msg
comicQuery message decoder url =
    let
        timestamp =
            -- The timestamp is pretty arbitrary and while the docs say you need
            -- to change it each call, I didn't experience that need.
            -- see "Authentication for Server-Side Applications" in
            -- https://developer.marvel.com/documentation/authorization
            "42"
    in
    Http.get
        { url =
            url
                ++ "/characters?ts="
                ++ timestamp
                ++ "&apikey="
                ++ marvelPublicKey
                ++ "&hash="
                ++ marvelHash timestamp
        , expect = Http.expectJson message decoder
        }



-- Comic Stuff


type alias Comic =
    { name : String
    , resource : String
    , parents : List Int
    }
