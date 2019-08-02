module Main exposing
    ( Connection
    , Model
    , Msg(..)
    , characterInput
    , init
    , main
    , update
    , view
    , viewConnection
    , writeConnection
    )

import Accessibility.Styled as Html
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , img
        , inputText
        , labelHidden
        , text
        )
import BeautifulExample
import Browser
import Color
import Dict exposing (Dict)
import Graphql.Http
import Graphql.Http.GraphqlError exposing (GraphqlError)
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html as NormHtml
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra as List
import Marvelql.InputObject exposing (buildCharacterWhereInput, buildComicWhereInput)
import Marvelql.Object exposing (Character(..), Comic(..))
import Marvelql.Object.Character as CharacterApi
import Marvelql.Object.Comic as ComicApi
import Marvelql.Object.Summary as Summary
import Marvelql.Query as Query
import Marvelql.Scalar as Scalar
import Marvelql.ScalarCodecs
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData)
import Result.Extra as Result



--- Model


type alias Model =
    { workingConnections : WorkingConnections
    , pendingComics : PendingComics
    , endCharacter : String
    , comicApiCache : ComicApiCache
    , answersCache : Dict String Answer
    }


type WorkingConnections
    = NotAsked
    | Asked (List (Dict Int WorkingConnection))
    | FoundConnection Answer
    | NoConnection
    | Error String


{-| by comic ID
-}
type alias ComicApiCache =
    Dict Int (List ComicsForCharacter)


type alias Answer =
    List Connection


type alias PendingComics =
    Dict Int Comic


type alias Character =
    { name : String
    , id : Int
    , resource : String
    }


{-| connection =
Just
[
{ character = endCharacter
, comic = { name = "BFFs", resource = "https://www.bffs.com" }
, parentId = Nothing
}
]
-}
type alias Connection =
    { character : String
    , comic : Comic
    , parentId : Int
    }


type alias WorkingConnection =
    { character : String
    , comic : Comic
    , parentId : Int
    , comics : List Comic
    }


formalizeConnection : WorkingConnection -> Connection
formalizeConnection working =
    { character = working.character
    , comic = working.comic
    , parentId = working.parentId
    }


init : ( Model, Maybe Effect )
init =
    ( { endCharacter = ""
      , workingConnections = NotAsked
      , pendingComics = Dict.empty -- comics I am currently working off of
      , comicApiCache = Dict.empty
      , answersCache = Dict.empty
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedEndCharacter String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List SummaryComicsForCharacter))) (Maybe (List SummaryComicsForCharacter)))
    | GotComicCharacters (List Int) Comic (Result Http.Error (List ComicsForCharacter))


type Effect
    = LoadCharacterInfo
    | LoadComicCharacters PendingComics


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserUpdatedEndCharacter name ->
            ( { model | endCharacter = name }, Nothing )

        UserRequestsConnection ->
            let
                ( workingConnections, effect ) =
                    if model.endCharacter == "Squirrel Girl" then
                        ( FoundConnection [], Nothing )

                    else
                        case Dict.get model.endCharacter model.answersCache of
                            Just connections ->
                                ( FoundConnection connections, Nothing )

                            Nothing ->
                                ( Asked [ Dict.empty ], Just LoadCharacterInfo )
            in
            ( { model | workingConnections = workingConnections }, effect )

        GotCharactersComicsDetails maybeDetails ->
            let
                parentCharacterId =
                    case maybeDetails of
                        RemoteData.Success details ->
                            details
                                |> pluckId
                                |> Maybe.map fromScalarToInt
                                |> Maybe.join

                        _ ->
                            Nothing

                { pendingComics, workingConnections, answersCache, effect } =
                    case ( maybeDetails, parentCharacterId ) of
                        ( RemoteData.Success details, Just id ) ->
                            let
                                allDetails =
                                    allOrNothing id details

                                cachedConnections : Dict Int WorkingConnection
                                cachedConnections =
                                    loadConnectionsFromCache allDetails model.comicApiCache

                                working : WorkingConnections
                                working =
                                    checkForConnection model.endCharacter [ cachedConnections ]

                                _ =
                                    Debug.log "working connections are " cachedConnections

                                answers =
                                    updateAnswersCache
                                        model.endCharacter
                                        working
                                        model.answersCache

                                pending =
                                    uncachedPending model.comicApiCache allDetails
                            in
                            { pendingComics = pending
                            , workingConnections = working
                            , answersCache = answers
                            , effect = Just (LoadComicCharacters pending)
                            }

                        ( RemoteData.Failure _, _ ) ->
                            { pendingComics = model.pendingComics
                            , workingConnections = Error "Something went wrong initially!"
                            , answersCache = model.answersCache
                            , effect = Nothing
                            }

                        ( _, _ ) ->
                            { pendingComics = model.pendingComics
                            , workingConnections = model.workingConnections
                            , answersCache = model.answersCache
                            , effect = Nothing
                            }
            in
            ( { model
                | pendingComics = pendingComics
                , workingConnections = workingConnections
                , answersCache = answersCache
              }
            , effect
            )

        GotComicCharacters parentCharacters parentComic result ->
            -- time to build up a connection and add it to WorkingConnections
            let
                -- TODO figure out how to short circuit
                shifted =
                    shiftQueue parentCharacters parentComic result model

                pendingLength =
                    shifted.pendingComics
                        |> Dict.values
                        |> List.length

                ( { workingConnections, pendingComics, answersCache }, effect ) =
                    case shifted.workingConnections of
                        Asked a ->
                            let
                                _ =
                                    Debug.log "List degree" (List.length a)

                                _ =
                                    Debug.log "Total pending" pendingLength
                            in
                            if List.length a < 6 && pendingLength == 0 then
                                let
                                    cached =
                                        nextDegree model.endCharacter shifted
                                in
                                ( cached, Just (LoadComicCharacters cached.pendingComics) )

                            else
                                ( { workingConnections = shifted.workingConnections
                                  , pendingComics = shifted.pendingComics
                                  , answersCache = shifted.answersCache
                                  }
                                , Nothing
                                )

                        _ ->
                            ( { workingConnections = shifted.workingConnections
                              , pendingComics = shifted.pendingComics
                              , answersCache = shifted.answersCache
                              }
                            , Nothing
                            )
            in
            ( { model
                | workingConnections = workingConnections
                , pendingComics = pendingComics
                , comicApiCache = shifted.comicApiCache
                , answersCache = answersCache
              }
            , effect
            )


nextDegree :
    String
    ->
        { a
            | workingConnections : WorkingConnections
            , pendingComics : PendingComics
            , answersCache : Dict String Answer
            , comicApiCache : ComicApiCache
        }
    ->
        { workingConnections : WorkingConnections
        , pendingComics : PendingComics
        , answersCache : Dict String Answer
        }
nextDegree endCharacter model =
    case model.workingConnections of
        Asked workingConnections ->
            let
                pending : PendingComics
                -- indexed by comic id, contains parent characters
                pending =
                    workingConnections
                        |> List.getAt 0
                        |> Maybe.withDefault Dict.empty
                        -- now Dict Int Connection
                        |> queueComics

                _ =
                    pending
                        |> Debug.log "next pending"

                cachedConnections : Dict Int WorkingConnection
                cachedConnections =
                    loadConnectionsFromCache pending model.comicApiCache

                working : WorkingConnections
                working =
                    [ [ cachedConnections ], workingConnections ]
                        |> List.concat
                        |> checkForConnection endCharacter

                answersCache =
                    updateAnswersCache
                        endCharacter
                        working
                        model.answersCache

                dequeued =
                    dequeuePendingFromCached pending model.comicApiCache

                _ =
                    dequeued
                        |> Dict.values
                        |> List.length
                        |> Debug.log "next dequeued pending"
            in
            { workingConnections = working
            , pendingComics = dequeued
            , answersCache = answersCache
            }

        _ ->
            { workingConnections = model.workingConnections
            , pendingComics = model.pendingComics
            , answersCache = model.answersCache
            }


dequeuePendingFromCached : PendingComics -> ComicApiCache -> PendingComics
dequeuePendingFromCached pending comicApiCache =
    Dict.filter (\k _ -> not (Dict.member k comicApiCache)) pending


loadConnectionsFromCache : PendingComics -> ComicApiCache -> Dict Int WorkingConnection
loadConnectionsFromCache pending comicApiCache =
    -- pending comics is indexed by comic id and contains a list of parent characters
    -- comics cache is indexed by comic ids and contains a list of parent characters
    -- based on pending, we'll check if the comic
    pending
        |> Dict.toList
        |> List.filterMap
            (\( id, comic ) ->
                case Dict.get id comicApiCache of
                    Just cachedCall ->
                        Just ( comic, cachedCall )

                    Nothing ->
                        Nothing
            )
        |> List.map buildConnectionFromCache
        |> List.concat
        |> Dict.fromList


buildConnectionFromCache : ( Comic, List ComicsForCharacter ) -> List ( Int, WorkingConnection )
buildConnectionFromCache ( parentComic, characters ) =
    characters
        |> List.map
            (\character -> { name = character.name, id = character.id, comics = character.comics.items })
        |> List.map (buildConnections (Just parentComic))
        |> List.concat


shiftQueue :
    List Int
    -> Comic
    -> Result Http.Error (List ComicsForCharacter)
    -> Model
    ->
        { workingConnections : WorkingConnections
        , pendingComics : PendingComics
        , comicApiCache : ComicApiCache
        , answersCache : Dict String Answer
        }
shiftQueue parentCharacters parentComic result model =
    let
        pendingWithoutCurrentComic =
            dequeuePendingComic parentComic model.pendingComics

        isLastComic =
            Dict.isEmpty pendingWithoutCurrentComic

        -- update working connections with new uncached nodes
        updatedWorkingConnections =
            updateWorkingConnections
                parentComic
                result
                model

        -- build the new cache
        updatedComicsCache =
            updateComicsCache
                parentComic
                result
                model.comicApiCache

        -- Build up pending comics (should be only uncached here)
    in
    { workingConnections = updatedWorkingConnections
    , comicApiCache = updatedComicsCache
    , answersCache =
        updateAnswersCache
            model.endCharacter
            updatedWorkingConnections
            model.answersCache
    , pendingComics =
        pendingWithoutCurrentComic
    }


dequeuePendingComic : Comic -> PendingComics -> PendingComics
dequeuePendingComic parentComic pendingComics =
    case comicId parentComic of
        Just id ->
            Dict.remove id pendingComics

        Nothing ->
            pendingComics


updateComicsCache : Comic -> Result Http.Error (List ComicsForCharacter) -> ComicApiCache -> ComicApiCache
updateComicsCache parentComic result currentCache =
    case ( comicId parentComic, result ) of
        ( Just id, Ok characters ) ->
            Dict.insert id characters currentCache

        ( _, _ ) ->
            currentCache


updateAnswersCache : String -> WorkingConnections -> Dict String Answer -> Dict String Answer
updateAnswersCache name working answers =
    case working of
        FoundConnection answer ->
            Dict.insert name answer answers

        _ ->
            answers


queueConnectionsComic : ( Int, Comic ) -> Dict Int Comic -> Dict Int Comic
queueConnectionsComic ( parentId, comic ) acc =
    case comicId comic of
        Just id ->
            case Dict.get id acc of
                Just existing ->
                    -- use what's there and just update the parents list
                    Dict.insert id { existing | parents = parentId :: existing.parents } acc

                Nothing ->
                    -- entirely new insert
                    Dict.insert id
                        { name = comic.name
                        , resource = comic.resource
                        , parents = [ parentId ]
                        }
                        acc

        Nothing ->
            acc


queueComics : Dict Int WorkingConnection -> Dict Int Comic
queueComics workingConnections =
    -- For each connection's comic either add that comic to the queue with just the
    -- character's id in parents or if the comic is already accounted for, add
    -- character's id to parents
    workingConnections
        |> Dict.toList
        |> List.concatMap
            (\( characterId, connection ) ->
                -- here we don't have to worry about a unique key yet
                List.map (\v -> ( characterId, v )) connection.comics
            )
        |> List.foldl queueConnectionsComic Dict.empty


buildAnswer : List (Dict Int WorkingConnection) -> Int -> Int -> Answer -> Answer
buildAnswer connections startIndex id acc =
    case List.getAt startIndex connections of
        Just dict ->
            case Dict.get id dict of
                Just connection ->
                    let
                        formalConnection =
                            formalizeConnection connection
                    in
                    buildAnswer
                        connections
                        (startIndex + 1)
                        formalConnection.parentId
                        (formalConnection :: acc)

                Nothing ->
                    acc

        Nothing ->
            acc


buildWorkingConnection : List (Dict Int WorkingConnection) -> Int -> Maybe Int -> WorkingConnections
buildWorkingConnection connections startIndex maybeId =
    case maybeId of
        Just id ->
            FoundConnection (buildAnswer connections startIndex id [])

        Nothing ->
            Asked connections


checkForConnection : String -> List (Dict Int WorkingConnection) -> WorkingConnections
checkForConnection name connections =
    let
        focusedIndex =
            if Maybe.unwrap True Dict.isEmpty (List.getAt 0 connections) && List.length connections > 1 then
                1

            else
                0
    in
    case List.getAt focusedIndex connections of
        Just focused ->
            focused
                |> Dict.filter (\k v -> v.character == name)
                |> Dict.keys
                |> List.head
                |> buildWorkingConnection connections focusedIndex

        Nothing ->
            Asked connections


buildConnection : Comic -> { id : Int, name : String, comics : List Comic } -> Int -> Maybe ( Int, WorkingConnection )
buildConnection parentComic character parentCharacterId =
    if parentCharacterId == character.id then
        Nothing
        -- TODO confirm this isn't actually problematic
        -- Don't add the parent character!

    else
        Just
            ( character.id
              -- TODO this assumes a character can only be connected by a single comic
              -- maybe this is the problem right now
            , { character = character.name -- probably include character id in here
              , comic = parentComic -- then also add the parent comic's id to character.id to make a hash
              , parentId = parentCharacterId
              , comics = character.comics
              }
            )


buildConnections : Maybe Comic -> { id : Int, name : String, comics : List Comic } -> List ( Int, WorkingConnection )
buildConnections maybeComic character =
    case maybeComic of
        Just comic ->
            List.filterMap (buildConnection comic character) comic.parents

        Nothing ->
            []


updateWorkingConnections :
    Comic
    -> Result Http.Error (List ComicsForCharacter)
    -> Model
    -> WorkingConnections
updateWorkingConnections parentComic result model =
    let
        -- result is the list of character info including the comics they are in
        -- so characters are the list of characters connected to parentCharacterId by parentComic
        dequeuedComic =
            Dict.get (Maybe.withDefault -1 (comicId parentComic)) model.pendingComics

        characters : List { id : Int, name : String, comics : List Comic }
        characters =
            Result.unwrap
                []
                (List.map
                    (\character -> { name = character.name, id = character.id, comics = character.comics.items })
                )
                result

        connections =
            characters
                |> List.map (buildConnections dequeuedComic)
                |> List.concat
                |> Dict.fromList
    in
    case ( model.workingConnections, result ) of
        ( Asked current, Ok _ ) ->
            connections
                |> updateConnections current
                |> checkForConnection model.endCharacter

        ( _, Err _ ) ->
            Error "Something went wrong!"

        ( _, _ ) ->
            model.workingConnections


uncachedPending : ComicApiCache -> PendingComics -> PendingComics
uncachedPending cache newComics =
    Dict.filter (\k _ -> not (Dict.member k cache)) newComics


allOrNothing : Int -> Maybe (List SummaryComicsForCharacter) -> PendingComics
allOrNothing characterId details =
    case pluckComics details of
        Just comics ->
            comics
                |> List.map (addParent characterId)
                |> fromList

        Nothing ->
            Dict.empty


pluckId : Maybe (List SummaryComicsForCharacter) -> Maybe Scalar.Id
pluckId details =
    details
        |> Maybe.unwrap [] (List.map .id)
        |> List.map (Maybe.withDefault (Scalar.Id ""))
        -- I'm assuming that only one character was returned
        |> List.head


pluckComics : Maybe (List SummaryComicsForCharacter) -> Maybe (List Comic)
pluckComics details =
    details
        |> Maybe.unwrap [] (List.map .comics)
        |> List.map (Maybe.withDefault [])
        |> List.concat
        |> List.map
            (\comic ->
                case ( comic.name, comic.resourceUri ) of
                    ( Just name, Just resourceUri ) ->
                        Just [ { name = name, resource = resourceUri, parents = [] } ]

                    ( _, _ ) ->
                        Nothing
            )
        |> List.map (Maybe.withDefault [])
        |> List.concat
        |> Just


updateConnections : List (Dict Int WorkingConnection) -> Dict Int WorkingConnection -> List (Dict Int WorkingConnection)
updateConnections currentConnections newConnections =
    let
        currentDegree =
            currentConnections
                |> List.getAt 0
                |> Maybe.withDefault Dict.empty

        updatedDegree =
            Dict.union currentDegree newConnections

        untouchedConnections =
            Maybe.withDefault [] (List.tail currentConnections)
    in
    List.concat [ [ updatedDegree ], untouchedConnections ]


perform : ( Model, Maybe Effect ) -> ( Model, Cmd Msg )
perform ( model, effects ) =
    ( model
    , Maybe.map (runEffect model) effects
        |> Maybe.withDefault Cmd.none
    )


runEffect : Model -> Effect -> Cmd Msg
runEffect model effect =
    case effect of
        LoadCharacterInfo ->
            startQuery
                |> Graphql.Http.queryRequest "https://api.marvelql.com/"
                |> Graphql.Http.send (RemoteData.fromResult >> GotCharactersComicsDetails)

        LoadComicCharacters pendingComics ->
            if Dict.isEmpty pendingComics then
                Cmd.none
                -- keeping this Cmd.none is _super_ helpful in performance, figure out why later

            else
                pendingComics
                    |> Dict.values
                    |> List.map (\comic -> ( comic.parents, comic ))
                    |> List.map comicQuery
                    |> Cmd.batch



--- API


type alias SummaryData =
    { name : Maybe String
    , resourceUri : Maybe String
    }


type alias SummaryComicsForCharacter =
    { id : Maybe Scalar.Id

    -- , name : Maybe String
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


comicQuery : ( List Int, Comic ) -> Cmd Msg
comicQuery ( parentCharacters, comic ) =
    Http.get
        { url = comic.resource ++ "/characters?ts=1&apikey=91cd822df1814786af8af9eb2fbaa1b3&hash=85451d29757f46077d38b315d32990b2"
        , expect = Http.expectJson (GotComicCharacters parentCharacters comic) comicCharactersDecoder
        }


type alias ComicsForCharacter =
    { id : Int
    , name : String
    , comics : ComicInfo
    }


type alias ComicInfo =
    { available : Int, items : List Comic }


comicCharactersDecoder : Json.Decoder (List ComicsForCharacter)
comicCharactersDecoder =
    Json.field "data"
        (Json.field "results"
            (Json.list
                (Json.succeed ComicsForCharacter
                    |> required "id" Json.int
                    |> required "name" Json.string
                    |> required "comics" comicInfoDecoder
                )
            )
        )


comicInfoDecoder : Json.Decoder ComicInfo
comicInfoDecoder =
    Json.succeed ComicInfo
        |> required "available" Json.int
        |> required "items" comicsDecoder


comicsDecoder : Json.Decoder (List Comic)
comicsDecoder =
    Json.list
        (Json.succeed Comic
            |> required "name" Json.string
            |> required "resourceURI" Json.string
            |> hardcoded []
        )



--- View


view : Model -> NormHtml.Html Msg
view model =
    div []
        [ characterInput model.endCharacter
        , characterSubmitButton model.endCharacter
        , viewConnection model.workingConnections
        , h2 [] [ text "Cached Comics" ]
        , viewComicApiCache model.comicApiCache
        , h2 [] [ text "Pending Comics" ]
        , viewComicsCache model.pendingComics
        , div [] [ text "Data provided by Marvel. © 2014 Marvel" ]
        ]
        |> Html.toUnstyled


writeConnection : Answer -> String
writeConnection connection =
    case connection of
        [] ->
            "Squirrel Girl"

        conn :: conns ->
            conn.character ++ " is in " ++ conn.comic.name ++ " with " ++ writeConnection conns


viewConnection : WorkingConnections -> Html Msg
viewConnection working =
    case working of
        FoundConnection connection ->
            div [] [ text (writeConnection connection) ]

        NoConnection ->
            div [] [ text "No connection" ]

        Error problem ->
            div [] [ text problem ]

        Asked _ ->
            div [] [ text "Connections pending" ]

        NotAsked ->
            div [] []


viewComicApiCache : ComicApiCache -> Html msg
viewComicApiCache apiCache =
    apiCache
        |> Dict.map
            (\k _ ->
                div []
                    [ text ("comic id: " ++ String.fromInt k)
                    ]
            )
        |> Dict.values
        |> div []


viewComicsCache : ComicsCache -> Html msg
viewComicsCache comicsCache =
    comicsCache
        |> Dict.map
            (\k comic ->
                div []
                    [ text ("id: " ++ String.fromInt k)
                    , text ("name: " ++ comic.name)
                    , text ("resource: " ++ comic.resource)
                    ]
            )
        |> Dict.values
        |> div []


characterInput : String -> Html Msg
characterInput name =
    labelHidden
        "character-name-input"
        []
        (text "Character Name:")
        (inputText name [ onInput UserUpdatedEndCharacter ])


characterSubmitButton : String -> Html Msg
characterSubmitButton name =
    button
        [ onClick UserRequestsConnection ]
        [ text "connect to squirrel girl" ]



--- Program


main : Program () Model Msg
main =
    BeautifulExample.element
        { title = "6 Degrees of Squirrel Girl"
        , details = Nothing
        , color = Just Color.charcoal
        , maxWidth = 820
        , githubUrl = Just "https://github.com/glitteringkatie/squirrel-degrees"
        , documentationUrl = Nothing
        }
        { view = view
        , init = \flags -> perform init
        , update = \msg model -> perform (update msg model)
        , subscriptions = always Sub.none
        }



-- Comic stuff


type alias Comic =
    { name : String
    , resource : String
    , parents : List Int
    }


type alias ComicsCache =
    Dict Int Comic


type alias Resource =
    String



-- type alias ComicsForCharacter =
--     { id : Int
--     , name : String
--     , comics : ComicInfo
--     }
-- type alias ComicInfo =
--     { available : Int, items : List Comic }


fromList : List Comic -> PendingComics
fromList comics =
    comics
        |> comicTuples
        |> Dict.fromList


addParent : Int -> Comic -> Comic
addParent characterId comic =
    let
        uniqueParents =
            List.unique (characterId :: comic.parents)
    in
    { comic | parents = uniqueParents }


{-| "<http://gateway.marvel.com/v1/public/comics/58636">
"<http:"> "" "gateway.marvel.com" "v1" "public" "comics" "58636"
-}
comicId : Comic -> Maybe Int
comicId comic =
    let
        words =
            String.split "/" comic.resource

        comicsIndex =
            List.elemIndex "comics" words
    in
    case comicsIndex of
        Just index ->
            words
                |> List.getAt (index + 1)
                |> Maybe.map String.toInt
                |> Maybe.join

        Nothing ->
            Nothing


comicTuples : List Comic -> List ( Int, Comic )
comicTuples comics =
    comics
        |> List.map (\comic -> ( comicId comic, comic ))
        |> List.filterMap
            (\tuple ->
                case Tuple.first tuple of
                    Just id ->
                        Just ( id, Tuple.second tuple )

                    Nothing ->
                        Nothing
            )



--- WorkingConnections


askedWithDefault : WorkingConnections -> List (Dict Int WorkingConnection)
askedWithDefault connection =
    case connection of
        Asked a ->
            a

        _ ->
            []



-- Scalar.Id helpers


isEqualScalarInt : Scalar.Id -> Int -> Bool
isEqualScalarInt (Scalar.Id scalar) id =
    case String.toInt scalar of
        Just s ->
            s == id

        Nothing ->
            False


fromScalarToInt : Scalar.Id -> Maybe Int
fromScalarToInt (Scalar.Id scalar) =
    String.toInt scalar
