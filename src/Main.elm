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
    , workingCache : WorkingCache
    , answersCache : Dict String Answer
    }


type WorkingConnections
    = NotAsked
    | Asked (List (Dict Int WorkingConnection))
    | FoundConnection Answer
    | NoConnection
    | Error String


type alias WorkingCache =
    Dict Int WorkingConnection


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
      , workingCache = Dict.empty
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

                ( pendingComics, workingConnections, effect ) =
                    case ( maybeDetails, parentCharacterId ) of
                        ( RemoteData.Success details, Just id ) ->
                            let
                                allDetails =
                                    allOrNothing details

                                pending =
                                    Dict.singleton id
                                        (onlyUncached model.workingCache allDetails)

                                _ =
                                    Debug.log "allDetails: " allDetails
                            in
                            ( pending
                            , model.workingConnections
                            , Just (LoadComicCharacters pending)
                            )

                        ( RemoteData.Failure _, _ ) ->
                            ( model.pendingComics
                            , Error "Something went wrong initially!"
                            , Nothing
                            )

                        ( _, _ ) ->
                            ( model.pendingComics
                            , model.workingConnections
                            , Nothing
                            )
            in
            ( { model
                | pendingComics = pendingComics
                , workingConnections = workingConnections
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

                                    -- _ =
                                    --     Debug.log "next degree" cached
                                    -- _ =
                                    --     Debug.log (Debug.toString (List.head (Dict.keys cached.pendingComics))) 3
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
                , workingCache = shifted.workingCache
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
            , workingCache : WorkingCache
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
                pending =
                    workingConnections
                        |> List.getAt 0
                        |> Maybe.withDefault Dict.empty
                        -- now Dict Int Connection
                        |> queueComics

                _ =
                    pending
                        |> Dict.size
                        |> Debug.log "next pending"

                cachedConnections : Dict Int WorkingConnection
                cachedConnections =
                    loadConnectionsFromCache pending model.workingCache

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
                    dequeuePendingFromCached pending model.workingCache

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


dequeuePendingFromCached : PendingComics -> WorkingCache -> PendingComics
dequeuePendingFromCached pending workingCache =
    pending
        |> Dict.map
            (\parentCharacterId comics ->
                comics
                    |> Dict.filter
                        (\id comic ->
                            case Dict.get id workingCache of
                                -- should this be cache or connections
                                Just cachedComic ->
                                    cachedComic.parentId /= id

                                Nothing ->
                                    True
                        )
            )
        |> Dict.filter (\_ v -> not (Dict.isEmpty v))


loadConnectionsFromCache : PendingComics -> WorkingCache -> Dict Int WorkingConnection
loadConnectionsFromCache pending workingCache =
    workingCache
        |> Dict.filter
            (\id connection ->
                case Dict.get connection.parentId pending of
                    Just pendingCall ->
                        Maybe.isJust (Dict.get id pendingCall)

                    Nothing ->
                        False
            )
        |> Dict.values
        |> List.map (\connection -> ( connection.parentId, connection ))
        |> Dict.fromList


shiftQueue :
    List Int
    -> Comic
    -> Result Http.Error (List ComicsForCharacter)
    -> Model
    ->
        { workingConnections : WorkingConnections
        , pendingComics : PendingComics
        , workingCache : WorkingCache
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
                updatedWorkingConnections
                model.workingCache

        -- Build up pending comics (should be only uncached here)
    in
    { workingConnections = updatedWorkingConnections
    , workingCache = updatedComicsCache
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


updateComicsCache : WorkingConnections -> WorkingCache -> WorkingCache
updateComicsCache workingConnections currentCache =
    case workingConnections of
        Asked connections ->
            let
                updatedCache =
                    connections
                        |> List.getAt 0
                        |> Maybe.withDefault Dict.empty
                        |> Dict.values
                        |> List.filterMap
                            (\connection ->
                                case comicId connection.comic of
                                    Just id ->
                                        Just ( id, connection )

                                    Nothing ->
                                        Nothing
                            )
                        |> Dict.fromList
            in
            Dict.union currentCache updatedCache

        _ ->
            currentCache


updateAnswersCache : String -> WorkingConnections -> Dict String Answer -> Dict String Answer
updateAnswersCache name working answers =
    case working of
        FoundConnection answer ->
            Dict.insert name answer answers

        _ ->
            answers


queueComics : Dict Int WorkingConnection -> Dict Int Comic
queueComics workingConnections =
    -- TODO START HERE
    -- For each connection's comic either add that comic to the queue with just the
    -- character's id in parents or if the comic is already accounted for, add
    -- character's id to parents
    -- workingConnections.comics
    --     |> List.map (\comic -> ( Maybe.withDefault 0 (comicId comic), comic ))
    --     |> List.filter (\( k, _ ) -> k /= 0)
    --     -- Filtered out anything that didn't successfully translate to a comicId
    --     |> Dict.fromList
    workingConnections
        |> Dict.foldl
            (\parentId connection acc ->
                connection.comics
                    |> List.map
                        (\comic ->
                            ( case comicId comic of
                                Just id ->
                                    case Dict.get id acc of
                                        Just pending ->
                                            Dict.insert id { pending | parents = parentId :: pending.parents } acc

                                        Nothing ->
                                            Dict.insert id
                                                { name = comic.name
                                                , resource = comic.resource
                                                , parents = [ parentId ]
                                                }
                                                acc

                                Nothing ->
                                    acc
                            , { comic | parents = [ parentId ] }
                            )
                        )
             -- Filtered out anything that didn't successfully translate to a comicId
            )
            Dict.empty



-- We have a list of lists where each list holds a comic indexed by its id and containing its parent Id
-- we want to merge/dedupe these lists so lists with the same comic ID merge their parents
-- |> Dict.map (\parentId connection ->
--     List.map (add to queue or add just parentId to parents) connection.comics
-- )


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


onlyUncached : WorkingCache -> ComicsCache -> ComicsCache
onlyUncached cache newComics =
    Dict.filter (\k _ -> not (Dict.member k cache)) newComics


onlyCached : ComicsCache -> ComicsCache -> ComicsCache
onlyCached cache newComics =
    Dict.intersect newComics cache


{-| only used for the first call since this only handles 1 parent character id
-}
allOrNothing : Maybe (List SummaryComicsForCharacter) -> ComicsCache
allOrNothing details =
    case pluckComics details of
        Just comics ->
            fromList comics

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
                let
                    _ =
                        Debug.log ("Not loading comic characters for: " ++ Debug.toString pendingComics)
                in
                Cmd.none
                -- keeping this Cmd.none is _super_ helpful in performance, figure out why later

            else
                let
                    _ =
                        Debug.log ("Loading comic characters for " ++ Debug.toString (Dict.keys pendingComics))
                in
                pendingComics
                    |> Dict.toList
                    |> List.concatMap (\( parent, cache ) -> List.map (Tuple.pair parent) (Dict.values cache))
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
        { url = comic.resource ++ "/characters?ts=1&apikey=***REMOVED***&hash=***REMOVED***"
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
        , viewWorkingCache model.workingCache
        , h2 [] [ text "Pending Comics" ]
        , viewPendingComics model.pendingComics
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


viewPendingComics : PendingComics -> Html msg
viewPendingComics pendingComics =
    pendingComics
        |> Dict.map
            (\k cache ->
                div []
                    [ text ("parent character id: " ++ String.fromInt k)
                    , viewComicsCache cache
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


viewWorkingCache : WorkingCache -> Html msg
viewWorkingCache workingCache =
    workingCache
        |> Dict.map
            (\k comic ->
                div []
                    [ text ("id: " ++ String.fromInt k)
                    , text ("name: " ++ comic.comic.name)
                    , text ("resource: " ++ comic.comic.resource)
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


fromList : List Comic -> ComicsCache
fromList comics =
    comics
        |> comicTuples
        |> Dict.fromList


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
