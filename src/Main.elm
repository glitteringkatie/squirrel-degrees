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
    , comicsCache : ComicsCache
    }


type WorkingConnections
    = NotAsked
    | Asked (List (Dict Int Connection))
    | FoundConnection (List Connection)
    | NoConnection
    | Error String


type alias PendingComics =
    Dict Int ComicsCache


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


init : ( Model, Maybe Effect )
init =
    ( { endCharacter = ""
      , workingConnections = NotAsked
      , pendingComics = Dict.empty -- comics I am currently working off of
      , comicsCache = Dict.empty
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedEndCharacter String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List SummaryComicsForCharacter))) (Maybe (List SummaryComicsForCharacter)))
    | GotComicCharacters Int Comic (Result Http.Error (List ComicsForCharacter))


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
                                pending =
                                    Dict.singleton id
                                        (details
                                            |> allOrNothing
                                            |> onlyUncached model.comicsCache
                                        )
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

        GotComicCharacters parentCharacterId parentComic result ->
            -- time to build up a connection and add it to WorkingConnections
            let
                -- TODO figure out how to short circuit
                { pendingComics, workingConnections, comicsCache } =
                    shiftQueue parentCharacterId parentComic result model

                pendingLength =
                    pendingComics
                        |> Dict.values
                        |> List.map Dict.values
                        |> List.map List.length
                        |> List.sum

                effect =
                    case workingConnections of
                        Asked a ->
                            let
                                _ =
                                    Debug.log "List degree is " (List.length a)

                                isNewDegree =
                                    Maybe.unwrap False Dict.isEmpty (List.head a)
                            in
                            if List.length a < 7 && isNewDegree then
                                let
                                    _ =
                                        Debug.log (Debug.toString (List.head (Dict.keys pendingComics))) 3
                                in
                                Just (LoadComicCharacters pendingComics)

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            ( { model
                | workingConnections = workingConnections
                , pendingComics = pendingComics
                , comicsCache = comicsCache
              }
            , effect
            )


shiftQueue :
    Int
    -> Comic
    -> Result Http.Error (List ComicsForCharacter)
    -> Model
    -> { workingConnections : WorkingConnections, pendingComics : PendingComics, comicsCache : ComicsCache }
shiftQueue parentCharacterId parentComic result model =
    let
        updatedPendingQueue =
            case comicId parentComic of
                Just id ->
                    model.pendingComics
                        |> Dict.get parentCharacterId
                        |> Maybe.map (Dict.remove id)

                Nothing ->
                    Nothing

        preliminaryUpdatedPending =
            updatedPendingQueue
                |> Maybe.unwrap model.pendingComics (\p -> Dict.insert parentCharacterId p model.pendingComics)
                |> Dict.filter (\k v -> v |> Dict.isEmpty |> not)

        updatedWorkingConnections =
            updateWorkingConnections
                parentCharacterId
                parentComic
                result
                preliminaryUpdatedPending
                model.endCharacter
                model.workingConnections
    in
    { workingConnections = updatedWorkingConnections
    , pendingComics =
        updatePendingComics
            preliminaryUpdatedPending
            updatedWorkingConnections
    , comicsCache =
        updateComicsCache
            parentComic
            model.comicsCache
    }


updateComicsCache : Comic -> ComicsCache -> ComicsCache
updateComicsCache parentComic currentCache =
    case comicId parentComic of
        Just parentComicId ->
            Dict.insert parentComicId parentComic currentCache

        Nothing ->
            currentCache


updatePendingComics : PendingComics -> WorkingConnections -> PendingComics
updatePendingComics pending currentConnections =
    if Dict.isEmpty pending then
        currentConnections
            |> askedWithDefault
            |> List.getAt 1
            |> Maybe.withDefault Dict.empty
            -- now Dict Int Connection
            |> Dict.map (\k v -> Dict.singleton (Maybe.withDefault 0 (comicId v.comic)) v.comic)
            |> Dict.filter (\k v -> k /= 0)
        -- Filtered out anything that didn't successfully translate to a comicId

    else
        pending


buildAnswer : List (Dict Int Connection) -> Int -> Int -> List Connection -> List Connection
buildAnswer connections startIndex id acc =
    case List.getAt startIndex connections of
        Just dict ->
            case Dict.get id dict of
                Just connection ->
                    buildAnswer
                        connections
                        (startIndex + 1)
                        connection.parentId
                        (connection :: acc)

                Nothing ->
                    acc

        Nothing ->
            acc


buildWorkingConnection : List (Dict Int Connection) -> Int -> Maybe Int -> WorkingConnections
buildWorkingConnection connections startIndex maybeId =
    case maybeId of
        Just id ->
            FoundConnection (buildAnswer connections startIndex id [])

        Nothing ->
            Asked connections


checkForConnection : String -> List (Dict Int Connection) -> WorkingConnections
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


updateWorkingConnections :
    Int
    -> Comic
    -> Result Http.Error (List ComicsForCharacter)
    -> PendingComics
    -> String
    -> WorkingConnections
    -> WorkingConnections
updateWorkingConnections parentCharacterId parentComic result updatedPendingComics name currentConnections =
    let
        buildConnection : { id : Int, name : String } -> Maybe ( Int, Connection )
        buildConnection character =
            if parentCharacterId == character.id then
                Nothing
                -- Don't add the parent character!

            else
                Just
                    ( character.id
                      -- TODO this assumes a character can only be connected by a single comic
                    , { character = character.name -- probably include character id in here
                      , comic = parentComic -- then also add the parent comic's id to character.id to make a hash
                      , parentId = parentCharacterId
                      }
                    )

        characters : List { id : Int, name : String }
        characters =
            Result.unwrap
                []
                (List.map
                    (\character -> { name = character.name, id = character.id })
                )
                result

        connections =
            characters
                |> List.map buildConnection
                |> Maybe.values
                |> Dict.fromList
    in
    case ( currentConnections, result ) of
        ( Asked current, Ok _ ) ->
            updatedPendingComics
                |> Dict.values
                |> List.map Dict.isEmpty
                |> List.all (\v -> v == True)
                |> updateConnections current connections
                |> checkForConnection name

        ( _, Err _ ) ->
            Error "Something went wrong!"

        ( _, _ ) ->
            currentConnections


onlyUncached : ComicsCache -> ComicsCache -> ComicsCache
onlyUncached cache pendingComics =
    Dict.diff pendingComics cache


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
                        Just [ { name = name, resource = resourceUri } ]

                    ( _, _ ) ->
                        Nothing
            )
        |> List.map (Maybe.withDefault [])
        |> List.concat
        |> Just


updateConnections : List (Dict Int Connection) -> Dict Int Connection -> Bool -> List (Dict Int Connection)
updateConnections currentConnections newConnections isLastComic =
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
    if isLastComic then
        List.concat [ [ Dict.empty, updatedDegree ], untouchedConnections ]

    else
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


comicQuery : ( Int, Comic ) -> Cmd Msg
comicQuery ( characterId, comic ) =
    Http.get
        { url = comic.resource ++ "/characters?ts=1&apikey=91cd822df1814786af8af9eb2fbaa1b3&hash=85451d29757f46077d38b315d32990b2"
        , expect = Http.expectJson (GotComicCharacters characterId comic) comicCharactersDecoder
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
        )



--- View


view : Model -> NormHtml.Html Msg
view model =
    div []
        [ characterInput model.endCharacter
        , characterSubmitButton model.endCharacter
        , viewConnection model.workingConnections
        , div [] [ text "Data provided by Marvel. © 2014 Marvel" ]
        ]
        |> Html.toUnstyled


writeConnection : List Connection -> String -> String
writeConnection connection acc =
    case connection of
        [] ->
            acc ++ "Squirrel Girl"

        conn :: conns ->
            writeConnection conns (acc ++ conn.character ++ " is in " ++ conn.comic.name ++ " with ")


viewConnection : WorkingConnections -> Html Msg
viewConnection working =
    case working of
        FoundConnection connection ->
            div [] [ text (writeConnection connection "") ]

        NoConnection ->
            div [] [ text "No connection" ]

        Error problem ->
            div [] [ text problem ]

        Asked _ ->
            div [] [ text "Connections pending" ]

        NotAsked ->
            div [] []


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


askedWithDefault : WorkingConnections -> List (Dict Int Connection)
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
