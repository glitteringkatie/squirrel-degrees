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
import Browser
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



--- Model


type alias Model =
    { workingConnections : WorkingConnections
    , pendingComics : Maybe PendingComics
    , endCharacter : String
    }


type WorkingConnections
    = NotAsked
    | Asked (List (Dict Int Connection))
    | FoundConnection (List Connection)
    | NoConnection
    | Error String


type alias PendingComics =
    { characterId : Scalar.Id
    , comics : Dict Int Comic
    }


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
    , parentId : Scalar.Id
    }


init : ( Model, Maybe Effect )
init =
    let
        endCharacter =
            "Spider-Man"
    in
    ( { endCharacter = endCharacter
      , workingConnections = NotAsked
      , pendingComics = Nothing -- comics I am currently working off of
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedEndCharacter String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List SummaryComicsForCharacter))) (Maybe (List SummaryComicsForCharacter)))
    | GotComicCharacters Comic (Result Http.Error (List ComicsForCharacter))
    | UserRequestsFurtherConnections


type Effect
    = LoadCharacterInfo
    | LoadComicCharacters


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserUpdatedEndCharacter name ->
            ( { model | endCharacter = name }, Nothing )

        UserRequestsConnection ->
            ( { model | workingConnections = Asked [ Dict.empty ] }, Just LoadCharacterInfo )

        GotCharactersComicsDetails maybeDetails ->
            let
                pendingComics =
                    case maybeDetails of
                        RemoteData.Success details ->
                            allOrNothing details

                        _ ->
                            Nothing

                -- effect =
                --     case pendingComics of
                --         Just working -> either we found the end character or we keep looking
                --         Nothing -> Nothing
                -- also consider the case that the end character is our character and we don't have to do anything
            in
            ( { model | pendingComics = pendingComics }, Nothing )

        GotComicCharacters parentComic result ->
            -- time to build up a connection and add it to WorkingConnections
            let
                -- needed to update pendingComics
                updatedComics =
                    case comicId parentComic.resource of
                        Just parentComicId ->
                            Maybe.map (.comics >> Dict.remove parentComicId) model.pendingComics

                        Nothing ->
                            Nothing

                updatedPendingComics =
                    case updatedComics of
                        Just updated ->
                            Maybe.map (\pendingComics -> { pendingComics | comics = updated }) model.pendingComics

                        Nothing ->
                            model.pendingComics

                -- needed to update workingConnections
                parentCharacter : Maybe Scalar.Id
                parentCharacter =
                    case model.pendingComics of
                        Just pendingComics ->
                            Just pendingComics.characterId

                        Nothing ->
                            Nothing

                characters : Maybe (List { id : Int, name : String })
                characters =
                    case result of
                        Ok characterList ->
                            characterList
                                |> List.map (\character -> { name = character.name, id = character.id })
                                |> Just

                        _ ->
                            Nothing

                buildConnection : { id : Int, name : String } -> Maybe ( Int, Connection )
                buildConnection character =
                    case parentCharacter of
                        Just id ->
                            if isEqualScalarInt id character.id then
                                Nothing
                                -- Don't add the parent character!

                            else
                                Just
                                    ( character.id
                                    , { character = character.name
                                      , comic = parentComic
                                      , parentId = id
                                      }
                                    )

                        _ ->
                            Nothing

                connections =
                    characters
                        |> Maybe.withDefault []
                        |> List.map buildConnection
                        |> Maybe.values
                        |> Dict.fromList

                updatedConnections =
                    case model.workingConnections of
                        Asked workingConnections ->
                            updatedComics
                                |> Maybe.unwrap False Dict.isEmpty
                                |> updateConnections workingConnections connections
                                |> Asked

                        _ ->
                            model.workingConnections

                _ =
                    Debug.log
                        (Debug.toString
                            (case updatedConnections of
                                Asked c ->
                                    Maybe.map Dict.size (List.getAt 0 c)

                                _ ->
                                    Nothing
                            )
                        )
                        3
            in
            ( { model
                | workingConnections = updatedConnections
                , pendingComics = updatedPendingComics
              }
            , Nothing
            )

        UserRequestsFurtherConnections ->
            ( model, Just LoadComicCharacters )


allOrNothing : Maybe (List SummaryComicsForCharacter) -> Maybe PendingComics
allOrNothing details =
    case ( pluckId details, pluckComics details ) of
        ( Just id, Just comics ) ->
            Just
                { comics = fromList comics
                , characterId = id
                }

        ( _, _ ) ->
            Nothing


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
updateConnections modelConnections newConnections isLastComic =
    let
        currentDegree =
            modelConnections
                |> List.getAt 0
                |> Maybe.withDefault Dict.empty

        updatedDegree =
            Dict.union currentDegree newConnections
    in
    if isLastComic then
        [ Dict.empty, updatedDegree ]

    else
        [ updatedDegree ]


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

        LoadComicCharacters ->
            case model.pendingComics of
                Just pendingComics ->
                    pendingComics.comics
                        |> Dict.values
                        |> List.map comicQuery
                        |> Cmd.batch

                Nothing ->
                    Cmd.none



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


comicQuery : Comic -> Cmd Msg
comicQuery comic =
    Http.get
        { url = comic.resource ++ "/characters?ts=1&apikey=91cd822df1814786af8af9eb2fbaa1b3&hash=85451d29757f46077d38b315d32990b2"
        , expect = Http.expectJson (GotComicCharacters comic) comicCharactersDecoder
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
        , comicLookupButton model.pendingComics
        , viewConnection model.workingConnections
        , div [] [ text "Data provided by Marvel. © 2014 Marvel" ]
        ]
        |> Html.toUnstyled


writeConnection : List Connection -> String
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
        [ text "connect the spider-man to squirrel girl" ]


comicLookupButton : Maybe PendingComics -> Html Msg
comicLookupButton pendingComics =
    button
        [ onClick UserRequestsFurtherConnections ]
        [ text "look at the squirrel's friends" ]



--- Program


main : Program () Model Msg
main =
    Browser.element
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


type alias Resource =
    String


fromList : List Comic -> Dict Int Comic
fromList comics =
    comics
        |> comicTuples
        |> Dict.fromList


{-| "<http://gateway.marvel.com/v1/public/comics/58636">
"<http:"> "" "gateway.marvel.com" "v1" "public" "comics" "58636"
-}
comicId : Resource -> Maybe Int
comicId resource =
    let
        words =
            String.split "/" resource

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
        |> List.map (\comic -> ( comicId comic.resource, comic ))
        |> List.filterMap
            (\tuple ->
                case Tuple.first tuple of
                    Just id ->
                        Just ( id, Tuple.second tuple )

                    Nothing ->
                        Nothing
            )



-- Scalar.Id helpers


isEqualScalarInt : Scalar.Id -> Int -> Bool
isEqualScalarInt (Scalar.Id scalar) id =
    case String.toInt scalar of
        Just s ->
            s == id

        Nothing ->
            False
