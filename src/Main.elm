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
    { connection : Maybe (List Connection)
    , workingConnections1 : Maybe (Dict Int Connection)
    , workingConnections2 : Maybe (Dict Int Connection)
    , workingConnections3 : Maybe (Dict Int Connection)
    , workingConnections4 : Maybe (Dict Int Connection)
    , workingConnections5 : Maybe (Dict Int Connection)
    , workingConnections6 : Maybe (Dict Int Connection)
    , workingComics : Maybe PendingComics
    , workingGraph : List ( Comic, Character ) -- where the comic connects the character to their neighbor
    , endCharacter : String
    }



-- will also have a working graph but idk what that'll look like yet


type alias Comic =
    { name : String
    , resource : String
    }


type alias Character =
    { name : String
    , id : Int
    , resource : String
    }


type alias Connection =
    { character : String
    , comic : Comic
    , parentId : Maybe Scalar.Id
    }


type alias PendingComics =
    { characterId : Scalar.Id
    , comics : List Comic
    }


init : ( Model, Maybe Effect )
init =
    let
        endCharacter =
            "Spider-Man"
    in
    ( { endCharacter = endCharacter
      , connection =
            Just
                [ { character = endCharacter
                  , comic =
                        { name = "BFFs"
                        , resource = "https://www.bffs.com"
                        }
                  , parentId = Nothing
                  }
                ]
      , workingConnections1 = Nothing
      , workingConnections2 = Nothing
      , workingConnections3 = Nothing
      , workingConnections4 = Nothing
      , workingConnections5 = Nothing
      , workingConnections6 = Nothing
      , workingGraph = []
      , workingComics = Nothing -- comics I am currently working off of
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedEndCharacter String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List SummaryComicsForCharacter))) (Maybe (List SummaryComicsForCharacter)))
    | GotComicCharacters (Result Http.Error (List ComicsForCharacter))
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
            ( model, Just LoadCharacterInfo )

        GotCharactersComicsDetails maybeDetails ->
            let
                workingComics =
                    case maybeDetails of
                        RemoteData.Success details ->
                            allOrNothing details

                        _ ->
                            Nothing

                -- effect =
                --     case workingComics of
                --         Just working -> either we found the end character or we keep looking
                --         Nothing -> Nothing
            in
            ( { model | workingComics = workingComics }, Nothing )

        GotComicCharacters result ->
            -- time to build up a connection and add it to WorkingConnections1
            let
                parentCharacter =
                    case model.workingComics of
                        Just workingComics ->
                            Just workingComics.characterId

                        Nothing ->
                            Nothing

                connectingComic =
                    case model.workingComics of
                        Just workingComics ->
                            workingComics.comics
                                |> List.head

                        Nothing ->
                            Nothing

                characters =
                    case result of
                        Ok characterList ->
                            characterList
                                |> List.map (\character -> { name = character.name, id = character.id })
                                |> Just

                        -- contains the next batch of comics, I only want the character names and their ids though
                        _ ->
                            Nothing

                -- _ =
                --     Debug.log (Debug.toString ( parentCharacter, connectingComic, characters )) 3
                buildConnection character =
                    case ( parentCharacter, connectingComic ) of
                        ( Just id, Just title ) ->
                            Just
                                ( character.id
                                , { character = character.name
                                  , comic = title
                                  , parentId = Just id
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
                    case model.workingConnections1 of
                        Just connections1 ->
                            Just (Dict.union connections1 connections)

                        Nothing ->
                            Just connections

                _ =
                    Debug.log (Debug.toString updatedConnections) 3
            in
            ( { model | workingConnections1 = updatedConnections }, Nothing )

        UserRequestsFurtherConnections ->
            ( model, Just LoadComicCharacters )


allOrNothing : Maybe (List SummaryComicsForCharacter) -> Maybe PendingComics
allOrNothing details =
    case ( pluckId details, pluckComics details ) of
        ( Just id, Just comics ) ->
            Just
                { comics = comics
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
            case model.workingComics of
                Just workingComics ->
                    comicQuery (List.head workingComics.comics)

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



-- type alias ComicDetails =
--     { id : Mabye Marvelql.ScalarCodecs.Id }
-- "query {
--   getCharacter3770981225: getCharacter(where: {name: "Spider-Man"}) {
--     id1079877010: id
--   }
-- }"


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


comicQuery : Maybe Comic -> Cmd Msg
comicQuery comic =
    case comic of
        Just book ->
            Http.get
                { url = book.resource ++ "/characters?ts=1&apikey=91cd822df1814786af8af9eb2fbaa1b3&hash=85451d29757f46077d38b315d32990b2"
                , expect = Http.expectJson GotComicCharacters (comicCharactersDecoder book)
                }

        Nothing ->
            Cmd.none



-- in data, in results which is an array of
-- I want id, name, and comics with available and items with resource and name


type alias ComicsForCharacter =
    { id : Int
    , parentComic : Comic
    , name : String
    , comics : ComicInfo
    }


type alias ComicInfo =
    { available : Int, items : List Comic }


comicCharactersDecoder : Comic -> Json.Decoder (List ComicsForCharacter)
comicCharactersDecoder comic =
    Json.field "data"
        (Json.field "results"
            (Json.list
                (Json.succeed ComicsForCharacter
                    |> required "id" Json.int
                    |> hardcoded comic
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
        , comicLookupButton model.workingComics
        , viewConnection model.connection
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


viewConnection : Maybe (List Connection) -> Html Msg
viewConnection connection =
    div [] [ text (Maybe.unwrap "No connection" writeConnection connection) ]


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
comicLookupButton workingComics =
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
