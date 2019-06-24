module Main exposing
    ( Connection
    , Model
    , Msg(..)
    , heroInput
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
    , workingConnections1 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingConnections2 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingConnections3 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingConnections4 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingConnections5 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingConnections6 : Maybe (Dict Marvelql.ScalarCodecs.Id Connection)
    , workingComics : Maybe WorkingComics
    , startHero : String
    }



-- will also have a working graph but idk what that'll look like yet


type alias ComicDetail =
    { name : String
    , resource : String
    }


type alias Connection =
    { hero : String
    , comic : ComicDetail
    , parentId : Maybe Marvelql.ScalarCodecs.Id
    }


init : ( Model, Maybe Effect )
init =
    let
        startHero =
            "Spider-Man"
    in
    ( { startHero = startHero
      , connection =
            Just
                [ { hero = startHero
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
      , workingComics = Nothing -- comics I am currently working off of
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedStartHero String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List CharactersComicsDetails))) (Maybe (List CharactersComicsDetails)))
    | GotComicCharacters (Result Http.Error (List RestComicCharacterDetails))
    | UserRequestsFurtherConnections



-- | GotComicResponse (RemoteData (Graphql.Http.Error (Maybe (List ComicDetails))) (Maybe (List ComicDetails)))


type Effect
    = LoadCharacterInfo
    | LoadComicCharacters



-- | LoadComicInfo CharactersComicsDetails


type alias WorkingComics =
    { characterId : Scalar.Id
    , comics : List ComicDetail
    }


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserUpdatedStartHero name ->
            ( { model | startHero = name }, Nothing )

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
                --         Just working ->
                --         Nothing -> Nothing
            in
            ( { model | workingComics = workingComics }, Nothing )

        GotComicCharacters result ->
            let
                _ =
                    Debug.log (Debug.toString result) 3
            in
            ( model, Nothing )

        UserRequestsFurtherConnections ->
            ( model, Just LoadComicCharacters )


allOrNothing : Maybe (List CharactersComicsDetails) -> Maybe WorkingComics
allOrNothing details =
    case ( pluckId details, pluckComics details ) of
        ( Just id, Just comics ) ->
            Just
                { comics = comics
                , characterId = id
                }

        ( _, _ ) ->
            Nothing


pluckId : Maybe (List CharactersComicsDetails) -> Maybe Scalar.Id
pluckId details =
    details
        |> Maybe.unwrap [] (List.map .id)
        |> List.map (Maybe.withDefault (Scalar.Id ""))
        -- I'm assuming that only one character was returned
        |> List.head


pluckComics : Maybe (List CharactersComicsDetails) -> Maybe (List ComicDetail)
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



-- GotComicResponse


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
            characterQuery model.startHero
                |> Graphql.Http.queryRequest "https://api.marvelql.com/"
                |> Graphql.Http.send (RemoteData.fromResult >> GotCharactersComicsDetails)

        LoadComicCharacters ->
            case model.workingComics of
                Just workingComics ->
                    comicQuery (Maybe.map .resource (List.head workingComics.comics))

                Nothing ->
                    Cmd.none



--- API


type alias SummaryData =
    { name : Maybe String
    , resourceUri : Maybe String
    }


type alias CharactersComicsDetails =
    { id : Maybe Scalar.Id

    -- , name : Maybe String
    , comics : Maybe (List SummaryData)
    }


type alias ComicsCharactersDetails =
    { id : Maybe Scalar.Id

    -- , name : Maybe String
    , characters : Maybe (List (Maybe String))
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


characterQuery : String -> SelectionSet (Maybe (List CharactersComicsDetails)) RootQuery
characterQuery name =
    let
        whereClause =
            buildCharacterWhereInput
                (\optionals ->
                    { optionals | name = Present "Spider-Man" }
                )
    in
    Query.characters
        (\optionals ->
            { optionals
                | where_ = Present whereClause
            }
        )
        (SelectionSet.map2 CharactersComicsDetails
            CharacterApi.id
            (CharacterApi.comics
                (SelectionSet.map2 SummaryData
                    Summary.name
                    Summary.resourceURI
                )
            )
        )


comicQuery : Maybe String -> Cmd Msg
comicQuery uri =
    case uri of
        Just url ->
            Http.get
                { url = url ++ "/characters?ts=1&apikey=***REMOVED***&hash=***REMOVED***"
                , expect = Http.expectJson GotComicCharacters comicCharactersDecoder
                }

        Nothing ->
            Cmd.none



-- in data, in results which is an array of
-- I want id, name, and comics with available and items with resource and name


type alias RestComicCharacterDetails =
    { id : Int
    , name : String
    , comics : ComicInfo
    }


type alias ComicInfo =
    { available : Int, items : List RestComicDetails }


type alias RestComicDetails =
    { resourceURI : String
    , name : String
    }


comicCharactersDecoder : Json.Decoder (List RestComicCharacterDetails)
comicCharactersDecoder =
    Json.field "data"
        (Json.field "results"
            (Json.list
                (Json.succeed RestComicCharacterDetails
                    |> required "id" Json.int
                    |> required "name" Json.string
                    |> required "comics"
                        (Json.succeed ComicInfo
                            |> required "available" Json.int
                            |> required "items"
                                (Json.list
                                    (Json.succeed RestComicDetails
                                        |> required "resourceURI" Json.string
                                        |> required "name" Json.string
                                    )
                                )
                        )
                )
            )
        )



--- View


view : Model -> NormHtml.Html Msg
view model =
    div []
        [ heroInput model.startHero
        , heroSubmitButton model.startHero
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
            conn.hero ++ " is in " ++ conn.comic.name ++ " with " ++ writeConnection conns


viewConnection : Maybe (List Connection) -> Html Msg
viewConnection connection =
    div [] [ text (Maybe.unwrap "No connection" writeConnection connection) ]


heroInput : String -> Html Msg
heroInput name =
    labelHidden
        "hero-name-input"
        []
        (text "Hero Name:")
        (inputText name [ onInput UserUpdatedStartHero ])


heroSubmitButton : String -> Html Msg
heroSubmitButton name =
    button
        [ onClick UserRequestsConnection ]
        [ text "connect the spider-man to squirrel girl" ]


comicLookupButton : Maybe WorkingComics -> Html Msg
comicLookupButton workingComics =
    button
        [ onClick UserRequestsFurtherConnections ]
        [ text "look at the spider's friends" ]



--- Program


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \flags -> perform init
        , update = \msg model -> perform (update msg model)
        , subscriptions = always Sub.none
        }
