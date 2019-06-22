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
import Marvelql.InputObject exposing (buildCharacterWhereInput)
import Marvelql.Object exposing (Character(..))
import Marvelql.Object.Character as CharacterApi
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
    , workingComics : Maybe CharactersComicsDetails
    , startHero : String
    }



-- will also have a working graph but idk what that'll look like yet


type alias Connection =
    { hero : String
    , comic : String
    , parentId : Maybe Marvelql.ScalarCodecs.Id
    }


init : ( Model, Maybe Effect )
init =
    let
        startHero =
            "Spider-Man"
    in
    ( { startHero = startHero
      , connection = Just [ { hero = startHero, comic = "BFFs", parentId = Nothing } ]
      , workingConnections1 = Nothing
      , workingConnections2 = Nothing
      , workingConnections3 = Nothing
      , workingConnections4 = Nothing
      , workingConnections5 = Nothing
      , workingConnections6 = Nothing
      , workingComics = Nothing
      }
    , Nothing
    )



--- Update


type Msg
    = UserUpdatedStartHero String
    | UserRequestsConnection
    | GotCharactersComicsDetails (RemoteData (Graphql.Http.Error (Maybe (List CharactersComicsDetails))) (Maybe (List CharactersComicsDetails)))



-- | GotComicResponse (RemoteData (Graphql.Http.Error (Maybe (List ComicDetails))) (Maybe (List ComicDetails)))


type Effect
    = LoadCharacterInfo



-- | LoadComicInfo CharactersComicsDetails


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserUpdatedStartHero name ->
            ( { model | startHero = name }, Nothing )

        UserRequestsConnection ->
            ( model, Just LoadCharacterInfo )

        GotCharactersComicsDetails maybeDetails ->
            let
                -- TODO clean up so Nothing is decided in one place
                -- TODO Make a type that lacks maybes
                comicNames =
                    case maybeDetails of
                        RemoteData.Success details ->
                            details
                                |> Maybe.unwrap [] (List.map .comics)
                                |> List.concatMap (Maybe.withDefault [])
                                |> Just

                        -- |> List.map (Maybe.withDefault "")
                        -- |> List.filter (\s -> not (String.isEmpty s))
                        _ ->
                            Nothing

                workingComics =
                    case maybeDetails of
                        RemoteData.Success details ->
                            let
                                id =
                                    details
                                        |> Maybe.unwrap [] (List.map .id)
                                        |> List.map (Maybe.withDefault (Scalar.Id ""))
                                        |> List.head

                                _ =
                                    Debug.log (Debug.toString id) 3
                            in
                            Just
                                { comics = comicNames
                                , id = id
                                }

                        _ ->
                            Nothing

                _ =
                    Debug.log (Debug.toString workingComics) 3
            in
            ( { model | workingComics = workingComics }, Nothing )



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



--- GraphQL


type alias CharactersComicsDetails =
    { id : Maybe Scalar.Id

    -- , name : Maybe String
    , comics : Maybe (List (Maybe String))
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
            (CharacterApi.comics Summary.name)
        )



--- View


view : Model -> NormHtml.Html Msg
view model =
    div []
        [ heroInput model.startHero
        , heroSubmitButton model.startHero
        , viewConnection model.connection
        ]
        |> Html.toUnstyled


writeConnection : List Connection -> String
writeConnection connection =
    case connection of
        [] ->
            "Squirrel Girl"

        conn :: conns ->
            conn.hero ++ " is in " ++ conn.comic ++ " with " ++ writeConnection conns


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



--- Program


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \flags -> perform init
        , update = \msg model -> perform (update msg model)
        , subscriptions = always Sub.none
        }
