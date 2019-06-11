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
import Html as NormHtml
import Html.Styled.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe



---- MODEL ----


type alias Model =
    { connection : Maybe (List Connection)
    , startHero : String
    }



-- will also have a working graph but idk what that'll look like yet


type alias Connection =
    { hero : String
    , comic : String
    }


init : ( Model, Maybe Effect )
init =
    let
        startHero =
            "Spider-Man"
    in
    ( { startHero = startHero
      , connection = Just [ { hero = startHero, comic = "BFFs" } ]
      }
    , Nothing
    )



---- UPDATE ----


type Msg
    = UserUpdatedStartHero String
    | UserRequestsConnection


type Effect
    = LoadComics


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserUpdatedStartHero name ->
            ( { model | startHero = name }, Nothing )

        UserRequestsConnection ->
            ( model, Just LoadComics )


perform : ( Model, Maybe Effect ) -> ( Model, Cmd Msg )
perform ( model, effects ) =
    ( model
    , Maybe.map (runEffect model) effects
        |> Maybe.withDefault Cmd.none
    )


runEffect : Model -> Effect -> Cmd Msg
runEffect model effect =
    case effect of
        LoadComics ->
            Cmd.none



---- VIEW ----


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \flags -> perform init
        , update = \msg model -> perform (update msg model)
        , subscriptions = always Sub.none
        }
