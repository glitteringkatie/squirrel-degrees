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

import Accessibility.Styled as Html exposing (Html, div, h1, img, inputText, labelHidden, text)
import Browser
import Html as NormHtml
import Html.Styled.Events exposing (onInput)
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


init : ( Model, Cmd Msg )
init =
    let
        startHero =
            "Spider-Man"
    in
    ( { startHero = startHero
      , connection = Just [ { hero = startHero, comic = "BFFs" } ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UserUpdatedStartHero String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserUpdatedStartHero name ->
            ( { model | startHero = name }, Cmd.none )



---- VIEW ----


view : Model -> NormHtml.Html Msg
view model =
    div []
        [ heroInput model.startHero
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
