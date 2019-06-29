-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Object.TextObject exposing (language, text, type_)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Marvelql.InputObject
import Marvelql.Interface
import Marvelql.Object
import Marvelql.Scalar
import Marvelql.ScalarCodecs
import Marvelql.Union


{-| The canonical type of the text object (e.g. solicit text, preview text, etc.).
-}
type_ : SelectionSet (Maybe String) Marvelql.Object.TextObject
type_ =
    Object.selectionForField "(Maybe String)" "type" [] (Decode.string |> Decode.nullable)


{-| The IETF language tag denoting the language the text object is written in.
-}
language : SelectionSet (Maybe String) Marvelql.Object.TextObject
language =
    Object.selectionForField "(Maybe String)" "language" [] (Decode.string |> Decode.nullable)


{-| The text.
-}
text : SelectionSet (Maybe String) Marvelql.Object.TextObject
text =
    Object.selectionForField "(Maybe String)" "text" [] (Decode.string |> Decode.nullable)