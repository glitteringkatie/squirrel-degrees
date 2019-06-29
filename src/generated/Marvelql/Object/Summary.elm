-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Object.Summary exposing (name, resourceURI, role, type_)

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


{-| The canonical URL identifier for this summary resource.
-}
resourceURI : SelectionSet (Maybe String) Marvelql.Object.Summary
resourceURI =
    Object.selectionForField "(Maybe String)" "resourceURI" [] (Decode.string |> Decode.nullable)


name : SelectionSet (Maybe String) Marvelql.Object.Summary
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


role : SelectionSet (Maybe String) Marvelql.Object.Summary
role =
    Object.selectionForField "(Maybe String)" "role" [] (Decode.string |> Decode.nullable)


type_ : SelectionSet (Maybe String) Marvelql.Object.Summary
type_ =
    Object.selectionForField "(Maybe String)" "type" [] (Decode.string |> Decode.nullable)