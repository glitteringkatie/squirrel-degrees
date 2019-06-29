-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Object.ComicPrice exposing (price, type_)

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


{-| A description of the price (e.g. print price, digital price).
-}
type_ : SelectionSet (Maybe String) Marvelql.Object.ComicPrice
type_ =
    Object.selectionForField "(Maybe String)" "type" [] (Decode.string |> Decode.nullable)


{-| The price of the comic resource
-}
price : SelectionSet (Maybe Int) Marvelql.Object.ComicPrice
price =
    Object.selectionForField "(Maybe Int)" "price" [] (Decode.int |> Decode.nullable)