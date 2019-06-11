-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Object.ComicImage exposing (extension, path)

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


{-| A file path to the resources image
-}
path : SelectionSet (Maybe String) Marvelql.Object.ComicImage
path =
    Object.selectionForField "(Maybe String)" "path" [] (Decode.string |> Decode.nullable)


{-| The file extension for the resource image
-}
extension : SelectionSet (Maybe String) Marvelql.Object.ComicImage
extension =
    Object.selectionForField "(Maybe String)" "extension" [] (Decode.string |> Decode.nullable)
