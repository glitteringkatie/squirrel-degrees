-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Marvelql.Object.Character exposing (comics, description, events, id, modified, name, resourceURI, series, stories, thumbnail, urls)

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


{-| A unique ID to a particular marvel resource.
-}
id : SelectionSet (Maybe Marvelql.ScalarCodecs.Id) Marvelql.Object.Character
id =
    Object.selectionForField "(Maybe ScalarCodecs.Id)" "id" [] (Marvelql.ScalarCodecs.codecs |> Marvelql.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


{-| The canonical URL identifier for a resource.
-}
resourceURI : SelectionSet (Maybe String) Marvelql.Object.Character
resourceURI =
    Object.selectionForField "(Maybe String)" "resourceURI" [] (Decode.string |> Decode.nullable)


{-| The url path for a canonical photo to the resource
-}
thumbnail : SelectionSet (Maybe String) Marvelql.Object.Character
thumbnail =
    Object.selectionForField "(Maybe String)" "thumbnail" [] (Decode.string |> Decode.nullable)


{-| A date for which the resource has been modified
-}
modified : SelectionSet (Maybe String) Marvelql.Object.Character
modified =
    Object.selectionForField "(Maybe String)" "modified" [] (Decode.string |> Decode.nullable)


{-| The name of the character.
-}
name : SelectionSet (Maybe String) Marvelql.Object.Character
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


{-| A short bio or description of the character.
-}
description : SelectionSet (Maybe String) Marvelql.Object.Character
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| A set of public web site URLs for the resource.
-}
urls : SelectionSet decodesTo Marvelql.Object.MarvelUrl -> SelectionSet (Maybe (List decodesTo)) Marvelql.Object.Character
urls object_ =
    Object.selectionForCompositeField "urls" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| Lists of comics filtered by a character id.
-}
comics : SelectionSet decodesTo Marvelql.Object.Summary -> SelectionSet (Maybe (List decodesTo)) Marvelql.Object.Character
comics object_ =
    Object.selectionForCompositeField "comics" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| Lists of series filtered by a character id.
-}
series : SelectionSet decodesTo Marvelql.Object.Summary -> SelectionSet (Maybe (List decodesTo)) Marvelql.Object.Character
series object_ =
    Object.selectionForCompositeField "series" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| Lists of events filtered by a character id.
-}
events : SelectionSet decodesTo Marvelql.Object.Summary -> SelectionSet (Maybe (List decodesTo)) Marvelql.Object.Character
events object_ =
    Object.selectionForCompositeField "events" [] object_ (identity >> Decode.list >> Decode.nullable)


{-| Lists of stories filtered by a character id.
-}
stories : SelectionSet decodesTo Marvelql.Object.Summary -> SelectionSet (Maybe (List decodesTo)) Marvelql.Object.Character
stories object_ =
    Object.selectionForCompositeField "stories" [] object_ (identity >> Decode.list >> Decode.nullable)