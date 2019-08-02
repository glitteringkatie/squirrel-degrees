module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)



-- { character : String
--     , comic : Comic
--     , parentId : Int
--     }


suite : Test
suite =
    describe "Stringify a connection"
        [ test "empty connection" <|
            \_ ->
                Expect.equal (Main.writeConnection [] "") "Squirrel Girl"
        , test "first level connection" <|
            \_ ->
                let
                    spidey =
                        { character = "Spider-Man"
                        , comic =
                            { name = "Cats"
                            , resource = "https://www.cats.com"
                            , parents = []
                            }
                        , parentId = 42
                        }
                in
                Expect.equal
                    (Main.writeConnection [ spidey ] "")
                    (spidey.character
                        ++ " is in "
                        ++ spidey.comic.name
                        ++ " with Squirrel Girl"
                    )
        , test "nth level connection" <|
            \_ ->
                let
                    chewie =
                        { character = "Chewie"
                        , comic =
                            { name = "Cats"
                            , resource = "https://www.cats.com"
                            , parents = []
                            }
                        , parentId = 42
                        }

                    spidey =
                        { character = "Spider-Man"
                        , comic =
                            { name = "BFFs"
                            , resource = "https://www.bffs.com"
                            , parents = []
                            }
                        , parentId = 42
                        }
                in
                Expect.equal
                    (Main.writeConnection [ chewie, spidey ] "")
                    (chewie.character
                        ++ " is in "
                        ++ chewie.comic.name
                        ++ " with "
                        ++ spidey.character
                        ++ " is in "
                        ++ spidey.comic.name
                        ++ " with Squirrel Girl"
                    )
        ]
