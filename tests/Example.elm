module Example exposing (suite)

import Api
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Error(..))
import Main exposing (WorkingConnections(..))
import Result exposing (Result(..))
import Test exposing (..)



-- { character : String
--     , comic : Comic
--     , parentId : Int
--     }


defaultModel : Main.Model
defaultModel =
    { endCharacter = ""
    , workingConnections = NotAsked
    , pendingComics = Dict.empty
    , comicApiCache = Dict.empty
    , answersCache = Dict.empty
    }


spideyComic : Api.Comic
spideyComic =
    { name = "BFFs"
    , resource = comicUrl 3
    , parents = [ 42 ]
    }


spidey : Main.Connection
spidey =
    { character = "Spider-Man"
    , comic = spideyComic
    , parentId = 42
    }


workingSpidey : Main.WorkingConnection
workingSpidey =
    { character = "Spider-Man"
    , comic = spideyComic
    , parentId = 42
    , comics = []
    }


comicUrl : Int -> String
comicUrl id =
    "comics/" ++ String.fromInt id


suite : Test
suite =
    describe "test key main functions"
        [ describe "When stringifying a connection"
            [ test "that is empty, the result should be Squirrel Girl" <|
                \_ ->
                    Expect.equal (Main.writeConnection [] "") "Squirrel Girl"
            , test "that is the first level connection, write out the connection to Squirrel Girl" <|
                \_ ->
                    Expect.equal
                        (Main.writeConnection [ spidey ] "")
                        (spidey.character
                            ++ " is in "
                            ++ spidey.comic.name
                            ++ " with Squirrel Girl"
                        )
            , test "that is the nth level connection, write out the connections to Squirrel Girl" <|
                \_ ->
                    let
                        chewie =
                            { character = "Chewie"
                            , comic =
                                { name = "Cats"
                                , resource = comicUrl 2
                                , parents = []
                                }
                            , parentId = 3
                            }
                    in
                    Expect.equal
                        (Main.writeConnection [ spidey, chewie ] "")
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
        , describe "When shifting the queue"
            [ describe "and the api errored"
                [ test "and a solution has already been found, working connections should stay as the found solution" <|
                    \_ ->
                        let
                            model =
                                { defaultModel | workingConnections = FoundConnection [ spidey ] }

                            shiftedQueue =
                                Main.shiftQueue
                                    []
                                    spideyComic
                                    (Err NetworkError)
                                    model
                        in
                        Expect.equal
                            shiftedQueue.workingConnections
                            (FoundConnection [ spidey ])
                , test "and a solution has already been found to not exist, working connections should stay as the non-solution" <|
                    \_ ->
                        let
                            model =
                                { defaultModel | workingConnections = NoConnection }

                            shiftedQueue =
                                Main.shiftQueue
                                    []
                                    spideyComic
                                    (Err NetworkError)
                                    model
                        in
                        Expect.equal
                            shiftedQueue.workingConnections
                            NoConnection
                , test "working connections should reflect the error" <|
                    \_ ->
                        let
                            model =
                                { defaultModel | workingConnections = NotAsked }

                            shiftedQueue =
                                Main.shiftQueue
                                    []
                                    spideyComic
                                    (Err NetworkError)
                                    model
                        in
                        Expect.equal
                            shiftedQueue.workingConnections
                            (Error "Something went wrong!")
                ]
            , describe "and the api succeeded"
                [ test "with a match to the goal character, return an answer & update caches" <|
                    \_ ->
                        let
                            spId =
                                3

                            previousComic =
                                { name = "Spiders & Cats"
                                , resource = comicUrl 4
                                , parents = [ spId ]
                                }

                            pendingComics =
                                Dict.singleton 4 previousComic

                            workingSpideyWithComics =
                                { workingSpidey | comics = [ previousComic ] }

                            model =
                                { defaultModel
                                    | workingConnections =
                                        Asked
                                            [ Dict.empty
                                            , Dict.singleton spId workingSpideyWithComics
                                            ]
                                    , pendingComics = pendingComics
                                    , endCharacter = "Chewie"
                                }

                            apiComic =
                                { id = 2
                                , name = "Chewie"
                                , comics =
                                    { available = 1
                                    , items =
                                        [ { name = "Cats"
                                          , resource = comicUrl 2
                                          , parents = []
                                          }
                                        ]
                                    }
                                }

                            shiftedQueue =
                                Main.shiftQueue
                                    [ spId ]
                                    previousComic
                                    (Ok [ apiComic ])
                                    model
                        in
                        Expect.equal
                            shiftedQueue.workingConnections
                            (FoundConnection
                                [ { character = "Spider-Man"
                                  , comic =
                                        { name = "BFFs"
                                        , parents = [ 42 ]
                                        , resource = "comics/3"
                                        }
                                  , parentId = 42
                                  }
                                , { character = "Chewie"
                                  , comic =
                                        { name =
                                            "Spiders & Cats"
                                        , parents = [ 3 ]
                                        , resource = "comics/4"
                                        }
                                  , parentId = 3
                                  }
                                ]
                            )
                , test "add connection to working connection & update caches" <|
                    \_ ->
                        let
                            spId =
                                3

                            nextComic =
                                { name = "Spiders & Cats"
                                , resource = comicUrl 4
                                , parents = [ spId ]
                                }

                            pendingComics =
                                Dict.singleton spId spideyComic

                            model =
                                { defaultModel
                                    | workingConnections =
                                        Asked [ Dict.empty ]
                                    , pendingComics = pendingComics
                                    , endCharacter = "Chewie"
                                }

                            apiComic =
                                { id = 3
                                , name = "Spider-Man"
                                , comics =
                                    { available = 1
                                    , items = [ nextComic ]
                                    }
                                }

                            shiftedQueue =
                                Main.shiftQueue
                                    [ 42 ]
                                    spideyComic
                                    (Ok [ apiComic ])
                                    model
                        in
                        Expect.equal
                            shiftedQueue.workingConnections
                            (Asked
                                [ Dict.singleton
                                    3
                                    { character = "Spider-Man"
                                    , comic =
                                        { name = "BFFs"
                                        , parents = [ 42 ]
                                        , resource = "comics/3"
                                        }
                                    , parentId = 42
                                    , comics = [ nextComic ]
                                    }
                                ]
                            )
                ]
            ]
        ]
