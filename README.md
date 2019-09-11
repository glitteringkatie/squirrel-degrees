# 6 Degrees of Squirrel Girl

## Why?

This is a parody of [6 Degrees of Kevin Bacon](https://en.wikipedia.org/wiki/Six_Degrees_of_Kevin_Bacon) using the Marvel universe with [Squirrel Girl](https://en.wikipedia.org/wiki/Squirrel_Girl) as our "Kevin Bacon". It was intended to explore [MarvelQL by Novvum](https://github.com/Novvum/MarvelQL) but ended up relying mostly on the original [Marvel REST API](https://developer.marvel.com/).

This project was created for my GraphQSquirrel talk at [elm-conf 2019](https://2019.elm-conf.com/speakers/katie-hughes/). You can find the slides and recording listed in my [presentations repo](https://github.com/glitteringkatie/presentations/blob/master/README.md).

## Development

### Setting up your api keys

1. Sign up for an account through the [Marvel developer portal](https://developer.marvel.com).

1. At the root of the project, run `cp src/Secrets.elm.template src/Secrets.elm` to duplicate the Secrets template file.

1. Open `src/Secrets.elm` and fill in your private and public api keys found in [your account details](https://developer.marvel.com/account). This Secrets.elm file is ignored in the .gitignore so no worries.

### Running the project

Make sure you've first followed [Setting up your api keys](#setting-up-your-api-keys).

1. If you don't already have elm installed, follow the install instructions from the [elm-lang guide](https://guide.elm-lang.org/install.html).

1. Install dependencies through `npm install`.

1. Generate the GraphQL client by running `npm run api:gen` from the root of the project.

1. Run `elm reactor`.

1. Navigate to `http://localhost:8000/src/Main.elm` to see the app running! Have fun!

### Running the tests

This project doesn't have enough tests because I was lazy but there are _some_ tests!

1. Install [elm-explorations/test](https://github.com/elm-explorations/test) globally by running `npm install -g elm-test`

2. Run `elm-test` or `npm run test`.

## Other Resources

- Find Squirrel Girl at your [local comic book store](https://www.comicshoplocator.com/).

- Find [Squirrel Girl Vol 1](https://www.indiebound.org/book/9780785197027) at your local indie bookstore.

- Find [Squirrel Girl Vol 1](https://www.worldcat.org/title/unbeatable-squirrel-girl-vol-01-squirrel-power) at your local public library--also remember that it is the 21st centruy and most libraries have a way to digitially checkout comics! Check with your local library!

## Two main issues
- Currently does not deal with paginating through API responses--would also require rethinking of when a degree has been completely searched since I currently use an empty PendingComics queue to mean it's time to step to the next degree
- Currently does not correctly fill in the working graph from the cache past the first degree--only fills in the first degree and thinks it is done. Probably need to just reload PendingComics appropriately but the first degree is handled by one function and the rest of the degrees are handled by another function (since one is from /comics and the other is from /comics/#{id}/character)
