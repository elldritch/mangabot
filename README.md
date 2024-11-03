# MangaBot

MangaBot watches a subreddit for comments that mention a `[[Manga title in brackets]]` and replies with a link to the manga on MangaDex.

## Usage

```bash
$ cabal run mangabot -- --help
Usage: mangabot --client-id CLIENT_ID --client-secret CLIENT_SECRET
                --username USERNAME --password PASSWORD --subreddit SUBREDDIT
                --owner-username USERNAME [--dry-run]

  MangaBot

Available options:
  --client-id CLIENT_ID    OAuth2 client ID
  --client-secret CLIENT_SECRET
                           OAuth2 client secret
  --username USERNAME      Reddit bot username
  --password PASSWORD      Reddit bot password
  --subreddit SUBREDDIT    Subreddit to watch
  --owner-username USERNAME
                           Username of the bot operator
  --dry-run                Don't actually reply to comments
  -h,--help                Show this help text

```
