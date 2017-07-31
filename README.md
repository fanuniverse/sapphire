# Sapphire

A Redis-based ordered autocomplete service.

## API

### GET /search

This is a public endpoint that accepts a single query parameter, `q`,
and returns JSON-encoded search results.

| Example query | Status | Example response | Description |
| --- | --- | --- | --- |
| `/search?q=ta` | 200 | `[["tag", 5], ["second tag", 3]]` | search for a given query yielded several results (the response includes first ten, ordered by score) |
| `/search?q=sth` | 200 | `[]` | search for a given query yielded no results |
| `/search` | 400 | (empty) | no query was given |

### POST /update

This endpoint increments and decrements scores for given tags.

Tags with score less than or equal to zero are not
included in search results, and tags that were not present before
are created with an initial score of 1.

Accepted POST parameters are `add`, `remove`, or both. They should
contain JSON-encoded arrays of tag names; names can be repeated (in
that case, the score gets incremented/decremented several times).

The endpoint responds with `OK 200` in case of a successful update.

Example queries:

```bash
curl -XPOST -d 'add=["tag one", "tag two"]' localhost:3030/update
curl -XPOST -d 'remove=["tag one", "tag two"]' localhost:3030/update
curl -XPOST -d 'add=["tag three"]&remove=["tag four"]' localhost:3030/update
```

## Docker

Pull from [littlebobbytables/sapphire](https://hub.docker.com/r/littlebobbytables/sapphire/).

## Development

### Requirements

* [Docker CE](https://docker.com/community-edition#/download)
* [Docker Compose](https://docs.docker.com/compose/install/)
* [stack](https://github.com/commercialhaskell/stack)

### Building

```bash
stack docker pull
stack build
```

### Testing

```bash
docker-compose run
stack test
```
