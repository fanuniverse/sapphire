# Sapphire

A Redis-based ordered autocomplete service.

## API

WIP

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
