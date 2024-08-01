# rinici.de

## Development

<!-- maid-tasks -->

### generate

```sh
cabal run site
```

### watch

```sh
watchexec -w Site.hs -w Pages.hs cabal run site
```

### serve

Run task `generate` before this

```sh
cabal run server
```
