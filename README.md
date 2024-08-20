# rinici.de

All Work and Derivative Works in Source or Object form are licensed per Apache-2.0. Unless stated
otherwise, all text, media and other content is licensed under the CC BY-SA 4.0 license.

## Development

<!-- maid-tasks -->

### generate

```sh
cabal run site
```

### generate-feed

```sh
find posts/*.md -exec yq '.url = ("/{}" | sub("/\d{4}-\d\d-\d\d-", "/") | sub("\.md$", ""))' -f extract '{}' ';' | \
  yq ea '{
    "entry": {
      "title": .title,
      "link": {"+@href": .url},
      "id": "https://rinici.de" + .url,
      "updated": .date | with_dtf("Jan 2, 2006"; format_datetime("2006-01-02T15:04:05Z07:00")),
      "content": {"+@src": .url, "+@type": "html"},
      "summary": .subtitle
    }
  }' -oxml | \
  yq '{
    "+p_xml": "version=\"1.0\" encoding=\"utf-8\"",
    "feed": {
      "+@xmlns": "http://www.w3.org/2005/Atom",
      "title": "rini blog",
      "link": [
        {"+@href": "http://localhost:8000"},
        {"+@href": "http://localhost:8000/posts/atom.xml", "+@rel": "self"}
      ],
      "icon": "/ico.png",
      "author": {"name": "rini c", "email": "rini@rinici.de", "uri": "https://rinici.de"},
      "rights": "© 2024 rini",
      "id": "https://rinici.de/",
      "updated": now | tz("UTC"),
      "entry": .entry
    }
  }' -pxml -oxml
```

### watch

```sh
maid -w . generate &
maid serve &
wait
```

### serve

```sh
cabal run server
```
