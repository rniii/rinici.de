# rinici.de

All Work and Derivative Works in Source or Object form are licensed per Apache-2.0. Unless stated
otherwise, all text, media and other content is licensed under the CC BY-SA 4.0 license.

## Development

<!-- maid-tasks -->

### generate

```sh
cabal run -v0 site
```

### meta

```sh
find posts/*.md \
  -exec yq '.url = ("/{}" | sub("/\d{4}-\d\d-\d\d-", "/") | sub("\.md$", ""))' -f extract '{}' ';'
```

### generate-list

```sh
maid -q meta | \
  yq ea 'with_dtf("Jan 2 2006";
    [.] | sort_by(.date) | reverse[]
        | "<article><h3><a href=\"\(.url)\">\(.title)</a></h3><i>\(.subtitle)</i><br>\(.date)</article>")'
```

### generate-feed

```sh
maid -q meta | \
  yq ea -oxml '(
    { "+p_xml": "version=\"1.0\" encoding=\"utf-8\""
    , "feed":
      { "+@xmlns": "http://www.w3.org/2005/Atom"
      , "title": "rini blog"
      , "link":
        [ {"+@href": "http://rinici.de/"}
        , {"+@href": "http://rinici.de/posts/atom.xml", "+@rel": "self"}
        ]
      , "icon": "/ico.png"
      , "rights": "© 2024 rini"
      , "id": "https://rinici.de/"
      , "updated": now | tz("UTC")
      , "entry": [.] | sort_by(.date) | reverse[] |
        { "title": .title
        , "author": (.author | (select(type == "!!seq").0 // .) | {"name": .name, "uri": .url})
        , "link": {"+@href": .url}
        , "id": "https://rinici.de" + .url
        , "updated": .date | with_dtf("Jan 2, 2006"; format_datetime("2006-01-02T15:04:05Z07:00"))
        , "content": {"+@src": .url, "+@type": "html"}
        , "summary": .subtitle
        }
      }
    })'
```

### new-post

```sh
set -o noclobber
post=posts/$(yq -n 'now | format_datetime("2006-01-02")')-"$3".md
cat >$post <<EOF
---
title: ${3}
subtitle: Subtitle
$(yq "{\"author\":.$2}" authors.yml)
date: $(yq -n 'now | format_datetime("Jan 02, 2006")')
---
EOF
echo "Created $post"
```

### watch

```sh
maid -w . generate &
maid serve &
wait
```

### serve

```sh
cabal run -v0 server
```
