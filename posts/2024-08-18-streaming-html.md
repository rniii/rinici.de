---
title: The Cursed Art of Streaming HTML
subtitle: Or, how I wrote a live chat without JavaScript
author:
- name: rini
  url: https://rinici.de/
date: Aug 18, 2024
---

When I talk about [streaming HTML]{.keyword}, I am not talking about incrementally requesting HTML to
hydrate a page, or whatever the fancy thing is web frameworks do nowadays—no, I am talking about
streaming an actual HTML response, creating live updates much like a [WebSocket] (or actually just an
[SSE]) does, without the need for any JavaScript.

[WebSocket]: https://developer.mozilla.org/docs/WebSockets
[SSE]: https://developer.mozilla.org/docs/Server-sent_events

Turns out, it's really easy to do! Basically every single web browser (even ancient ones) will
request HTML with `Connection: keep-alive`, which means you get to be as slow as you want
responding! More realistically, you can use this to make sure the most important parts of your
webpage are sent as quickly as possible, while you fetch data for the rest. That's boring, though,
so we're going to go for something a bit more fun.

## Real-time chat, sans JS

::: info
For a live version of this, [check the home page!](/) The source code of this example can also be
found [here](https://gist.github.com/rniii/f29b822e8ef6d8017cbc870411d441e4).
:::

Here's a simple webpage to get started with. We'll be embedding the magic chat endpoint into an
iframe, and have a little form to send messages:

```html
<!doctype html>
<html lang="en">
  <h1>hello, chat!</h1>
  <iframe src="/chat/history" frameborder="0"></iframe>
  <form method="post" action="/chat/history">
    <input id="text" name="text" placeholder="Send a message...">
  </form>
</html>
```

Well... this doesn't actually work as intended, because sending a message causes the page to reload.
Does anyone even still use no-JS forms? It's such a pain. Anyways, we could avoid reloading the page
by setting a `target`, but lets go a step further and make the chatbox another iframe:

`index.html`:

```html
<iframe src="/chat/history" frameborder="0"></iframe> <hr>
<iframe src="/chat" frameborder="0"></iframe>
```

`chat.html`:

```html
<form method="post" action="/chat/history">
  <input id="text" name="text" placeholder="Send a message...">
</form>
```

This has the cursed bonus of automatically clearing the form, and now you wont lose chat history!

So, how do we stream HTML? It's no magic, it works exactly how you'd implement [SSEs][SSE] or
[WebSockets][WebSocket]. In Node frameworks, it's [`res.write()`][node-write], in Sinatra, it's
[`stream`][sinatra-stream], in Actix, it's [`HttpResponse::streaming()`][actix-stream], etc etc.

[node-write]: https://nodejs.org/api/http.html#responsewritechunk-encoding-callback
[sinatra-stream]: https://sinatrarb.com/intro.html#streaming-responses
[actix-stream]: https://actix.rs/docs/handlers#streaming-response-body

Here, we'll be using [Express], simply because it's probably the most universally known framework. I
actually have this website in Haskell, which if you're in to read some unholy code you can do so
[here](https://github.com/rniii/rinici.de). We'll also use an [`EventEmitter`][EventEmitter] to send
messages to clients. In a better language, this would be a proper broadcast channel, but oh well.

[Express]: http://expressjs.com
[EventEmitter]: https://nodejs.org/api/events.html

```js
const chat = new EventEmitter()

app.get("/chat/history", (req, res) => {
  res.set("Content-Type", "text/html")
  res.write("<!doctype html>")
  res.write("<meta charset=utf-8>")
  res.write("<body><ul>")

  chat.on("message", (text) => {
    res.write("<li>" + text)
  })
})
```

That's a resource leak. Okay, let's try:

```js
app.get("/chat/history", (req, res) => {
  // ...

  const listen = (text) => res.write("<li>" + text)

  chat.on("message", listen)
  res.on("close", () => chat.off("message", listen))
})
```

How to avoid XSS is left as an excercise for the reader. Now, to receive messages:

```js
app.use(express.urlencoded({ extended: true }))

app.post("/chat/history", (req, res) => {
  chat.emit("message", req.body.text)
  res.redirect("/chat#text")
})
```

And there's another neat trick: by redirecting to `/chat#text`, the textbox is automatically
focused. Add cache and you can't even tell the thing is an iframe!

## Conclusion

> *Oh my god the page doesn't finish loading*

If you actually try this code now, you'll see the page just... doesn't finish loading. Probably
because we don't actually close it. So, uh, I actually don't know how to fix this for now, if you do
figure it out, shoot me a DM or email!

- `loading="lazy"`. In theory should delay loading of the iframe, making it not count into page
load, but doesn't do anything, and wouldn't work with JS disabled.

My solution for now is to have a short snippet to trick the browser into loading the page. Sadly,
this means a little JS is necessary. But hey, it's actually fully functional without it, yay for
progressive-enhancement!

```html
<script>
chat.src = ""
setTimeout(() => chat.src = "/chat/history", 300)
</script>
```