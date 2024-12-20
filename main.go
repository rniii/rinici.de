package main

import (
	"context"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/diamondburned/arikawa/v3/gateway"
	"github.com/diamondburned/arikawa/v3/session"
)

const chatURL = "http://localhost:45067/chat/history"
const relayID = 1316180922697257061

var chatSession string

func main() {
	chatSession = os.Getenv("CHATSESSION")
	token := os.Getenv("DISCORDTOKEN")

	s := session.New("Bot " + token)
	s.AddIntents(gateway.IntentGuildMessages)
	s.AddHandler(func(msg *gateway.MessageCreateEvent) {
		if msg.Author.Bot {
			return
		}

		if msg.ChannelID == relayID {
			proxyMessage(msg.Author.Username + ": " + msg.Content)
		}
	})

	if err := s.Open(context.Background()); err != nil {
		log.Fatalln(err)
	}
	defer s.Close()

	select {}
}

func proxyMessage(text string) error {
	req, _ := http.NewRequest(
		http.MethodPost, chatURL,
		strings.NewReader(url.Values{"text": {text}}.Encode()),
	)
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.AddCookie(&http.Cookie{Name: "chatSession", Value: chatSession})

	if _, err := http.DefaultClient.Do(req); err != nil {
		return err
	}

	return nil
}
