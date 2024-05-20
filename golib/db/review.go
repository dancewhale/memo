package db

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"github.com/spewerspew/spew"
	"time"
)

func Review() {
	card := fsrs.NewCard()
	spew.Dump(card)

	w := fsrs.DefaultParam()
	spew.Dump(w)
	now := time.Now()
	schedulingCards := w.Repeat(card, now)
	spew.Dump(schedulingCards)
}
