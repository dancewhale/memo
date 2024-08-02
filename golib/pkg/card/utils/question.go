package utils

import (
	"regexp"

	"memo/pkg/storage"
)


func QueNoteToCard(note *storage.Note) []storage.Card {
	re := regexp.MustCompile(`\n-+\n`)
	split := re.Split(note.Content, -1)
	if len(split) < 2 {
		return nil
	}
	card := storage.Card{
		Front: split[0],
		Back:  split[1],
		Type:  note.Type,
		NoteID: note.ID,
		Orgid: note.Orgid,
	}
	cards := []storage.Card{card}
	
	return cards
}
