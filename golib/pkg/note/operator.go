package note

import (
	"memo/pkg/storage"
)

type NoteOperator interface{
	View() (string, string)
}



func NewOperator(note storage.Note) NoteOperator {
	if note.Type == "cloze" {
		return NewClozeNoteOperator(note)
	} else if note.Type == "question" {
		return NewQueNoteOperator(note)
	}
	return nil
}
