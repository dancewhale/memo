package note

import (
	"memo/pkg/storage"
)

type QueNoteOperator struct {
	note storage.Note	
}

func NewQueNoteOperator(note storage.Note) *QueNoteOperator {
	return &QueNoteOperator{note: note}
}

func (c *QueNoteOperator) View() (string, string){
	return "", ""
}
