package note

import (
	"memo/pkg/storage"
)

type ClozeNoteOperator struct {
	note storage.Note	
}

func NewClozeNoteOperator(note storage.Note) *ClozeNoteOperator {
	return &ClozeNoteOperator{note: note}
}

func (c *ClozeNoteOperator) View() (string, string){
	return "", ""
}
