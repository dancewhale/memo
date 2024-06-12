package storage

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gorm"
)

type Note struct {
	gorm.Model
	Content       string      `json:"Content",copier:"Content`
	Type          string      `json:"Type",copier:"Type`
	Orgid         string      `gorm:"unique,not null,index",copier:"Orgid`
	Hash          string      `json:"Hash",copier:"Hash"`
	Card          Card        `gorm:"foreignKey:NoteID",copier:"-"`
	Logs          []ReviewLog `gorm:"foreignKey:NoteID",json:"Logs"`
}

type Card struct {
	gorm.Model
	fcard         fsrs.Card    `gorm:"embedded"`
	NoteID	      uint         `json:"NoteID"`
}

type ReviewLog struct {
	gorm.Model
	flog	  fsrs.ReviewLog `gorm:"embedded"`
	NoteID	      uint          `json:"NoteID"`
}

