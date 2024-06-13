package storage

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gorm"
)

type Note struct {
	gorm.Model
	Content       string      `json:"Content",copier:"Content`
	Type          string      `json:"Type",copier:"Type`
	Orgid         string      `gorm:"unique;not null;index",copier:"Orgid`
	Hash          string      `json:"Hash",copier:"Hash"`
	Card          Card        `json:"Card"`
	Logs          []ReviewLog `json:"Logs"`
}

type Card struct {
	gorm.Model
	fsrs.Card    `json:"Fcard",gorm:"embedded"`
	NoteID	      uint         `json:"NoteID"`
}

type ReviewLog struct {
	gorm.Model
	fsrs.ReviewLog    `json:"Flog",gorm:"embedded"`
	NoteID	      uint          `json:"NoteID"`
}

