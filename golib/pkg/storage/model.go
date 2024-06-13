package storage

import (
	"time"

	fsrs "github.com/open-spaced-repetition/go-fsrs"
)

type Note struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	Content         string      `json:"Content",copier:"Content`
	Type            string      `json:"Type",copier:"Type`
	Orgid           string      `gorm:"unique;not null;index",copier:"Orgid`
	Hash            string      `json:"Hash",copier:"Hash"`
	Card            Card        `json:"Card"`
	Logs            []ReviewLog `json:"Logs"`
}

type Card struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	NoteID	        uint         `json:"NoteID"`
	fsrs.Card       `json:"Fcard",gorm:"embedded"`
}

type ReviewLog struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	fsrs.ReviewLog      `json:"Flog",gorm:"embedded"`
	NoteID	        uint          `json:"NoteID"`
}

