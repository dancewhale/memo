package storage

import (
	"time"

	fsrs "github.com/open-spaced-repetition/go-fsrs"
)

type Note struct {
	ID              uint        `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	Content         string      `json:"Content"`
	Type            string      `json:"Type"`
	Orgid           string      `gorm:"unique;not null;index"`
	Hash            string      `json:"Hash"`
	Card            Card
	Logs            []ReviewLog
}

type Card struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	fsrs.Card       `gorm:"embedded"`
	NoteID	        uint
}

type ReviewLog struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	fsrs.ReviewLog  `gorm:"embedded",json:"Flog"`
	NoteID	        uint
}
