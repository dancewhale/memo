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



type FCard struct {
	Due           time.Time `json:"Due"`
	Stability     float64   `json:"Stability"`
	Difficulty    float64   `json:"Difficulty"`
	ElapsedDays   uint64    `json:"ElapsedDays"`
	ScheduledDays uint64    `json:"ScheduledDays"`
	Reps          uint64    `json:"Reps"`
	Lapses        uint64    `json:"Lapses"`
	LastReview    time.Time `json:"LastReview"`
	State         fsrs.State     `json:"State"`
}
