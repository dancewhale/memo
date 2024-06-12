package storage

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gorm"
	"time"
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
	Due           time.Time     `json:"Due"`
	Stability     float64       `json:"Stability"`
	Difficulty    float64       `json:"Difficulty"`
	ElapsedDays   uint64        `json:"ElapsedDays"`
	ScheduledDays uint64        `json:"ScheduledDays"`
	Reps          uint64        `json:"Reps"`
	Lapses        uint64        `json:"Lapses"`
	State         fsrs.State    `json:"State"`
	LastReview    time.Time     `json:"LastReview"`
	NoteID	      uint          `json:"NoteID"`
}

type ReviewLog struct {
	gorm.Model
	Rating        fsrs.Rating    `json:"Rating"`
	ScheduledDays uint64    `json:"ScheduledDays"`
	ElapsedDays   uint64    `json:"ElapsedDays"`
	Review        time.Time `json:"Review"`
	State         fsrs.State     `json:"State"`
	NoteID	      uint          `json:"NoteID"`
}

