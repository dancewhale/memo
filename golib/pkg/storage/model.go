package storage

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gorm"
	"time"
)

type Note struct {
	gorm.Model
	Content string      `json:"Content"`
	Type    string      `json:"Type"`
	Card    Card        `json:"Card"`
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
	ReviewLogs    []ReviewLog   `json:"ReviewLogs"`
	NoteID	      uint          `json:"NoteID"`
}

type ReviewLog struct {
	gorm.Model
	Rating        fsrs.Rating    `json:"Rating"`
	ScheduledDays uint64    `json:"ScheduledDays"`
	ElapsedDays   uint64    `json:"ElapsedDays"`
	Review        time.Time `json:"Review"`
	State         fsrs.State     `json:"State"`
	CardID        uint      `json:"CardID"`
}

