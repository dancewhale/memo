package storage

import (
	fsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gorm"
	"time"
)

type Card struct {
	gorm.Model
	fsrs.Card
	Due           time.Time  `json:"Due"`
	Stability     float64    `json:"Stability"`
	Difficulty    float64    `json:"Difficulty"`
	ElapsedDays   uint64     `json:"ElapsedDays"`
	ScheduledDays uint64     `json:"ScheduledDays"`
	Reps          uint64     `json:"Reps"`
	Lapses        uint64     `json:"Lapses"`
	State         fsrs.State `json:"State"`
	LastReview    time.Time  `json:"LastReview"`
	ReviewLogs    []ReviewLog `json:"ReviewLogs"`
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

func (c *Card) Create(db *gorm.DB) error {
	return db.Debug().Create(c).Error
}

func (c *Card) UpdateName(db *gorm.DB) error {
	return db.Save(&c).Error
}

