package storage

import (
	"time"

	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	"gorm.io/gorm"
)

// change string to fsrs.rate
func StringToRate(rate string) gfsrs.Rating {
	switch rate {
	case "Good":
		return gfsrs.Good
	case "Easy":
		return gfsrs.Easy
	case "Hard":
		return gfsrs.Hard
	case "Again":
		return gfsrs.Again
	default:
		return gfsrs.Again
	}
}

// change string to int8 for sqlite record.
func RateToInt(rate gfsrs.Rating) int8 {
	switch rate {
	case gfsrs.Good:
		return 4
	case gfsrs.Easy:
		return 3
	case gfsrs.Hard:
		return 2
	case gfsrs.Again:
		return 1
	default:
		return 1
	}
}

// change string to int8 for sqlite record.
func IntToRate(rate int8) gfsrs.Rating {
	switch rate {
	case 4:
		return gfsrs.Easy
	case 3:
		return gfsrs.Good
	case 2:
		return gfsrs.Hard
	case 1:
		return gfsrs.Again
	default:
		return gfsrs.Again
	}
}

type FsrsInfo struct {
	HeadlineID string `gorm:"primaryKey;index"`
	CreatedAt  time.Time
	UpdatedAt  time.Time
	DeletedAt  gorm.DeletedAt
	gfsrs.Card `gorm:"embedded"`
}

type ReviewLog struct {
	gorm.Model
	gfsrs.ReviewLog   `gorm:"embedded" json:"ReviewLog"`
	CardDue           time.Time   `json:"Due"`
	CardStability     float64     `json:"Stability"`
	CardDifficulty    float64     `json:"Difficulty"`
	CardElapsedDays   uint64      `json:"ElapsedDays"`
	CardScheduledDays uint64      `json:"ScheduledDays"`
	CardReps          uint64      `json:"Reps"`
	CardLapses        uint64      `json:"Lapses"`
	CardState         gfsrs.State `json:"State"`
	CardLastReview    time.Time   `json:"LastReview"`
	HeadlineID        string
}

func (log *ReviewLog) GetPreCard() gfsrs.Card {
	return gfsrs.Card{
		Due:           log.CardDue,
		Stability:     log.CardStability,
		Difficulty:    log.CardDifficulty,
		ElapsedDays:   log.CardElapsedDays,
		ScheduledDays: log.CardScheduledDays,
		Reps:          log.CardReps,
		Lapses:        log.CardLapses,
		State:         log.CardState,
		LastReview:    log.CardLastReview,
	}
}

func (log *ReviewLog) SetPreCard(card gfsrs.Card) {
	log.CardDue = card.Due
	log.CardStability = card.Stability
	log.CardDifficulty = card.Difficulty
	log.CardElapsedDays = card.ElapsedDays
	log.CardScheduledDays = card.ScheduledDays
	log.CardReps = card.Reps
	log.CardLapses = card.Lapses
	log.CardState = card.State
	log.CardLastReview = card.LastReview
}
