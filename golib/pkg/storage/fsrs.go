package storage

import (
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	"gorm.io/gorm"
	"time"
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
	HeadlineID string `gorm:"primaryKey"`
	CreatedAt  time.Time
	UpdatedAt  time.Time
	DeletedAt  gorm.DeletedAt `gorm:"index"`
	gfsrs.Card `gorm:"embedded"`
}

type ReviewLog struct {
	gorm.Model
	gfsrs.ReviewLog `gorm:"embedded",json:"Flog"`
	HeadlineID      string
}
