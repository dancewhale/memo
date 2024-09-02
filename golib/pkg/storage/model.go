package storage

import (
	"time"

	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)


var QuestionType string = "Question"
var ClozeType    string = "Cloze"


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


const (
	WaitCardInit  int8 = iota    // review cards of note are init already 0.
	WaitReview                   // wait for card init can't review  1.
)

type Note struct {
	Orgid         string `gorm:"primaryKey;index"`
	CreatedAt     time.Time
	UpdatedAt     time.Time
	Content       string `json:"Content"`
	Type          string `json:"Type"`
	Hash          string `json:"Hash"`
	Fsrs          FsrsInfo
	ReviewLogs    []ReviewLog
}


type FsrsInfo struct {
	ID         uint `gorm:"primarykey"`
	CreatedAt  time.Time
	UpdatedAt  time.Time
	gfsrs.Card `gorm:"embedded"`
	NoteOrgid     string
}

type ReviewLog struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	gfsrs.ReviewLog `gorm:"embedded",json:"Flog"`
	NoteOrgid     string
}

func (fs *FsrsInfo) IsEmpty() bool {
	return *fs == FsrsInfo{}
}
