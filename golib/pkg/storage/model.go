package storage

import (
	"time"

	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)


var QuestionType string = "Question"
var ClozeType    string = "Cloze"


// change string to fsrs.rate
func Rate(rate string) gfsrs.Rating {
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
func RateInt(rate string) int8 {
	switch rate {
	case "Good":
		return Good
	case "Easy":
		return Easy
	case "Hard":
		return Hard
	case "Again":
		return Again
	default:
		return Again
	}
}

// change string to int8 for sqlite record.
func IntRate(rate int8) string {
	switch rate {
	case 4:
		return "Good"
	case 3:
		return "Easy"
	case 2:
		return "Hard"
	case 1:
		return "Again"
	default:
		return "Again"
	}
}


const (
	WaitReview int8 = iota  // wait for next review date of note  0
	Again                    // review again 1
	Hard                     // review hard  2
	Good                     // review good  3
	Easy                     // review easy  4
	ReviewCardsReady         // review cards of note are init already. 5
)

type Note struct {
	ID            uint `gorm:"primarykey"`
	CreatedAt     time.Time
	UpdatedAt     time.Time
	Content       string `json:"Content"`
	Type          string `json:"Type"`
	Orgid         string `gorm:"unique;not null;index"`
	Hash          string `json:"Hash"`
	Fsrs          FsrsInfo
	ReviewLogs    []ReviewLog
	Cards         []Card // 等待review的卡片
	ReviewState   int8
}


type FsrsInfo struct {
	ID         uint `gorm:"primarykey"`
	CreatedAt  time.Time
	UpdatedAt  time.Time
	gfsrs.Card `gorm:"embedded"`
	NoteID     uint
}

type ReviewLog struct {
	ID              uint `gorm:"primarykey"`
	CreatedAt       time.Time
	UpdatedAt       time.Time
	gfsrs.ReviewLog `gorm:"embedded",json:"Flog"`
	NoteID          uint
}

type Card struct {
	ID        uint `gorm:"primarykey"`
	CreatedAt time.Time
	UpdatedAt time.Time
	Type      string `json:"Type"`  // no use now
	Front     string `json:"Front"`
	Back      string `json:"Back"`
	NoteID    uint
	Orgid     string
}
