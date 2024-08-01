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

type State int8

const (
	WaitReview State = iota  // wait for next review date of note
	Again                    // review again
	Hard                     // review hard
	Good                     // review good
	Easy                     // review easy
	ReviewCardsReady         // review cards of note are init already.
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
	ReviewState   State
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
}
