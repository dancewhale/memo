package storage

import (
	"time"

	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)

type Note struct {
	ID         uint `gorm:"primarykey"`
	CreatedAt  time.Time
	UpdatedAt  time.Time
	Content    string `json:"Content"`
	Type       string `json:"Type"`
	Orgid      string `gorm:"unique;not null;index"`
	Hash       string `json:"Hash"`
	Fsrs       FsrsInfo
	ReviewLogs []ReviewLog
	Cards      []Card // 等待review的卡片
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
	Type      string `json:"Type"`
	Front     string `json:"Front"`
	Back      string `json:"Back"`
	NoteID    uint
}
