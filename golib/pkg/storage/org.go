package storage

import (
	"fmt"
	"time"

	"gorm.io/gorm"
)

const POSTPONE string = "postpone"
const SUSPEND string = "suspend"
const NORMAL string = "normal"

type File struct {
	ID          string `gorm:"primaryKey;not null"`
	CreatedAt   time.Time
	UpdatedAt   time.Time
	DeletedAt   gorm.DeletedAt `gorm:"index"`
	FilePath    string         `gorm:"primaryKey;not null"`
	Hash        string
	MetaContent string
	Headlines   []Headline
}

type Headline struct {
	ID        string         `gorm:"primarykey;not null"`
	CreatedAt time.Time      `hash:"ignore"`
	UpdatedAt time.Time      `hash:"ignore"`
	DeletedAt gorm.DeletedAt `gorm:"index" hash:"ignore"`
	Weight    int64          `json:"weight"`
	Source    string         `json:"source"`
	// Scheduled Type
	// Suspend: hang up card, not review until set to normal.
	// Normal: normal schedule.
	// Postphone: move card to the end of the queue in today and reset to 0 after review.
	ScheduledType string      `json:"scheduled_type"`
	Title         string      `json:"title"`
	Hash          string      `json:"hash" hash:"ignore"`
	Content       string      `json:"content"`
	ParentID      *string     `json:"parent_id"`
	Level         int         `json:"level"`
	Order         int         `json:"order"`
	Status        string      `json:"status"`
	Scheduled     *time.Time  `json:"scheduled"`
	Deadline      *time.Time  `json:"deadline"`
	Closed        *time.Time  `json:"closed"`
	Priority      string      `json:"priority"`
	Properties    []Property  `gorm:"foreignKey:HeadlineID;references:ID" json:"properties"`
	Children      []Headline  `gorm:"foreignKey:ParentID" json:"children" hash:"ignore"`
	FileID        *string     `gorm:"primaryKey"`
	File          File        `gorm:"foreignKey:FileID;references:ID" json:"file" hash:"ignore"`
	Fsrs          *FsrsInfo   `hash:"ignore"`
	ReviewLogs    []ReviewLog `hash:"ignore"`
	LogBook       []*Clock    `gorm:"foreignKey:HeadlineID;references:ID" json:"logbook"`
	Locations     []*Location `gorm:"many2many:headline_locations;" json:"locations"`
}

type Property struct {
	HeadlineID string   `gorm:"primarykey;not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Key        string   `gorm:"primarykey;not null"`
	Value      string   `gorm:"not null"`
}

type Clock struct {
	ID         uint     `gorm:"primarykey;not null" hash:"ignore"`
	HeadlineID string   `gorm:"not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Start      *time.Time
	End        *time.Time
}

type Location struct {
	ID       uint       `gorm:"primarykey"`
	Headline []Headline `gorm:"many2many:headline_locations;" json:"headline"`
	Protocol string
	Link     string
	ExLink   string
	Type     string
}

func (c Clock) String() string {
	start := "nil"
	end := "nil"
	if c.Start != nil {
		start = c.Start.Format("2006-01-02 15:04:05")
	}
	if c.End != nil {
		end = c.End.Format("2006-01-02 15:04:05")
	}
	return fmt.Sprintf("Clock{Start: %s, End: %s}", start, end)
}
