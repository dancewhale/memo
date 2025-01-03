package storage

import (
	"fmt"
	"gorm.io/gorm"
	"time"
)

type File struct {
	ID        string `gorm:"primaryKey;not null"`
	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt gorm.DeletedAt `gorm:"index"`
	FilePath  string         `gorm:"primaryKey;not null"`
	Hash      string
	Headlines []Headline
}

type Headline struct {
	ID        string         `gorm:"primarykey;not null"`
	CreatedAt time.Time      `hash:"ignore"`
	UpdatedAt time.Time      `hash:"ignore"`
	DeletedAt gorm.DeletedAt `gorm:"index" hash:"ignore"`

	// 标题
	Title string `json:"title"`
	// 内容
	Content string `json:"content"`
	// 卡片复习优先级
	Weight int64 `json:"weight"`
	// 父级ID
	ParentID *string `json:"parent_id"`
	// 层级
	Level int `json:"level"`
	// 同一层级下的排序
	Order int `json:"order"`
	// 任务状态
	Status    string     `json:"status"`
	Scheduled *time.Time `json:"scheduled"`
	Deadline  *time.Time `json:"deadline"`
	Closed    *time.Time `json:"closed"`
	// org 任务优先级
	Priority   string      `json:"priority"`
	Children   []Headline  `gorm:"foreignKey:ParentID" json:"children" hash:"ignore"`
	FileID     *string     `gorm:"primaryKey"`
	File       File        `gorm:"foreignKey:FileID;references:ID" json:"file" hash:"ignore"`
	Fsrs       FsrsInfo    `hash:"ignore"`
	ReviewLogs []ReviewLog `hash:"ignore"`
	LogBook    []*Clock    `gorm:"foreignKey:HeadlineID;references:ID" json:"logbook"`
}

type Clock struct {
	gorm.Model
	HeadlineID string   `gorm:"not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Start      *time.Time
	End        *time.Time
}

type Source struct {
	Link        string
	LinkType    string
	Description string
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
