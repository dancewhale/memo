package storage

import (
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
	// 类型
	Type *string `json:"type"`
	// 父级ID
	ParentID string `json:"parent_id"`
	// 层级
	Level int `json:"level"`
	// 同一层级下的排序
	Order int `json:"order"`
	// 任务状态
	Status string `json:"status"`
	// 优先级
	Priority   string      `json:"priority"`
	Children   []Headline  `gorm:"foreignKey:ParentID" json:"children" hash:"ignore"`
	FileID     string      `gorm:"primaryKey;not null"`
	File       File        `gorm:"foreignKey:FileID;references:ID" json:"file" hash:"ignore"`
	Fsrs       FsrsInfo    `hash:"ignore"`
	ReviewLogs []ReviewLog `hash:"ignore"`
}
