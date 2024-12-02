package storage

import (
	"gorm.io/gorm"
)

type File struct {
	ID       string `gorm:"primaryKey;unique;not null"`
	FilePath string
	Hash     string
}

type Headline struct {
	gorm.Model

	// 标题
	Title string `json:"title"`
	// 内容
	Content string `json:"content"`
	// 类型
	Type *string `json:"type"`
	// 父级ID
	ParentID int `json:"parent_id"`
	// 层级
	Level int `json:"level"`
	// 同一层级下的排序
	Order int `json:"order"`
	// 任务状态
	Status string `json:"status"`
	// 优先级
	Priority string     `json:"priority"`
	Children []Headline `gorm:"foreignKey:ParentID" json:"children"`
	FileID   string
	File     File `gorm:"foreignKey:FileID;references:ID" json:"file"`
	// note 引用包含orgid和type
	OrgID *string
	Card  Card `gorm:"foreignKey:OrgID;references:Orgid" json:"-"`
}
