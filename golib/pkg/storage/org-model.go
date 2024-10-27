package storage

import (
	"errors"
	"sync"

	"gorm.io/gorm"
)

type Headline struct {
	gorm.Model
	// 标题
	Title string `json:"title"`
	// 内容
	Content string `json:"content"`
	// 父级ID
	ParentID int `json:"parent_id"`
	// 层级
	Level int `json:"level"`
	// 同一层级下的排序
	Order int `json:"order"`
	// 任务状态
	Status   string     `json:"status"`
	Children []Headline `gorm:"foreignKey:ParentID" json:"children"`
}

type Stack struct {
	lock sync.Mutex // you don't have to do this if you don't want thread safety
	s    []*Headline
}

func NewStack() Stack {
	return Stack{sync.Mutex{}, make([]*Headline, 0)}
}

func (s *Stack) Push(v *Headline) {
	s.lock.Lock()
	defer s.lock.Unlock()

	s.s = append(s.s, v)
}

func (s *Stack) Pop() (*Headline, error) {
	s.lock.Lock()
	defer s.lock.Unlock()

	l := len(s.s)
	if l == 0 {
		return nil, errors.New("Empty Stack")
	}

	res := s.s[l-1]
	s.s = s.s[:l-1]
	return res, nil
}
