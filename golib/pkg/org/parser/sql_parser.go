package parser

import (
	"fmt"
	"memo/pkg/storage"

	"github.com/emirpasic/gods/lists/arraylist"
	"github.com/emirpasic/gods/stacks/arraystack"
)

// SqlWriter an org document into database.
type OrgParserSql struct {
	// 用于存储headline 结构体的堆栈
	stack     *arraystack.Stack
	Headlines []storage.Headline
	file      *storage.File
	fileId    string
}

func NewSqlWriter() *OrgParserSql {
	return &OrgParserSql{
		stack: arraystack.New(),
		file:  &storage.File{},
	}
}

func (s *OrgParserSql) ParseOrgFile(nodes *arraylist.List) *storage.File {
	it := nodes.Iterator()
	for it.Next() {
		n := it.Value()
		switch n := n.(type) {
		case Headline:
			s.parseHeadline(n)
		default:
			s.ParseFileMeta(n.(Node))
		}
	}
	s.file.Headlines = s.Headlines
	return s.file
}

func (s *OrgParserSql) ParseFileMeta(node Node) {
	var id string
	exist := false
	switch n := node.(type) {
	case PropertyDrawer:
		id, exist = n.Get("ID")
	}
	if exist {
		s.file.ID = id
	}
	s.file.MetaContent += node.String()
}

// WriteHeadline 构建headline 结构体用于后续数据库存储。
func (s *OrgParserSql) parseHeadline(h Headline) {
	if h.Children != nil {
		it := h.Children.Iterator()
		for it.Next() {
			n := it.Value()
			switch n := n.(type) {
			case Headline:
				s.parseHeadline(n)
			default:
				continue
			}
		}
	}
	head := storage.Headline{
		Level: h.Lvl, Title: h.Title, Status: h.Status,
		Content: h.BodyContent, Priority: h.Priority,
		FileID:    &s.file.ID,
		Scheduled: h.TaskTime.GetScheduled(),
		Deadline:  h.TaskTime.GetDeadline(),
		Closed:    h.TaskTime.GetClosed(),
		LogBook:   GetLogBookFromDrawer(h.LogBook),
		Children:  []storage.Headline{},
	}
	if h.ID() == "3A000187-0B2E-479B-96BC-9739ADC4EE7E" {
		fmt.Printf("head: %+v\n", head)
	}
	parseHeadlineProperty(&head, h.Properties)

	// 深度优先遍历
	for {
		preHeadline, _ := s.stack.Pop()
		if preHeadline != nil {
			ph := preHeadline.(storage.Headline)
			if ph.Level == head.Level+1 {
				ph.ParentID = &head.ID
				head.Children = append(head.Children, ph)
			} else {
				s.stack.Push(preHeadline)
				break
			}
		} else {
			break
		}
	}

	if h.Lvl == 1 {
		head.Order = len(s.Headlines) + 1
	} else {
		head.Order = getHeadOrder(s.stack, head)
	}

	s.stack.Push(head)
	if h.Lvl == 1 {
		s.stack.Pop()
		s.Headlines = append(s.Headlines, head)
	}
}

// change org.Logbook class to storage.Clock.
func GetLogBookFromDrawer(logBook *LogBookDrawer) []*storage.Clock {
	if logBook == nil {
		return nil
	} else if len(logBook.Clocks) == 0 {
		return nil
	}
	logs := make([]*storage.Clock, 0)
	for _, c := range logBook.Clocks {
		log := &storage.Clock{
			Start: c.GetStart(),
			End:   c.GetEnd(),
		}
		logs = append(logs, log)
	}
	return logs
}
