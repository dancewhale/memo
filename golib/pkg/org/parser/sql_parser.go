package parser

import (
	"memo/pkg/storage"

	"github.com/emirpasic/gods/lists/arraylist"
	"github.com/emirpasic/gods/stacks/arraystack"
)

// SqlWriter an org document into database.
type OrgParserSql struct {
	// 用于存储headline 结构体的堆栈
	stack     *arraystack.Stack
	Headlines []storage.Headline
	File      *storage.File
	fileType  int
}

func NewSqlWriter(fileType int) *OrgParserSql {
	return &OrgParserSql{
		stack:    arraystack.New(),
		File:     &storage.File{},
		fileType: fileType,
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
	s.File.Headlines = s.Headlines
	return s.File
}

func (s *OrgParserSql) ParseOrgHead(nodes *arraylist.List) []storage.Headline {
	it := nodes.Iterator()
	for it.Next() {
		n := it.Value()
		switch n := n.(type) {
		case Headline:
			s.parseHeadline(n)
		default:
			continue
		}
	}
	return s.Headlines
}

func (s *OrgParserSql) ParseFileMeta(node Node) {
	var id string
	exist := false
	switch n := node.(type) {
	case PropertyDrawer:
		id, exist = n.Get("ID")
	}
	if exist {
		s.File.ID = id
	}
	s.File.MetaContent += node.String()
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
		Scheduled: h.TaskTime.GetScheduled(),
		Deadline:  h.TaskTime.GetDeadline(),
		Closed:    h.TaskTime.GetClosed(),
		Children:  []storage.Headline{},
	}

	if s.fileType == storage.NormalFile {
		head.FileID = &s.File.ID
	} else if s.fileType == storage.VirtualFile {
		whitespace := ""
		head.HeadlineID = &s.File.ID
		head.FileID = &whitespace
	}

	parseHeadlineProperty(&head, h.Properties)

	head.Tags = getTags(h.Tags, head.ID)
	head.LogBook = GetLogBookFromDrawer(h.LogBook, head.ID)

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
func GetLogBookFromDrawer(logBook *LogBookDrawer, orgid string) []*storage.Clock {
	if logBook == nil {
		return nil
	} else if len(logBook.Clocks) == 0 {
		return nil
	}
	logs := make([]*storage.Clock, 0)
	for _, c := range logBook.Clocks {
		log := &storage.Clock{
			HeadlineID: orgid,
			Start:      c.GetStart(),
			End:        c.GetEnd(),
		}
		logs = append(logs, log)
	}
	return logs
}
