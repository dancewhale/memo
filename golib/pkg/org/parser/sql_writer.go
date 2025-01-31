package parser

import (
	"memo/pkg/org/db"
	"memo/pkg/storage"

	"memo/pkg/util/gods/lists/arraylist"
	"memo/pkg/util/gods/stacks/arraystack"
)

// SqlWriter an org document into database.
type SqlWriter struct {
	// 用于存储headline 结构体的堆栈
	stack     *arraystack.Stack
	Headlines []db.Headline
	fileId    string
}

func NewSqlWriter(id string) *SqlWriter {
	return &SqlWriter{
		stack:  arraystack.New(),
		fileId: id,
	}
}

func (s *SqlWriter) ParseNodes(nodes *arraylist.List) []db.Headline {
	it := nodes.Iterator()
	for it.Next() {
		n := it.Value()
		switch n := n.(type) {
		case Headline:
			s.ParseHeadline(n)
		default:
			continue
		}
	}
	return s.Headlines
}

// WriteHeadline 构建headline 结构体用于后续数据库存储。
func (s *SqlWriter) ParseHeadline(h Headline) {
	if h.Children != nil {
		s.ParseNodes(h.Children)
	}
	headline := db.Headline{
		Data: storage.Headline{Level: h.Lvl, Title: h.TitleContent, Status: h.Status,
			Content: h.BodyContent, Priority: h.Priority,
			FileID:    &s.fileId,
			Scheduled: h.TaskTime.GetScheduled(),
			Deadline:  h.TaskTime.GetDeadline(),
			Closed:    h.TaskTime.GetClosed(),
			LogBook:   GetLogBookFromDrawer(h.LogBook),
		},
		Children: []db.Headline{},
	}
	getHeadlineProperty(&headline, h.Properties)

	// 深度优先遍历
	for {
		preHeadline, _ := s.stack.Pop()
		if preHeadline != nil {
			ph := preHeadline.(db.Headline)
			if ph.Data.Level == headline.Data.Level+1 {
				ph.Data.ParentID = &headline.Data.ID
				headline.Children = append(headline.Children, ph)
			} else {
				s.stack.Push(preHeadline)
				break
			}
		} else {
			break
		}
	}

	if h.Lvl == 1 {
		headline.Data.Order = len(s.Headlines) + 1
	} else {
		headline.Data.Order = getHeadOrder(s.stack, headline)
	}

	s.stack.Push(headline)
	if h.Lvl == 1 {
		s.stack.Pop()
		s.Headlines = append(s.Headlines, headline)
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
