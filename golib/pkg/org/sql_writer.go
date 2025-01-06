package org

import (
	"memo/pkg/org/db"
	"strings"

	"memo/pkg/storage"

	"github.com/dancewhale/go-org/org"
	"github.com/emirpasic/gods/stacks/arraystack"
)

// SqlWriter an org document into database.
type SqlWriter struct {
	org.OrgWriter
	// 用于存储headline 结构体的堆栈
	stack     *arraystack.Stack
	Headlines []db.Headline
	fileId    string
}

func NewSqlWriter() *SqlWriter {
	return &SqlWriter{
		OrgWriter: *org.NewOrgWriter(),
		stack:     arraystack.New(),
	}
}

func (w *SqlWriter) WriterWithExtensions() org.Writer {
	return w
}

func (w *SqlWriter) Before(d *org.Document) {}
func (w *SqlWriter) After(d *org.Document)  {}

func (w *SqlWriter) WriteHeadlineContentAsString(nodes ...org.Node) string {
	builder := w.Builder
	w.Builder = strings.Builder{}
	contentNode := Filter(nodes, FilterContentForHeadline)
	org.WriteNodes(w, contentNode...)
	out := w.String()
	w.Builder = builder
	return out
}

// WriteHeadline 构建headline 结构体用于后续数据库存储。
func (w *SqlWriter) WriteHeadline(h org.Headline) {
	org.WriteNodes(w, h.Children...)
	title := w.WriteNodesAsString(h.Title...)
	content := w.WriteHeadlineContentAsString(h.Children...)
	headline := db.Headline{
		Data: storage.Headline{Level: h.Lvl, Title: title, Status: h.Status,
			Content: content, Priority: h.Priority,
			FileID:    &w.fileId,
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
		preHeadline, _ := w.stack.Pop()
		if preHeadline != nil {
			ph := preHeadline.(db.Headline)
			if ph.Data.Level == headline.Data.Level+1 {
				ph.Data.ParentID = &headline.Data.ID
				headline.Children = append(headline.Children, ph)
			} else {
				w.stack.Push(preHeadline)
				break
			}
		} else {
			break
		}
	}

	if h.Lvl == 1 {
		headline.Data.Order = len(w.Headlines) + 1
	} else {
		headline.Data.Order = getHeadOrder(w.stack, headline)
	}

	w.stack.Push(headline)
	if h.Lvl == 1 {
		w.stack.Pop()
		w.Headlines = append(w.Headlines, headline)
	}

}

// change org.Logbook class to storage.Clock.
func GetLogBookFromDrawer(logBook *org.LogBookDrawer) []*storage.Clock {
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
