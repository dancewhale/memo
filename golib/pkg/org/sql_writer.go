package org

import (
	"github.com/niklasfasching/go-org/org"
	"memo/pkg/storage"
	"regexp"
	"strings"
)

var exampleBlockUnescapeRegexp = regexp.MustCompile(`(^|\n)([ \t]*)(\*|,\*|#\+|,#\+)`)

var emphasisOrgBorders = map[string][]string{
	"_":   {"_", "_"},
	"*":   {"*", "*"},
	"/":   {"/", "/"},
	"+":   {"+", "+"},
	"~":   {"~", "~"},
	"=":   {"=", "="},
	"_{}": {"_{", "}"},
	"^{}": {"^{", "}"},
}

// SqlWriter an org document into database.
type SqlWriter struct {
	org.OrgWriter
	// 用于存储headline 结构体的堆栈
	s        storage.Stack
	Headline []*storage.Headline
	fileId   string
}

func NewSqlWriter() *SqlWriter {
	return &SqlWriter{
		OrgWriter: *org.NewOrgWriter(),
		s:         storage.NewStack(),
	}
}

func (w *SqlWriter) WriterWithExtensions() org.Writer {
	return w
}

func (w *SqlWriter) Before(d *org.Document) {}
func (w *SqlWriter) After(d *org.Document)  {}

func (w *SqlWriter) WriteNodesAsString(nodes ...org.Node) string {
	builder := w.Builder
	w.Builder = strings.Builder{}
	org.WriteNodes(w, nodes...)
	out := w.String()
	w.Builder = builder
	return out
}

func (w *SqlWriter) WriteHeadlineContentAsString(nodes ...org.Node) string {
	builder := w.Builder
	w.Builder = strings.Builder{}
	contentNode := Filter(nodes, func(n org.Node) bool {
		_, ok := n.(org.Headline)
		return !ok
	})
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
	note := getNoteIdType(h.Properties)
	var noteID *string
	if note.Orgid != "" {
		noteID = &note.Orgid
	} else {
		noteID = nil
	}
	headline := storage.Headline{Level: h.Lvl, Title: title, Status: h.Status,
		Content: content, Priority: h.Priority,
		Note:      note,
		FileRefer: w.fileId,
		OrgID:     noteID}
	// 深度优先遍历
	for {
		preHeadline, _ := w.s.Pop()
		if preHeadline != nil {
			if preHeadline.Level == headline.Level+1 {
				headline.Children = append(headline.Children, *preHeadline)
			} else {
				w.s.Push(preHeadline)
				break
			}
		} else {
			break
		}
	}

	w.s.Push(&headline)
	if h.Lvl == 1 {
		w.s.Pop()
		w.Headline = append(w.Headline, &headline)
	}
}
