package parser

import (
	"github.com/emirpasic/gods/lists/arraylist"
	"strings"
)

func NodesToString(nodes *arraylist.List) string {
	w := NewOrgWriter()
	it := nodes.Iterator()
	for it.Next() {
		n := it.Value()
		switch n := n.(type) {
		case PropertyDrawer:
			w.WritePropertyDrawer(n)
		case Paragraph:
			w.WriteParagraph(n)
		case Text:
			w.WriteText(n)
		case Headline:
			w.WriteHeadline(n)
		default:
			continue
		}
	}
	return w.String()
}

func NewOrgWriter() *OrgWriter {
	return &OrgWriter{}
}

type OrgWriter struct {
	strings.Builder
}

func (w *OrgWriter) WriteHeadline(h Headline) {
	w.WriteString(h.String())
	it := h.Children.Iterator()
	for it.Next() {
		head := it.Value()
		if h, ok := head.(Headline); ok {
			w.WriteHeadline(h)
		}
	}
}

func (w *OrgWriter) WriteText(t Text) {
	w.WriteString(t.String())
}

func (w *OrgWriter) WritePropertyDrawer(d PropertyDrawer) {
	w.WriteString(d.String())
}

func (w *OrgWriter) WriteParagraph(p Paragraph) {
	w.WriteString(p.String())
}
