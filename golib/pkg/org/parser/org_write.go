package parser

import (
	"strings"
)

func NodesToString(nodes ...Node) string {
	w := NewOrgWriter()
	for i, n := range nodes {
		if i != 0 {
			w.WriteString("\n")
		}
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
}

func (w *OrgWriter) WriteText(t Text) {
	w.WriteString(t.Content)
}

func (w *OrgWriter) WritePropertyDrawer(d PropertyDrawer) {
	w.WriteString(d.Content)
}

func (w *OrgWriter) WriteParagraph(p Paragraph) {
	w.WriteString(p.Content)
}
