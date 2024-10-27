package org

import (
	"fmt"
	"regexp"
	"strings"
	"unicode"
	"unicode/utf8"

	"memo/pkg/storage"

	"github.com/niklasfasching/go-org/org"
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
	ExtendingWriter org.Writer
	TagsColumn      int
	indent          string
	// 用于存储headline 结构体的堆栈
	s        storage.Stack
	Headline []storage.Headline
	// 用于内部参数传递,兼容org.Writer接口,不新增函数参数和返回值.
	strings.Builder
}

func NewSqlWriter() *SqlWriter {
	return &SqlWriter{
		s: storage.NewStack(),
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
	org.WriteNodes(w, nodes...)
	out := w.String()
	w.Builder = builder
	return out
}

func (w *SqlWriter) WriteHeadline(h org.Headline) {
	org.WriteNodes(w, h.Children...)
	title := w.WriteNodesAsString(h.Title...)
	contentNode := Filter(h.Children, func(n org.Node) bool {
		_, ok := n.(org.Headline)
		return !ok
	})
	content := w.WriteNodesAsString(contentNode...)
	headline := storage.Headline{Level: h.Lvl, Title: title, Status: h.Status, Content: content}
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
		w.Headline = append(w.Headline, headline)
	}
}

func (w *SqlWriter) WriteBlock(b org.Block) {
	w.WriteString(w.indent + "#+BEGIN_" + b.Name)
	if len(b.Parameters) != 0 {
		w.WriteString(" " + strings.Join(b.Parameters, " "))
	}
	w.WriteString("\n")
	if isRawTextBlock(b.Name) {
		w.WriteString(w.indent)
	}
	content := w.WriteNodesAsString(b.Children...)
	if b.Name == "EXAMPLE" || (b.Name == "SRC" && len(b.Parameters) >= 1 && b.Parameters[0] == "org") {
		content = exampleBlockUnescapeRegexp.ReplaceAllString(content, "$1$2,$3")
	}
	w.WriteString(content)
	if !isRawTextBlock(b.Name) {
		w.WriteString(w.indent)
	}
	w.WriteString("#+END_" + b.Name + "\n")

	if b.Result != nil {
		w.WriteString("\n")
		org.WriteNodes(w, b.Result)
	}
}

func (w *SqlWriter) WriteLatexBlock(b org.LatexBlock) {
	w.WriteString(w.indent)
	org.WriteNodes(w, b.Content...)
	w.WriteString("\n")
}

func (w *SqlWriter) WriteResult(r org.Result) {
	w.WriteString("#+RESULTS:\n")
	org.WriteNodes(w, r.Node)
}

func (w *SqlWriter) WriteInlineBlock(b org.InlineBlock) {
	switch b.Name {
	case "src":
		w.WriteString(b.Name + "_" + b.Parameters[0])
		if len(b.Parameters) > 1 {
			w.WriteString("[" + strings.Join(b.Parameters[1:], " ") + "]")
		}
		w.WriteString("{")
		org.WriteNodes(w, b.Children...)
		w.WriteString("}")
	case "export":
		w.WriteString("@@" + b.Parameters[0] + ":")
		org.WriteNodes(w, b.Children...)
		w.WriteString("@@")
	}
}

func (w *SqlWriter) WriteDrawer(d org.Drawer) {
	w.WriteString(w.indent + ":" + d.Name + ":\n")
	org.WriteNodes(w, d.Children...)
	w.WriteString(w.indent + ":END:\n")
}

func (w *SqlWriter) WritePropertyDrawer(d org.PropertyDrawer) {
	w.WriteString(":PROPERTIES:\n")
	for _, kvPair := range d.Properties {
		k, v := kvPair[0], kvPair[1]
		if v != "" {
			v = " " + v
		}
		w.WriteString(fmt.Sprintf(":%s:%s\n", k, v))
	}
	w.WriteString(":END:\n")
}

func (w *SqlWriter) WriteFootnoteDefinition(f org.FootnoteDefinition) {
	w.WriteString(fmt.Sprintf("[fn:%s]", f.Name))
	content := w.WriteNodesAsString(f.Children...)
	if content != "" && !unicode.IsSpace(rune(content[0])) {
		w.WriteString(" ")
	}
	w.WriteString(content)
}

func (w *SqlWriter) WriteParagraph(p org.Paragraph) {
	content := w.WriteNodesAsString(p.Children...)
	if len(content) > 0 && content[0] != '\n' {
		w.WriteString(w.indent)
	}
	w.WriteString(content + "\n")
}

func (w *SqlWriter) WriteExample(e org.Example) {
	for _, n := range e.Children {
		w.WriteString(w.indent + ":")
		if content := w.WriteNodesAsString(n); content != "" {
			w.WriteString(" " + content)
		}
		w.WriteString("\n")
	}
}

func (w *SqlWriter) WriteKeyword(k org.Keyword) {
	w.WriteString(w.indent + "#+" + k.Key + ":")
	if k.Value != "" {
		w.WriteString(" " + k.Value)
	}
	w.WriteString("\n")
}

func (w *SqlWriter) WriteInclude(i org.Include) {
	w.WriteKeyword(i.Keyword)
}

func (w *SqlWriter) WriteNodeWithMeta(n org.NodeWithMeta) {
	for _, ns := range n.Meta.Caption {
		w.WriteString("#+CAPTION: ")
		org.WriteNodes(w, ns...)
		w.WriteString("\n")
	}
	for _, attributes := range n.Meta.HTMLAttributes {
		w.WriteString("#+ATTR_HTML: ")
		w.WriteString(strings.Join(attributes, " ") + "\n")
	}
	org.WriteNodes(w, n.Node)
}

func (w *SqlWriter) WriteNodeWithName(n org.NodeWithName) {
	w.WriteString(fmt.Sprintf("#+NAME: %s\n", n.Name))
	org.WriteNodes(w, n.Node)
}

func (w *SqlWriter) WriteComment(c org.Comment) {
	w.WriteString(w.indent + "# " + c.Content + "\n")
}

func (w *SqlWriter) WriteList(l org.List) { org.WriteNodes(w, l.Items...) }

func (w *SqlWriter) WriteListItem(li org.ListItem) {
	originalBuilder, originalIndent := w.Builder, w.indent
	w.Builder, w.indent = strings.Builder{}, w.indent+strings.Repeat(" ", len(li.Bullet)+1)
	org.WriteNodes(w, li.Children...)
	content := strings.TrimPrefix(w.String(), w.indent)
	w.Builder, w.indent = originalBuilder, originalIndent
	w.WriteString(w.indent + li.Bullet)
	if li.Value != "" {
		w.WriteString(fmt.Sprintf(" [@%s]", li.Value))
	}
	if li.Status != "" {
		w.WriteString(fmt.Sprintf(" [%s]", li.Status))
	}
	if len(content) > 0 && content[0] == '\n' {
		w.WriteString(content)
	} else {
		w.WriteString(" " + content)
	}
}

func (w *SqlWriter) WriteDescriptiveListItem(di org.DescriptiveListItem) {
	indent := w.indent + strings.Repeat(" ", len(di.Bullet)+1)
	w.WriteString(w.indent + di.Bullet)
	if di.Status != "" {
		w.WriteString(fmt.Sprintf(" [%s]", di.Status))
		indent = indent + strings.Repeat(" ", len(di.Status)+3)
	}
	if len(di.Term) != 0 {
		term := w.WriteNodesAsString(di.Term...)
		w.WriteString(" " + term + " ::")
		indent = indent + strings.Repeat(" ", len(term)+4)
	}
	originalBuilder, originalIndent := w.Builder, w.indent
	w.Builder, w.indent = strings.Builder{}, indent
	org.WriteNodes(w, di.Details...)
	details := strings.TrimPrefix(w.String(), w.indent)
	w.Builder, w.indent = originalBuilder, originalIndent
	if len(details) > 0 && details[0] == '\n' {
		w.WriteString(details)
	} else {
		w.WriteString(" " + details)
	}
}

func (w *SqlWriter) WriteTable(t org.Table) {
	for _, row := range t.Rows {
		w.WriteString(w.indent)
		if len(row.Columns) == 0 {
			w.WriteString(`|`)
			for i := 0; i < len(t.ColumnInfos); i++ {
				w.WriteString(strings.Repeat("-", t.ColumnInfos[i].Len+2))
				if i < len(t.ColumnInfos)-1 {
					w.WriteString("+")
				}
			}
			w.WriteString(`|`)

		} else {
			w.WriteString(`|`)
			for _, column := range row.Columns {
				w.WriteString(` `)
				content := w.WriteNodesAsString(column.Children...)
				if content == "" {
					content = " "
				}
				n := column.Len - utf8.RuneCountInString(content)
				if n < 0 {
					n = 0
				}
				if column.Align == "center" {
					if n%2 != 0 {
						w.WriteString(" ")
					}
					w.WriteString(strings.Repeat(" ", n/2) + content + strings.Repeat(" ", n/2))
				} else if column.Align == "right" {
					w.WriteString(strings.Repeat(" ", n) + content)
				} else {
					w.WriteString(content + strings.Repeat(" ", n))
				}
				w.WriteString(` |`)
			}
		}
		w.WriteString("\n")
	}
}

func (w *SqlWriter) WriteHorizontalRule(hr org.HorizontalRule) {
	w.WriteString(w.indent + "-----\n")
}

func (w *SqlWriter) WriteText(t org.Text) { w.WriteString(t.Content) }

func (w *SqlWriter) WriteEmphasis(e org.Emphasis) {
	borders, ok := emphasisOrgBorders[e.Kind]
	if !ok {
		panic(fmt.Sprintf("bad emphasis %#v", e))
	}
	w.WriteString(borders[0])
	org.WriteNodes(w, e.Content...)
	w.WriteString(borders[1])
}

func (w *SqlWriter) WriteLatexFragment(l org.LatexFragment) {
	w.WriteString(l.OpeningPair)
	org.WriteNodes(w, l.Content...)
	w.WriteString(l.ClosingPair)
}

func (w *SqlWriter) WriteStatisticToken(s org.StatisticToken) {
	w.WriteString(fmt.Sprintf("[%s]", s.Content))
}

func (w *SqlWriter) WriteLineBreak(l org.LineBreak) {
	w.WriteString(strings.Repeat("\n"+w.indent, l.Count))
}

func (w *SqlWriter) WriteExplicitLineBreak(l org.ExplicitLineBreak) {
	w.WriteString(`\\` + "\n" + w.indent)
}

func (w *SqlWriter) WriteTimestamp(t org.Timestamp) {
	w.WriteString("<")
	if t.IsDate {
		w.WriteString(t.Time.Format(datestampFormat))
	} else {
		w.WriteString(t.Time.Format(timestampFormat))
	}
	if t.Interval != "" {
		w.WriteString(" " + t.Interval)
	}
	w.WriteString(">")
}

func (w *SqlWriter) WriteFootnoteLink(l org.FootnoteLink) {
	w.WriteString("[fn:" + l.Name)
	if l.Definition != nil {
		w.WriteString(":")
		org.WriteNodes(w, l.Definition.Children[0].(org.Paragraph).Children...)
	}
	w.WriteString("]")
}

func (w *SqlWriter) WriteRegularLink(l org.RegularLink) {
	if l.AutoLink {
		w.WriteString(l.URL)
	} else if l.Description == nil {
		w.WriteString(fmt.Sprintf("[[%s]]", l.URL))
	} else {
		w.WriteString(fmt.Sprintf("[[%s][%s]]", l.URL, w.WriteNodesAsString(l.Description...)))
	}
}

func (w *SqlWriter) WriteMacro(m org.Macro) {
	w.WriteString(fmt.Sprintf("{{{%s(%s)}}}", m.Name, strings.Join(m.Parameters, ",")))
}
