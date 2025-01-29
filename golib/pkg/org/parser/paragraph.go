package parser

import (
	"regexp"
	"strings"
)

type Paragraph struct {
	Children []Node
	Content  string
}

var plainTextRegexp = regexp.MustCompile(`^(.*)`)

func lexText(line string) (token, bool) {
	if m := plainTextRegexp.FindStringSubmatch(line); m != nil {
		return token{"text", m[1], m}, true
	}
	return nilToken, false
}

func (d *Document) parseParagraph(i int, parentStop stopFn) (int, Node) {
	var para = Paragraph{}
	lines, start := []string{d.tokens[i].content}, i
	stop := func(d *Document, i int) bool {
		return parentStop(d, i) || d.tokens[i].kind != "text" || d.tokens[i].content == ""
	}
	for i += 1; !stop(d, i); i++ {
		lines = append(lines, d.tokens[i].content)
	}
	consumed := i - start

	for j := start; j < start+consumed; j++ {
		content := d.tokens[j].matches[0]
		para.Content += "\n" + content
	}
	//if len(lines) == 1 && lines[0] == "" {
	//	para.Content = "\n"
	//}
	para.Children = d.parseInline(strings.Join(lines, "\n"))

	return consumed, para
}

func (p Paragraph) String() string {
	return p.Content
}
