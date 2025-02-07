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
	var consumed int
	var para = Paragraph{}
	lines, start := []string{d.tokens[i].content}, i
	if lines[0] == "" {
		para.Content = "\n"
		stop := func(d *Document, i int) bool {
			return parentStop(d, i) || d.tokens[i].kind != "text" || d.tokens[i].content != ""
		}
		for i += 1; !stop(d, i); i++ {
			para.Content += "\n"
		}
		consumed = i - start
	} else {
		stop := func(d *Document, i int) bool {
			return parentStop(d, i) || d.tokens[i].kind != "text" || d.tokens[i].content == ""
		}
		for i += 1; !stop(d, i); i++ {
			lines = append(lines, d.tokens[i].content)
		}
		para.Content = strings.Join(lines, "\n") + "\n"
		consumed = i - start
	}

	return consumed, para
}

func (p Paragraph) String() string {
	return p.Content
}
