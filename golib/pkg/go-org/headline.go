package orgp

import (
	"regexp"
	"strings"
	"unicode"
)

type Outline struct {
	*Section
	last  *Section
	count int
}

type Section struct {
	Headline *Headline
	Parent   *Section
	Children []*Section
}

type Headline struct {
	Index        int
	Lvl          int
	Status       string
	Priority     string
	Properties   *PropertyDrawer
	LogBook      *LogBookDrawer
	TaskTime     TaskTime
	Title        []Node
	Tags         []string
	Children     []Node
	TitleContent string
	BodyContent  string
}

var headlineRegexp = regexp.MustCompile(`^([*]+)(\s+.*)`)
var tagRegexp = regexp.MustCompile(`(.*?)\s+(:[\p{L}0-9_@#%:]+:\s*$)`)

func lexHeadline(line string) (token, bool) {
	if m := headlineRegexp.FindStringSubmatch(line); m != nil {
		return token{"headline", m[2], m}, true
	}
	return nilToken, false
}

func (d *Document) parseHeadline(i int, parentStop stopFn) (int, Node) {
	t, headline := d.tokens[i], Headline{}

	headline.TitleContent = t.matches[0]
	headline.Lvl = len(t.matches[1])
	text := strings.TrimLeftFunc(t.content, unicode.IsSpace)

	todoKeywords := trimFastTags(
		strings.FieldsFunc(d.Get("TODO"), func(r rune) bool { return unicode.IsSpace(r) || r == '|' }),
	)
	for _, k := range todoKeywords {
		if strings.HasPrefix(text, k) && len(text) > len(k) && unicode.IsSpace(rune(text[len(k)])) {
			headline.Status = k
			text = text[len(k)+1:]
			break
		}
	}

	if len(text) >= 4 && text[0:2] == "[#" && strings.Contains("ABC", text[2:3]) && text[3] == ']' {
		headline.Priority = text[2:3]
		text = strings.TrimSpace(text[4:])
	}

	if m := tagRegexp.FindStringSubmatch(text); m != nil {
		text = m[1]
		headline.Tags = strings.FieldsFunc(m[2], func(r rune) bool { return r == ':' })
	}
	headline.Index = d.addHeadline(&headline)
	headline.Title = d.parseInline(text)

	stop := func(d *Document, i int) bool {
		return parentStop(d, i) || d.tokens[i].kind == "headline" && len(d.tokens[i].matches[1]) <= headline.Lvl
	}
	consumed, nodes := d.parseMany(i+1, stop)
	for i, node := range nodes {
		if d, ok := node.(PropertyDrawer); ok {
			headline.Properties = &d
			continue
		} else if t, ok := node.(TaskTime); ok {
			headline.TaskTime = t
			continue
		} else if l, ok := node.(LogBookDrawer); ok {
			headline.LogBook = &l
			continue
		} else {
			nodes = nodes[i:]
			break
		}
	}
	headline.Children = nodes
	for j := i + 1; j <= i+consumed; j++ {
		content := d.tokens[j].matches[0]
		headline.BodyContent += "\n" + content
	}
	return consumed + 1, headline
}

func trimFastTags(tags []string) []string {
	trimmedTags := make([]string, len(tags))
	for i, t := range tags {
		lParen := strings.LastIndex(t, "(")
		rParen := strings.LastIndex(t, ")")
		end := len(t) - 1
		if lParen == end-2 && rParen == end {
			trimmedTags[i] = t[:end-2]
		} else {
			trimmedTags[i] = t
		}
	}
	return trimmedTags
}

func (h Headline) ID() string {
	if ID, ok := h.Properties.Get("ID"); ok {
		return ID
	}
	return ""
}

func (parent *Section) add(current *Section) {
	if parent.Headline == nil || parent.Headline.Lvl < current.Headline.Lvl {
		parent.Children = append(parent.Children, current)
		current.Parent = parent
	} else {
		parent.Parent.add(current)
	}
}

func (h Headline) String() string {
	return h.TitleContent + h.BodyContent
}
