package orgp

import (
	"strings"
)

type Text struct {
	Content string
	IsRaw   bool
}

type RegularLink struct {
	Protocol    string
	Description []Node
	URL         string
	Content     string
}

func (d *Document) parseInline(input string) (nodes []Node) {
	previous, current := 0, 0
	for current < len(input) {
		rewind, consumed, node := 0, 0, (Node)(nil)
		switch input[current] {
		case '[':
			consumed, node = d.parseRegularLink(input, current)
		}
		current -= rewind
		if consumed != 0 {
			if current > previous {
				nodes = append(nodes, Text{input[previous:current], false})
			}
			if node != nil {
				nodes = append(nodes, node)
			}
			current += consumed
			previous = current
		} else {
			current++
		}
	}

	if previous < len(input) {
		nodes = append(nodes, Text{input[previous:], false})
	}
	return nodes
}

func (d *Document) parseRegularLink(input string, start int) (int, Node) {
	input = input[start:]
	if len(input) < 3 || input[:2] != "[[" || input[2] == '[' {
		return 0, nil
	}
	end := strings.Index(input, "]]")
	if end == -1 {
		return 0, nil
	}
	rawLinkParts := strings.Split(input[2:end], "][")
	description, link := ([]Node)(nil), rawLinkParts[0]
	if len(rawLinkParts) == 2 {
		link, description = rawLinkParts[0], d.parseInline(rawLinkParts[1])
	}
	if strings.ContainsRune(link, '\n') {
		return 0, nil
	}
	consumed := end + 2
	protocol, linkParts := "", strings.SplitN(link, ":", 2)
	if len(linkParts) == 2 {
		protocol = linkParts[0]
	}
	return consumed, RegularLink{protocol, description, link, input[:consumed]}
}

func (t Text) String() string {
	return t.Content
}

func (l RegularLink) String() string {
	return l.Content
}
