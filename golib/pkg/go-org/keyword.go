package orgp

import (
	"regexp"
)

type Keyword struct {
	Key   string
	Value string
}

type Include struct {
	Keyword
	Resolve func() Node
}

var keywordRegexp = regexp.MustCompile(`^(\s*)#\+([^:]+):(\s+(.*)|$)`)
var commentRegexp = regexp.MustCompile(`^(\s*)#\s(.*)`)

func lexKeywordOrComment(line string) (token, bool) {
	if m := keywordRegexp.FindStringSubmatch(line); m != nil {
		return token{"keyword", m[2], m}, true
	} else if m := commentRegexp.FindStringSubmatch(line); m != nil {
		return token{"comment", m[2], m}, true
	}
	return nilToken, false
}
