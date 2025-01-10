package location

import "strings"

type InfoLocation struct {
	Location
}

func (l *InfoLocation) ParseLink() LocationApi {
	parts := strings.SplitN(l.Link, "#", 2)
	if len(parts) == 2 {
		l.Link = parts[0]
		l.ExLink = parts[1]
	}
	return l
}

func (l *InfoLocation) String() string {
	return "[[" + l.Protocol + ":" + l.Location.Link + "#" + l.Location.ExLink + "]]"
}
