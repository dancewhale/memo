package location

import (
	"memo/pkg/storage"
	"regexp"
	"strings"
)

type LocationType string

var (
	// mean the source of content copy from.
	SourceType LocationType = "source"
)

var pattern = regexp.MustCompile(`\[\[([^\]]+)\](?:\[([^\]]+)\])?\]`)

type LocationApi interface {
	Create() error
	ParseLink() LocationApi
	Get() storage.Location
	String() string
}

func ParseLocation(orgLink string, lotype LocationType) LocationApi {
	if orgLink == "" || lotype == "" {
		return nil
	}
	matches := pattern.FindStringSubmatch(orgLink)
	var link, linkUri, linkProtocol string
	if len(matches) > 1 {
		link = matches[1]
	}
	parts := strings.SplitN(link, ":", 2)
	if len(parts) == 2 {
		linkProtocol = parts[0]
		linkUri = parts[1]
	} else if len(parts) == 1 {
		linkProtocol = "file"
		linkUri = parts[0]
	}
	s := storage.Location{Link: linkUri, Protocol: linkProtocol, Type: string(lotype)}
	switch linkProtocol {
	case "ID":
		so := IDLocation{Location{content: orgLink, Location: s}}
		return so.ParseLink()
	default:
		so := Location{content: orgLink, Location: s}
		return so.ParseLink()
	}
}
