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
	BookMark   LocationType = "bookmark"
)

var pattern = regexp.MustCompile(`\[\[([^\]]+)\](?:\[([^\]]+)\])?\]`)

type LocationApi interface {
	Create() error
	ParseLink() LocationApi
	Get() *storage.Location
	String() string
}

// orgLink format is [[protocol:link::exlink][desctipion]]
// protocol is the type of link, like ID, file, http, https, ftp, etc.
// link is the real link.
// :: is the separator between link and exlink, default is ::, but can be changed in different protocol.
// exlink is the external info to help user location the position after open link.
// description is the description of the link.
func ParseOrgLink(orgLink string, lotype LocationType) LocationApi {
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
		so := IDLocation{Location{Content: orgLink, Location: s}}
		return so.ParseLink()
	case "info":
		so := InfoLocation{Location{Content: orgLink, Location: s}}
		return so.ParseLink()
	default:
		return nil
	}
}

func ParseLocation(location *storage.Location) LocationApi {
	if location == nil {
		return nil
	}
	switch location.Protocol {
	case "ID":
		so := IDLocation{Location{Location: *location}}
		return &so
	case "info":
		so := InfoLocation{Location{Location: *location}}
		return &so
	default:
		so := Location{Location: *location}
		return &so
	}
}
