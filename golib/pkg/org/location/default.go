package location

import (
	"context"
	"memo/pkg/logger"
	"strings"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

type Location struct {
	storage.Location
	content string
}

func (l *Location) Create() error {
	lo := dal.Use(storage.Engine).Location
	// check if location is already created
	if l.Location.ID != 0 {
		return nil
	} else {
		found, err := lo.WithContext(context.Background()).Where(lo.Link.Eq(l.Link), lo.ExLink.Eq(l.ExLink)).
			Where(lo.Protocol.Eq(l.Protocol), lo.Type.Eq(l.Type)).Find()
		if err != nil {
			logger.Errorf("Find location get error %v", err.Error())
			return err
		}
		if len(found) == 0 {
			return lo.WithContext(context.Background()).Create(&l.Location)
		} else {
			l.Location.ID = found[0].ID
			return nil
		}
	}
}

func (l *Location) ParseLink() LocationApi {
	parts := strings.SplitN(l.Link, "::", 2)
	if len(parts) == 2 {
		l.Link = parts[0]
		l.ExLink = parts[1]
	}
	return l
}

func (l *Location) String() string {
	return "[[" + l.Protocol + ":" + l.Location.Link + "::" + l.Location.ExLink + "]]"
}

func (l *Location) Get() *storage.Location {
	l.Create()
	return &l.Location
}
