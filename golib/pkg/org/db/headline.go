package db

import (
	"context"
	"github.com/emirpasic/gods/maps/linkedhashmap"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/gohugoio/hashstructure"
)

type Headline struct {
	Data     storage.Headline
	Children []Headline
	Hash     uint64
}

func (h *Headline) update() error {
	headline := dal.Use(storage.Engine).Headline
	_, err := headline.WithContext(context.Background()).Updates(&h.Data)
	if err != nil {
		return logger.Errorf("Insert headline %v error: %v", h.Data, err)
	}
	return nil
}

func (h *Headline) create() error {
	headline := dal.Use(storage.Engine).Headline
	err := headline.WithContext(context.Background()).Create(&h.Data)
	if err != nil {
		return logger.Errorf("Insert headline %v error: %v", h.Data, err)
	}
	return nil
}

func LoadHeadsByFileIDFromDB(fileID string) (*HeadsCache, error) {
	headsCache := HeadsCache{FileID: fileID, HeadlineMap: linkedhashmap.New()}
	if err := headsCache.loadHeadlinesFromDB(); err != nil {
		return nil, err
	}
	return &headsCache, nil
}

type HeadsCache struct {
	FileID      string
	HeadlineMap *linkedhashmap.Map // { hashid: Headline{} ... }
}

// Load all headline attach id from database.
func (h *HeadsCache) loadHeadlinesFromDB() error {
	headline := dal.Use(storage.Engine).Headline
	headlines, err := headline.WithContext(context.Background()).Where(headline.FileID.Eq(h.FileID)).Find()
	if err != nil {
		return logger.Errorf("Headlines load for file %s error: %v", h.FileID, err)
	}
	if len(headlines) != 0 {
		for _, headline := range headlines {
			hash, err := hashstructure.Hash(*headline, nil)
			if err != nil {
				return logger.Errorf("Hash headline struct error: %v", err)
			}
			head := Headline{Data: *headline, Hash: hash}
			h.HeadlineMap.Put(headline.ID, head)
		}
	}
	return nil
}

func (h *HeadsCache) UpdateHeadlineToDB(headFromFileCache *linkedhashmap.Map) error {
	it := headFromFileCache.Iterator()
	var err error
	for it.Begin(); it.Next(); {
		id, value := it.Key(), it.Value()
		head := value.(Headline)
		if headFromDB, ok := h.HeadlineMap.Get(id); ok {
			if headFromDB.(Headline).Hash != head.Hash {
				err = head.update()
			} else {
				continue
			}
		} else {
			err = head.create()
		}
	}
	return err
}
