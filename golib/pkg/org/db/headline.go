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
	headsCache := HeadsCache{FileID: fileID, HeadFromDBCache: linkedhashmap.New()}
	if err := headsCache.loadHeadlinesFromDB(); err != nil {
		return nil, err
	}
	return &headsCache, nil
}

type HeadsCache struct {
	FileID          string
	HeadFromDBCache *linkedhashmap.Map // { hashid: Headline{} ... }
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
			options := hashstructure.HashOptions{IgnoreZeroValue: true, ZeroNil: true}
			hash, err := hashstructure.Hash(*headline, &options)
			if err != nil {
				return logger.Errorf("Hash headline struct error: %v", err)
			}
			head := Headline{Data: *headline, Hash: hash}
			h.HeadFromDBCache.Put(headline.ID, head)
		}
	}
	return nil
}

// 通过对比两个缓存，进行对比更新数据库中的headline
func (h *HeadsCache) UpdateHeadlineToDB(headFromFileCache *linkedhashmap.Map) error {
	itFile := headFromFileCache.Iterator()
	var err error
	for itFile.Begin(); itFile.Next(); {
		id, value := itFile.Key(), itFile.Value()
		headFile := value.(Headline)
		if headFromDB, ok := h.HeadFromDBCache.Get(id); ok {
			if headFromDB.(Headline).Hash != headFile.Hash {
				err = headFile.update()
				h.HeadFromDBCache.Remove(id)
			} else {
				continue
			}
		} else {
			err = headFile.create()
		}
		if err != nil {
			return err
		}
	}

	// Unattach the headline from fileid which still left in dbcache map.
	itDB := h.HeadFromDBCache.Iterator()
	for itDB.Begin(); itDB.Next(); {
		_, head := itDB.Key(), itDB.Value()
		headDB := head.(Headline)
		headDB.Data.FileID = ""
		headDB.Data.ParentID = ""
		err = headDB.update()
		if err != nil {
			return err
		}
	}
	return err
}
