package db

import (
	"bytes"
	"context"

	"github.com/creker/hashstructure"
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

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
			hash, err := hashstructure.Hash(*headline, hashstructure.FormatV2, &options)
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
func (h *HeadsCache) UpdateHeadlineToDB(headFromFileCache *linkedhashmap.Map, force bool) error {
	itFile := headFromFileCache.Iterator()
	var err error
	for itFile.Begin(); itFile.Next(); {
		id, value := itFile.Key(), itFile.Value()
		headFromFile := value.(Headline)
		if headFromDB, ok := h.HeadFromDBCache.Get(id); ok {
			if !bytes.Equal(headFromDB.(Headline).Hash, headFromFile.Hash) || force {
				err = headFromFile.updateByIDFile()
				h.HeadFromDBCache.Remove(id)
			} else {
				h.HeadFromDBCache.Remove(id)
				continue
			}
		} else {
			err = headFromFile.createOrUpdate()
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
		err = headDB.unattachFromFile()
		if err != nil {
			return err
		}
	}
	return err
}
