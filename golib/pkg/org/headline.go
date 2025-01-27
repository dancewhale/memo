package org

import (
	"bytes"
	"errors"
	"fmt"
	"memo/pkg/org/db"

	"github.com/creker/hashstructure"
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"memo/pkg/logger"
)

// 用于加载从硬盘文件读取的 headline 数据，用于和数据库 headline 数据进行比较。
func NewHeadlineCache(headlines []db.Headline, fileID, filePath, hash string) (*HeadlineCacheMap, error) {
	var err error
	cache := HeadlineCacheMap{HeadlinesFileCache: linkedhashmap.New(), DuplicateID: []string{},
		HeadlinesDBCache: linkedhashmap.New(), fileID: fileID, filePath: filePath}

	err = cache.loadHeadFromFile(headlines)
	if err != nil {
		return nil, err
	}
	err = db.CheckFileDBRecord(fileID, filePath, hash)
	if err != nil {
		return nil, err
	}
	cache.HeadlinesDBCache, err = db.LoadHeadFromDB(fileID)
	if err != nil {
		return nil, err
	}
	if len(cache.DuplicateID) > 0 {
		return nil, errors.New(fmt.Sprintf("Found duplicate id: %v", cache.DuplicateID))
	}
	return &cache, nil
}

type HeadlineCacheMap struct {
	fileID             string
	filePath           string
	HeadlinesFileCache *linkedhashmap.Map
	HeadlinesDBCache   *linkedhashmap.Map
	DuplicateID        []string
	MissIDHeads        []string
}

// Scan for head without id or with duplicate id and prepare cache for compare.
func (h *HeadlineCacheMap) loadHeadFromFile(headlines []db.Headline) error {
	options := hashstructure.HashOptions{IgnoreZeroValue: true, ZeroNil: true}
	for _, head := range headlines {
		err := h.loadHeadFromFile(head.Children)
		if err != nil {
			return err
		}
		if _, ok := h.HeadlinesFileCache.Get(head.Data.ID); ok {
			h.DuplicateID = append(h.DuplicateID, head.Data.ID)
		} else {
			// compute headline struct hash.
			hash, err := hashstructure.Hash(head.Data, hashstructure.FormatV2, &options)
			if err != nil {
				return logger.Errorf("Hash headline error: %v", err)
			}
			head.Hash = hash
			if head.Data.ID != "" {
				h.HeadlinesFileCache.Put(head.Data.ID, head)
			}
		}
	}
	return nil
}

// 通过对比两个缓存，进行对比更新数据库中的headline
func (h *HeadlineCacheMap) UpdateHeadlineToDB(force bool) error {
	fileHeads := h.HeadlinesFileCache.Iterator()
	var err error
	for fileHeads.Begin(); fileHeads.Next(); {
		id, value := fileHeads.Key(), fileHeads.Value()
		headFromFile := value.(db.Headline)
		if headFromDB, ok := h.HeadlinesDBCache.Get(id); ok {
			if !bytes.Equal(headFromDB.(db.Headline).Hash, headFromFile.Hash) || force {
				err = headFromFile.UpdateByIDFile()
				h.HeadlinesDBCache.Remove(id)
			} else {
				h.HeadlinesDBCache.Remove(id)
				continue
			}
		} else {
			err = headFromFile.Create()
		}
		if err != nil {
			return err
		}
	}

	// Unattach the headline from fileid which still left in dbcache map.
	itDB := h.HeadlinesDBCache.Iterator()
	for itDB.Begin(); itDB.Next(); {
		_, head := itDB.Key(), itDB.Value()
		headDB := head.(db.Headline)
		err = headDB.UnattachFromFile()
		if err != nil {
			return err
		}
	}
	return err
}
