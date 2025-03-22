package org

import (
	"errors"
	"fmt"
	db2 "memo/pkg/db"
	"memo/pkg/logger"
	"memo/pkg/org/parser"
	"memo/pkg/storage"

	"github.com/emirpasic/gods/maps/linkedhashmap"
)

// 用于加载从硬盘文件读取的 headline 数据，用于和数据库 headline 数据进行比较。
func NewHeadlineCache(headlines []storage.Headline, fileID, filePath, hash string) (*HeadlineCacheMap, error) {
	var err error
	fileDB, err := db2.NewOrgFileDB()
	if err != nil {
		return nil, err
	}
	headDB, err := db2.NewOrgHeadlineDB()
	if err != nil {
		return nil, err
	}
	cache := HeadlineCacheMap{HeadlinesFileCache: linkedhashmap.New(), DuplicateID: []string{},
		HeadlinesDBCache: linkedhashmap.New(), fileID: fileID, filePath: filePath,
		FileDB: fileDB, HeadlineDB: headDB}

	err = cache.loadHead(headlines)
	if err != nil {
		return nil, err
	}
	cache.HeadlinesDBCache, err = headDB.LoadFileHeadFromDB(fileID)
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
	FileDB             *db2.OrgFileDB
	HeadlineDB         *db2.OrgHeadlineDB
}

// Scan for head without id or with duplicate id and prepare cache for compare.
func (h *HeadlineCacheMap) loadHead(headlines []storage.Headline) error {
	for _, head := range headlines {
		err := h.loadHead(head.Children)
		if err != nil {
			return err
		}
		if _, ok := h.HeadlinesFileCache.Get(head.ID); ok {
			h.DuplicateID = append(h.DuplicateID, head.ID)
		} else {
			// compute headline struct hash.
			hash := parser.HashContent(head.String())
			if err != nil {
				return logger.Errorf("Hash headline error: %v", err)
			}
			head.Hash = hash
			h.HeadlinesFileCache.Put(head.ID, head)
		}
	}
	return nil
}

// 通过对比两个缓存，进行对比更新数据库中的headline
func (h *HeadlineCacheMap) UpdateHeadlineToDB(force bool) error {
	fileHeads := h.HeadlinesFileCache.Iterator()
	var err error
	for fileHeads.Begin(); fileHeads.Next(); {
		fileheadID, value := fileHeads.Key(), fileHeads.Value()
		headFromFile := value.(storage.Headline)
		if headFromDB, ok := h.HeadlinesDBCache.Get(fileheadID); ok {
			if headFromDB.(storage.Headline).Hash != headFromFile.Hash || force {
				err = h.HeadlineDB.UpdateHeadline(headFromFile)
				h.HeadlinesDBCache.Remove(fileheadID)
			} else {
				h.HeadlinesDBCache.Remove(fileheadID)
				continue
			}
		} else {
			err = h.HeadlineDB.Create(headFromFile)
		}
		if err != nil {
			return err
		}
	}

	// Unattach the headline from fileid which still left in dbcache map.
	itDB := h.HeadlinesDBCache.Iterator()
	for itDB.Begin(); itDB.Next(); {
		_, head := itDB.Key(), itDB.Value()
		headDB := head.(storage.Headline)
		err = h.HeadlineDB.Delete(headDB)
		if err != nil {
			return err
		}
	}
	return err
}
