package org

import (
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"memo/pkg/logger"
	"memo/pkg/org/db"
)

func NewHeadlineCache(headlines []db.Headline, fileID string) (*HeadlineCacheMap, error) {
	cache := HeadlineCacheMap{HeadlinesFileCache: linkedhashmap.New(), DuplicateID: []string{},
		HeadlinesDBCache: linkedhashmap.New(), fileID: fileID}
	cache.scanDuplicateID(headlines)
	if len(cache.DuplicateID) == 0 {
		return &cache, nil
	} else {
		return nil, logger.Errorf("Found Duplicate headline id %v", cache.DuplicateID)
	}
}

type HeadlineCacheMap struct {
	fileID             string
	HeadlinesFileCache *linkedhashmap.Map
	HeadlinesDBCache   *linkedhashmap.Map
	DuplicateID        []string
}

func (h *HeadlineCacheMap) scanDuplicateID(headlines []db.Headline) {
	for _, head := range headlines {
		h.scanDuplicateID(head.Children)
		if _, ok := h.HeadlinesFileCache.Get(head.Data.ID); ok {
			h.DuplicateID = append(h.DuplicateID, head.Data.ID)
		} else {
			h.HeadlinesFileCache.Put(head.Data.ID, head)
		}
	}
}
