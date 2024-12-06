package org

import (
	"errors"
	"fmt"
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"github.com/gohugoio/hashstructure"
	"memo/pkg/logger"
	"memo/pkg/org/db"
)

func NewHeadlineCache(headlines []db.Headline, fileID string, filePath string) (*HeadlineCacheMap, error) {
	cache := HeadlineCacheMap{HeadlinesFileCache: linkedhashmap.New(), DuplicateID: []string{},
		HeadlinesDBCache: linkedhashmap.New(), fileID: fileID, filePath: filePath}

	cache.scanDuplicateID(headlines)
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
func (h *HeadlineCacheMap) scanDuplicateID(headlines []db.Headline) {
	options := hashstructure.HashOptions{IgnoreZeroValue: true, ZeroNil: true}
	for _, head := range headlines {
		h.scanDuplicateID(head.Children)
		if _, ok := h.HeadlinesFileCache.Get(head.Data.ID); ok {
			h.DuplicateID = append(h.DuplicateID, head.Data.ID)
		} else {
			// compute headline struct hash.
			hash, err := hashstructure.Hash(head.Data, &options)
			if err != nil {
				logger.Errorf("Hash headline error: %v", err)
				return
			}
			head.Hash = hash
			if head.Data.ID != "" {
				h.HeadlinesFileCache.Put(head.Data.ID, head)
			}
		}
	}
}
