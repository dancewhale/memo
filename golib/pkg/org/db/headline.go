package db

import (
	"context"
	"github.com/creker/hashstructure"
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"gorm.io/gorm"
	"memo/pkg/logger"
	"memo/pkg/org/location"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

type Headline struct {
	Data     storage.Headline
	Children []Headline
	Hash     []byte
}

// 修改保存依靠双键，ID和FileID都必须存在
func (h *Headline) UpdateByIDFile() error {
	head := dal.Use(storage.Engine).Headline
	loc := dal.Use(storage.Engine).Location
	clock := dal.Use(storage.Engine).Clock

	_, err := clock.WithContext(context.Background()).Unscoped().Where(clock.HeadlineID.Eq(h.Data.ID)).Delete()
	if err != nil {
		return logger.Errorf("Delete logbook of head %s error: %v", h.Data.ID, err)
	}

	// delete location of headline which type is source, and then append new location
	locs, _ := head.Locations.Where(loc.Type.Eq(string(location.SourceType))).Model(&h.Data).Find()
	head.Locations.Model(&h.Data).Delete(locs...)
	// Warn: only unattach source location, other type location may duplicate append.
	if len(h.Data.Locations) != 0 {
		head.Locations.Model(&h.Data).Append(h.Data.Locations...)
	}

	storage.Engine.Session(&gorm.Session{FullSaveAssociations: true}).Updates(h.Data)

	return nil
}

// Create headline record.
// need to deal with situation that headline unattach from file，then fileid of file is 为null
func (h *Headline) Create() error {
	headline := dal.Use(storage.Engine).Headline
	result, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(h.Data.ID)).UpdateSimple(headline.FileID.Value(*h.Data.FileID))
	if err != nil {
		return logger.Errorf("Update fileid of headline %v error: %v", h.Data, err)
	}
	if result.RowsAffected == 0 {
		err := headline.WithContext(context.Background()).Create(&h.Data)
		if err != nil {
			return logger.Errorf("Insert headline %v error: %v", h.Data, err)
		}
	}
	return nil
}

func (h *Headline) UnattachFromFile() error {
	headline := dal.Use(storage.Engine).Headline
	_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(h.Data.ID)).UpdateSimple(headline.FileID.Null(), headline.ParentID.Null())
	if err != nil {
		return logger.Errorf("Delete logbook of head %s error: %v", h.Data.ID, err)
	}
	return nil
}

// Load all headline attach to id from database.
func LoadHeadFromDB(fileID string) (*linkedhashmap.Map, error) {
	headlinesDBCache := linkedhashmap.New()
	headline := dal.Use(storage.Engine).Headline
	headlines, err := headline.WithContext(context.Background()).Where(headline.FileID.Eq(fileID)).Find()
	if err != nil {
		return nil, logger.Errorf("Headlines load for file %s error: %v", fileID, err)
	}
	if len(headlines) != 0 {
		for _, headline := range headlines {
			options := hashstructure.HashOptions{IgnoreZeroValue: true, ZeroNil: true}
			hash, err := hashstructure.Hash(*headline, hashstructure.FormatV2, &options)
			if err != nil {
				return nil, logger.Errorf("Hash headline struct error: %v", err)
			}
			head := Headline{Data: *headline, Hash: hash}
			headlinesDBCache.Put(headline.ID, head)
		}
	}
	return headlinesDBCache, nil
}
