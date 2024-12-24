package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

type Headline struct {
	Data     storage.Headline
	Children []Headline
	Hash     []byte
}

func (h *Headline) update() error {
	clock := dal.Use(storage.Engine).Clock
	_, err := clock.WithContext(context.Background()).Unscoped().Where(clock.HeadlineID.Eq(h.Data.ID)).Delete()
	if err != nil {
		return logger.Errorf("Delete logbook of head %s error: %v", h.Data.ID, err)
	}
	storage.Engine.Save(h.Data)
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
