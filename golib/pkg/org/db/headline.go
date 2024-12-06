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
