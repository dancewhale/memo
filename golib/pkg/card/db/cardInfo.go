package db

import (
	"context"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

func (c *CardDB) GetFileHasNewCard() ([]*storage.Headline, error) {
	head := dal.Headline
	fsrs := dal.FsrsInfo

	heads, err := head.WithContext(context.Background()).
		Join(fsrs, head.ID.EqCol(fsrs.HeadlineID)).
		Select(head.FileID).Distinct().
		Where(fsrs.State.Eq(0)).Find()
	if err != nil {
		return nil, logger.Errorf("Get file has new card error: %v", err)
	}
	return heads, nil
}
