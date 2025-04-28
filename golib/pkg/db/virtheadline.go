package db

import (
	"context"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
)

func NewVirtHeadDB() (*VirtHeadDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	if dal.Headline == nil {
		dal.SetDefault(db)
	}
	return &VirtHeadDB{db: db}, nil
}

type VirtHeadDB struct {
	db *gorm.DB
}

func preloadVirt(d *gorm.DB) *gorm.DB {
	return d.Order("`order` ASC").Preload("Children", preload).
		Preload("Properties").Preload("Tags").Preload("LogBook")
}

func (v *VirtHeadDB) GetVirtAncestorID(headID string) (*HeadlineWithFsrs, error) {
	virtHead := dal.Headline
	fsrs := dal.FsrsInfo

	headline, err := virtHead.WithContext(context.Background()).Where(virtHead.ID.Eq(headID)).First()
	if err != nil {
		return nil, logger.Errorf("Found head with id %s error: %v", headID, err)
	}
	for headline.HeadlineID != nil && *headline.HeadlineID != "" {
		parentID := *headline.HeadlineID
		headline, err = virtHead.WithContext(context.Background()).Where(virtHead.ID.Eq(parentID)).First()
		if err != nil {
			return nil, logger.Errorf("Found head with id %s error: %v", parentID, err)
		}
	}

	var results []*HeadlineWithFsrs
	err = virtHead.WithContext(context.Background()).
		Select(virtHead.ALL, fsrs.ALL).
		LeftJoin(fsrs, virtHead.ID.EqCol(fsrs.HeadlineID)).
		Where(virtHead.ID.Eq(headline.ID)).
		Scan(&results)

	if err != nil {
		return nil, logger.Errorf("Get ancestor head of head %s error: %v", headID, err)
	}
	if len(results) != 0 {
		count, err := virtHead.WithContext(context.Background()).Where(virtHead.HeadlineID.Eq(headline.ID)).Count()
		if err != nil {
			return nil, logger.Errorf("Get ancestor head of head %s error: %v", headID, err)
		}
		results[0].ChildVirtCards = int(count)
	}

	return results[0], nil
}

func (v *VirtHeadDB) GetVirtChildrenHeadTree(headID string) ([]*HeadlineWithFsrs, error) {
	fsrs := dal.FsrsInfo
	headline := dal.Headline

	var results []*HeadlineWithFsrs
	err := headline.WithContext(context.Background()).
		Select(headline.ALL, fsrs.ALL).
		LeftJoin(fsrs, headline.ID.EqCol(fsrs.HeadlineID)).
		Where(headline.HeadlineID.Eq(headID)).
		Scan(&results)
	if err != nil {
		return nil, logger.Errorf("Get children virt head of head %s error: %v", headID, err)
	}
	for _, result := range results {
		count, err := headline.WithContext(context.Background()).Where(headline.HeadlineID.Eq(result.ID)).Count()
		if err != nil {
			return nil, logger.Errorf("Get children virt head of head %s error: %v", headID, err)
		}
		result.ChildVirtCards = int(count)
	}
	return results, nil
}
