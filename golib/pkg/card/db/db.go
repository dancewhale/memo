package db

import (
	"context"
	"gorm.io/gorm"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

func NewFsrsDB() (*FsrsDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	return &FsrsDB{q: dal.Q, db: db}, nil
}

type FsrsDB struct {
	q  *dal.Query
	db *gorm.DB
}

func (f *FsrsDB) CreateFsrs(fsrsInfo *storage.FsrsInfo) error {
	fsrs := f.q.FsrsInfo
	err := fsrs.WithContext(context.Background()).Create(fsrsInfo)
	if err != nil {
		return logger.Errorf("Create fsrs info for headline %s failed: %v", fsrsInfo.HeadlineID, err)
	}
	return nil
}

// update fsrsinfo of headline.
func (f *FsrsDB) UpdateFsrs(Fsrs *storage.FsrsInfo) error {
	if Fsrs == nil {
		return nil
	}
	fsrs := f.q.FsrsInfo
	_, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(Fsrs.HeadlineID)).Updates(Fsrs)
	if err != nil {
		logger.Errorf("Update %s Headline FsrsInfo in database failed: %v", Fsrs.HeadlineID, err)
		return err
	}
	return nil
}

// 获取一张卡片的fsrs信息
func (f *FsrsDB) GetFsrsInfoByOrgID(orgid string) *storage.FsrsInfo {
	fsrs := f.q.FsrsInfo
	FsrsInfo, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get FsrsInfo by orgid failed: %v", err)
		return nil
	}
	return FsrsInfo
}

// 移除一张卡片,包括所有学习记录。
func (f *FsrsDB) removeFsrs(orgid string) error {
	fsrs := f.q.FsrsInfo
	r := f.q.ReviewLog

	_, err := fsrs.WithContext(context.Background()).Unscoped().Where(fsrs.HeadlineID.Eq(orgid)).Delete()
	if err != nil {
		return logger.Errorf("Remove Fsrs in by %s failed: %v", orgid, err)
	}
	_, err = r.WithContext(context.Background()).Unscoped().Where(r.HeadlineID.Eq(orgid)).Delete()
	if err != nil {
		return logger.Errorf("Remove ReviewLog in by %s failed: %v", orgid, err)
	}
	return nil
}

func (f *FsrsDB) CreateReviewLog(rlog *storage.ReviewLog) error {
	r := f.q.ReviewLog

	if rlog.HeadlineID == "" {
		return logger.Errorf("Add review log failed: headlineID is empty.")
	}

	err := r.WithContext(context.Background()).Create(rlog)
	if err != nil {
		return logger.Errorf("Add review log in db failed: %v", err)
	}
	return err
}

// get heads that need init fsrs info.
func (f *FsrsDB) GetFsrsEmptyHeadline() []*storage.Headline {
	headline := f.q.Headline
	fsrsInfo := f.q.FsrsInfo

	heads, err := headline.WithContext(context.Background()).
		LeftJoin(fsrsInfo, fsrsInfo.HeadlineID.EqCol(headline.ID)).
		Where(fsrsInfo.CreatedAt.IsNull()).
		Find()
	if err != nil {
		logger.Errorf("Get fsrs empty headline failed: %v", err)
		return nil
	}
	return heads
}
