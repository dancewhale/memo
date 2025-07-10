package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
)

func NewFsrsDB() (*FsrsDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	if dal.FsrsInfo != nil {
		dal.SetDefault(db)
	}
	return &FsrsDB{db: db}, nil
}

type FsrsDB struct {
	db *gorm.DB
}

func (f *FsrsDB) CreateFsrs(fsrsInfo *storage.FsrsInfo) error {
	fsrs := dal.FsrsInfo
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
	fsrs := dal.FsrsInfo
	_, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(Fsrs.HeadlineID)).Updates(Fsrs)
	if err != nil {
		logger.Errorf("Update %s Headline FsrsInfo in database failed: %v", Fsrs.HeadlineID, err)
		return err
	}

	if cacheManager := GetCacheManager(); cacheManager != nil {
		cacheManager.RefreshCacheByHeadlineID(Fsrs.HeadlineID)
	}
	return nil
}

// 获取一张卡片的fsrs信息
func (f *FsrsDB) GetFsrsInfoByOrgID(orgid string) *storage.FsrsInfo {
	fsrs := dal.FsrsInfo
	FsrsInfo, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get FsrsInfo by orgid failed: %v", err)
		return nil
	}
	return FsrsInfo
}

// 移除一张卡片,包括所有学习记录。
func (f *FsrsDB) removeFsrs(orgid string) error {
	fsrs := dal.FsrsInfo
	r := dal.ReviewLog

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

// 传入参数orgid,当对应的Card 卡片已经到期则返回true,否则返回false
func (f *FsrsDB) IfCardIsDue(orgid string) (bool, error) {
	fsrs := dal.FsrsInfo

	_, todayEnd := GetDayTime(0)

	heads, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(orgid)).
		Where(fsrs.Due.Lte(todayEnd)).Find()

	if err != nil {
		return false, logger.Errorf("Check orgid %s if is due Card failed: %v", orgid, err)
	} else if len(heads) == 0 {
		return false, nil
	} else {
		return true, nil
	}
}

func (f *FsrsDB) CreateReviewLog(rlog *storage.ReviewLog) error {
	r := dal.ReviewLog

	if rlog.HeadlineID == "" {
		return logger.Errorf("Add review log failed: headlineID is empty.")
	}

	err := r.WithContext(context.Background()).Create(rlog)
	if err != nil {
		return logger.Errorf("Add review log in db failed: %v", err)
	}
	return err
}

func (f *FsrsDB) Review(rlog *storage.ReviewLog, fsrsinfo *storage.FsrsInfo) error {
	fsrs := dal.FsrsInfo

	if fsrsinfo == nil || rlog == nil {
		return nil
	}
	r := dal.Q
	db := r.Begin()

	if rlog.HeadlineID == "" || fsrsinfo.HeadlineID == "" {
		return logger.Errorf("Review card with headlineID is empty.")
	}
	_, err := db.FsrsInfo.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(fsrsinfo.HeadlineID)).Updates(fsrsinfo)
	if err != nil {
		logger.Errorf("Update %s Headline FsrsInfo in database failed: %v", fsrsinfo.HeadlineID, err)
		_ = db.Rollback()
		return err
	}

	err = db.ReviewLog.WithContext(context.Background()).Create(rlog)
	if err != nil {
		_ = db.Rollback()
		return logger.Errorf("Add review log in db failed: %v", err)
	}

	return db.Commit()
}

func (f *FsrsDB) UndoReview(rlog *storage.ReviewLog) error {
	r := dal.Q
	// 使用事务确保操作的原子性
	db := r.Begin()
	defer func() {
		if r := recover(); r != nil {
			_ = db.Rollback()
		}
	}()

	// 更新Fsrs记录
	preCard := rlog.GetPreCard()
	reviewLog := db.ReviewLog
	fsrs := db.FsrsInfo

	_, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(rlog.HeadlineID)).
		UpdateSimple(fsrs.Due.Value(preCard.Due),
			fsrs.Stability.Value(preCard.Stability),
			fsrs.Difficulty.Value(preCard.Difficulty),
			fsrs.ElapsedDays.Value(preCard.ElapsedDays),
			fsrs.ScheduledDays.Value(preCard.ScheduledDays),
			fsrs.Reps.Value(preCard.Reps),
			fsrs.Lapses.Value(preCard.Lapses),
			fsrs.State.Value(int8(preCard.State)),
			fsrs.LastReview.Value(preCard.LastReview))

	if err != nil {
		db.Rollback()
		return logger.Errorf("Update Fsrs info failed: %v", err)
	}

	// 删除ReviewLog记录
	_, err = reviewLog.WithContext(context.Background()).Unscoped().Where(reviewLog.ID.Eq(rlog.ID)).Delete()
	if err != nil {
		db.Rollback()
		return logger.Errorf("Delete review log failed: %v", err)
	}

	return db.Commit()
}

func (f *FsrsDB) GetTodayLatestReviewLog(headlineID string) (*storage.ReviewLog, error) {
	// 获取当天的时间范围
	dayStart, dayEnd := GetDayTime(0)

	// 查询最新的ReviewLog记录
	reviewLog := dal.ReviewLog
	latestLog, err := reviewLog.WithContext(context.Background()).
		Where(reviewLog.HeadlineID.Eq(headlineID)).
		Where(reviewLog.Review.Between(dayStart, dayEnd)).
		Order(reviewLog.Review.Desc()).
		First()
	if err != nil {
		return nil, err
	}
	return latestLog, nil
}

// GetTodayReviewedHeadIDs 获取今天已复习的所有唯一 headlineID，按复习时间升序排列
func (f *FsrsDB) GetTodayReviewedHeadIDs() ([]string, error) {
	dayStart, dayEnd := GetDayTime(0)
	reviewLog := dal.ReviewLog
	var headIDs []string

	err := reviewLog.WithContext(context.Background()).
		Select(reviewLog.HeadlineID).Distinct(). // 选择唯一的 HeadlineID
		Where(reviewLog.Review.Between(dayStart, dayEnd)).
		Order(reviewLog.Review). // 按复习时间升序
		Pluck(reviewLog.HeadlineID, &headIDs)

	if err != nil {
		return nil, logger.Errorf("Get today reviewed head IDs failed: %v", err)
	}
	return headIDs, nil
}

// get heads that need init fsrs info.
func (f *FsrsDB) GetFsrsEmptyHeadline() []*storage.Headline {
	headline := dal.Headline
	fsrsInfo := dal.FsrsInfo

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
