package card

import (
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	cardDB "memo/pkg/db"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"sync"
	"time"
)

var muteDB = sync.Mutex{}

var Scheduler = gfsrs.NewFSRS(gfsrs.DefaultParam())

func ReviewCard(orgID string, rating gfsrs.Rating) error {
	db, err := cardDB.NewFsrsDB()
	if err != nil {
		return err
	}

	logger.Debugf("Start Review Headline with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fsrsInfo := db.GetFsrsInfoByOrgID(orgID)
	if fsrsInfo == nil {
		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}
	preCard := fsrsInfo.Card

	needReview, err := db.IfCardIsDue(orgID)

	if needReview && err == nil {
		schedulingInfo := Scheduler.Repeat(fsrsInfo.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := &storage.ReviewLog{HeadlineID: orgID, ReviewLog: rLog}
		reviewlog.SetPreCard(preCard)

		fsrsInfo.Card = updatedCard
		err = db.CreateReviewLog(reviewlog)
		if err != nil {
			return err
		}

		return db.UpdateFsrs(fsrsInfo)
	}
	logger.Infof("Card %s no need to review.", orgID)
	return err
}

// UndoReviewCard 撤销最近一次复习操作
// 传入headlineID，查询关联的ReviewLog记录，按Review时间从新到旧排序
// 如果最新记录的Review时间是当天，则将headlineID关联的Fsrs记录修改为ReviewLog中的Card数据并删除该记录
// 成功返回true，否则返回false，同时处理并返回error
func UndoReviewCard(headlineID string) (bool, error) {
	fsrsDB, err := cardDB.NewFsrsDB()
	if err != nil {
		return false, err
	}
	latestLog, err := fsrsDB.GetTodayLatestReviewLog(headlineID)
	if err != nil {
		logger.Infof("No review log found for headline %s today: %v", headlineID, err)
		return false, nil
	}

	err = fsrsDB.UndoReviewLog(latestLog)
	if err != nil {
		return false, err
	}

	// 清除缓存
	if cacheManager := cardDB.GetCacheManager(); cacheManager != nil {
		err = cacheManager.InvalidateCacheByHeadlineID(headlineID)
		if err != nil {
			logger.Errorf("Invalidate Cache by headlineid %s failed: %v.", headlineID, err)
		}
	}

	logger.Infof("Successfully undid review for headline %s", headlineID)
	return true, nil
}

func ScanInitFsrs() {
	go func() {
		muteDB.Lock()
		defer muteDB.Unlock()
		logger.Infof("Start to scan org for headline init.")
		fsrsDB, err := cardDB.NewFsrsDB()
		if err != nil {
			return
		}

		heads := fsrsDB.GetFsrsEmptyHeadline()
		for _, head := range heads {
			fsrsinfo := &storage.FsrsInfo{}
			fsrsinfo.Card = gfsrs.NewCard()
			fsrsinfo.HeadlineID = head.ID
			err = fsrsDB.CreateFsrs(fsrsinfo)
			if err != nil {
				logger.Errorf("Create fsrs info for headline %s failed: %v", head.ID, err)
				return
			}
		}
	}()
}
