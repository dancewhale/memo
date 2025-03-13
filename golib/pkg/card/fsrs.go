package card

import (
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"sync"
	"time"
)

var muteDB = sync.Mutex{}

var Scheduler = gfsrs.NewFSRS(gfsrs.DefaultParam())

func ReviewCard(orgID string, rating gfsrs.Rating) error {
	db, err := NewFsrsDB()
	if err != nil {
		return err
	}

	logger.Debugf("Start Review Headline with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fsrsInfo := db.getFsrsInfoByOrgID(orgID)
	if fsrsInfo == nil {
		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}

	// review 状态为waitReview 的Card, 如果评价为easy,则设置为WaitCardInit
	needReview, error := db.ifCardIsDue(orgID)

	if needReview && error == nil {
		schedulingInfo := Scheduler.Repeat(fsrsInfo.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := &storage.ReviewLog{HeadlineID: orgID, ReviewLog: rLog}

		fsrsInfo.Card = updatedCard
		err := db.createReviewLog(reviewlog)
		if err != nil {
			return err
		}

		return db.updateFsrs(fsrsInfo)
	}
	logger.Infof("Card %s no need to review.", orgID)
	return nil
}

func ScanInitFsrs() {
	go func() {
		muteDB.Lock()
		defer muteDB.Unlock()
		logger.Infof("Start to scan org for headline init.")
		fsrsDB, err := NewFsrsDB()
		if err != nil {
			return
		}

		heads := fsrsDB.getFsrsEmptyHeadline()
		for _, head := range heads {
			fsrsinfo := &storage.FsrsInfo{}
			fsrsinfo.Card = gfsrs.NewCard()
			fsrsinfo.HeadlineID = head.ID
			err = fsrsDB.createFsrs(fsrsinfo)
			if err != nil {
				logger.Errorf("Create fsrs info for headline %s failed: %v", head.ID, err)
				return
			}
		}
	}()
}
