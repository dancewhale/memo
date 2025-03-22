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

	needReview, err := db.IfCardIsDue(orgID)

	if needReview && err == nil {
		schedulingInfo := Scheduler.Repeat(fsrsInfo.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := &storage.ReviewLog{HeadlineID: orgID, ReviewLog: rLog}

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
