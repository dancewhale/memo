package card

import (
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"time"
)

func ReviewCard(orgID string, rating gfsrs.Rating) error {
	db, err := NewFsrsDB()
	if err != nil {
		return err
	}

	logger.Debugf("Start Review Headline with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fsrs := db.getFsrsInfoByOrgID(orgID)
	if fsrs == nil {
		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}

	// review 状态为waitReview 的Card, 如果评价为easy,则设置为WaitCardInit
	needReview, error := db.ifCardIsDue(orgID)

	if needReview && error == nil {
		schedulingInfo := api.scheduler.Repeat(fsrs.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := &storage.ReviewLog{HeadlineID: orgID, ReviewLog: rLog}

		fsrs.Card = updatedCard
		err := db.createReviewLog(reviewlog)
		if err != nil {
			return err
		}

		return db.updateFsrs(fsrs)
	}
	logger.Infof("Card %s no need to review.", orgID)
	return nil
}
