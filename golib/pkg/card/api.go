package card

import (
	"errors"
	"memo/pkg/org/db"
	"time"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	epc "github.com/kiwanami/go-elrpc"
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	//	"github.com/spewerspew/spew"
	"gorm.io/gorm"
)

func NewCardApi() (*CardApi, error) {
	DB, err := storage.InitDBEngine()
	scheduler := *gfsrs.NewFSRS(gfsrs.DefaultParam())
	return &CardApi{
		Query:     *dal.Use(DB),
		db:        DB,
		scheduler: scheduler,
	}, err
}

type CardApi struct {
	dal.Query
	db        *gorm.DB
	scheduler gfsrs.FSRS
}

func (api *CardApi) RegistryEpcMethod(service *epc.ServerService) *epc.ServerService {
	service.RegisterMethod(epc.MakeMethod("GetNextReviewNote", api.GetNextReviewNote, "string", "Get next review note"))
	service.RegisterMethod(epc.MakeMethod("ReviewNote", api.ReviewNote, "string", "Review note"))
	return service
}

// function to export to emacs rpc.
func (api *CardApi) GetNextReviewNote() util.Result {
	head := api.GetReviewCardByWeightDueTime()
	if head == nil {
		return util.Result{Data: nil, Err: errors.New("There is no card need review.")}
	}

	headDB, _ := db.NewOrgHeadlineDB()
	note := util.GetHeadStruct(head, headDB)

	return util.Result{note, nil}
}

func (api *CardApi) ReviewNote(orgID string, rating string) util.Result {
	if orgID == "" {
		return util.Result{nil, errors.New("orgID is empty When review note.")}
	}
	fsrsRate := storage.StringToRate(rating)
	err := ReviewCard(orgID, fsrsRate)
	logger.Debugf("Review note success, orgID: %s, rating: %s", orgID, rating)
	if err != nil {
		return util.Result{false, err}
	} else {
		return util.Result{true, nil}
	}
}

// CountCards 获取卡包中的闪卡数量。
func (api *CardApi) CountCards() int {
	return 0
}

func (api *CardApi) GetReviewCardByWeightDueTime() *storage.Headline {
	fsrsDB, err := NewFsrsDB()
	if err != nil {
		return nil
	}
	for _, stype := range []string{storage.NORMAL, storage.POSTPONE} {
		cards := fsrsDB.dueCardsInToday(stype)
		if len(cards) == 0 {
			cards = fsrsDB.dueCardsBeforeToday(stype)
			if len(cards) == 0 {
				continue
			} else {
				return cards[0]
			}
		} else {
			return cards[0]
		}
	}
	return nil
}

// 给指定过期的闪卡打分进行review,返回复习后的闪卡
func (api *CardApi) ReviewCard(orgID string, rating gfsrs.Rating) error {
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

func (api *CardApi) ScanHeadlineInitFsrs() ([]*storage.Headline, error) {
	logger.Infof("Start to scan org for headline init.")
	fsrsDB, err := NewFsrsDB()
	if err != nil {
		return nil, err
	}

	heads := fsrsDB.getFsrsEmptyHeadline()
	for _, head := range heads {
		if head.Status == "" && head.Content != "" {
			fsrsinfo := &storage.FsrsInfo{}
			fsrsinfo.Card = gfsrs.NewCard()
			fsrsinfo.HeadlineID = head.ID
			err = fsrsDB.createFsrs(fsrsinfo)
			if err != nil {
				return nil, logger.Errorf("Create fsrs info for headline %s failed: %v", head.ID, err)
			}
		}
	}
	return heads, err
}
