package card

import (
	"errors"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	epc "github.com/kiwanami/go-elrpc"
	//	"github.com/spewerspew/spew"
	"gorm.io/gorm"
)

func NewCardApi() (*CardApi, error) {
	DB, err := storage.InitDBEngine()
	return &CardApi{
		Query: *dal.Use(DB),
		db:    DB,
	}, err
}

type CardApi struct {
	dal.Query
	db *gorm.DB
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
