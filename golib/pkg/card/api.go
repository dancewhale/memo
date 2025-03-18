package card

import (
	"errors"
	cardDB "memo/pkg/card/db"
	"memo/pkg/logger"
	headDB "memo/pkg/org/db"
	"memo/pkg/storage"
	"memo/pkg/util"

	epc "github.com/kiwanami/go-elrpc"
	//	"github.com/spewerspew/spew"
	"gorm.io/gorm"
)

func NewCardApi() (*CardApi, error) {
	DB, err := storage.InitDBEngine()
	return &CardApi{
		db: DB,
	}, err
}

type CardApi struct {
	db *gorm.DB
}

func (api *CardApi) RegistryEpcMethod(service *epc.ServerService) *epc.ServerService {
	service.RegisterMethod(epc.MakeMethod("GetNextReviewNote", api.GetNextReviewNote, "string", "Get next review note"))
	service.RegisterMethod(epc.MakeMethod("ReviewNote", api.ReviewNote, "string", "Review note"))
	service.RegisterMethod(epc.MakeMethod("FindNoteFirst", api.FindNoteFirst, "string", "Find note with query"))
	service.RegisterMethod(epc.MakeMethod("FindNoteList", api.FindNoteList, "string", "Find notes with query"))
	return service
}

// function to export to emacs rpc.
func (api *CardApi) GetNextReviewNote() util.Result {
	head := cardDB.GetReviewCardByWeightDueTime()
	if head == nil {
		return util.Result{Data: nil, Err: errors.New("There is no card need review")}
	}

	hDB, _ := headDB.NewOrgHeadlineDB()
	note := util.GetHeadStruct(head, hDB)

	return util.Result{Data: note, Err: nil}
}

func (api *CardApi) FindNoteList(q []string) util.Result {
	hDB, err := headDB.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	query, err := cardDB.BuildQueryFromSyntax(q)
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	heads, err := query.ExecuteList()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	if len(heads) == 0 {
		return util.Result{Data: nil, Err: nil}
	}
	notes := util.GetHeadStructs(heads, hDB)
	return util.Result{Data: notes, Err: nil}
}

func (api *CardApi) FindNoteFirst(q []string) util.Result {
	hDB, err := headDB.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	query, err := cardDB.BuildQueryFromSyntax(q)
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	head, err := query.ExecuteFirst()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	if head == nil {
		return util.Result{Data: nil, Err: nil}
	}
	note := util.GetHeadStruct(head, hDB)
	return util.Result{Data: note, Err: nil}
}

func (api *CardApi) ReviewNote(orgID string, rating string) util.Result {
	if orgID == "" {
		return util.Result{Data: nil, Err: errors.New("orgID is empty When review note")}
	}
	fsrsRate := storage.StringToRate(rating)
	err := ReviewCard(orgID, fsrsRate)
	logger.Debugf("Review note success, orgID: %s, rating: %s", orgID, rating)
	if err != nil {
		return util.Result{Data: false, Err: err}
	} else {
		return util.Result{Data: true, Err: nil}
	}
}
