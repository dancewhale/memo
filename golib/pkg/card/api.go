package card

import (
	"errors"
	"memo/pkg/db"
	"memo/pkg/db/query"
	"memo/pkg/logger"
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
	service.RegisterMethod(epc.MakeMethod("GetFileHasNewCard", api.GetFileHasNewCard, "string", "Get file has new card"))
	return service
}

func (api *CardApi) GetFileHasNewCard() util.Result {
	cDB, err := query.NewCardDB()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	files, err := cDB.GetFileHasNewCard()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	return util.Result{Data: files, Err: nil}
}

// function to export to emacs rpc.
func (api *CardApi) GetNextReviewNote() util.Result {
	head := GetReviewCardByWeightDueTime()
	if head == nil {
		return util.Result{Data: nil, Err: errors.New("There is no card need review")}
	}

	hDB, _ := db.NewOrgHeadlineDB()
	note := util.GetHeadStruct(head, hDB)

	return util.Result{Data: note, Err: nil}
}

func (api *CardApi) FindNoteList(q []string) util.Result {
	hDB, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	query, err := query.BuildQueryFromSyntax(q)
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
	hDB, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: nil, Err: err}
	}
	query, err := query.BuildQueryFromSyntax(q)
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
