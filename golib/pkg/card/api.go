package card

import (
	"errors"
	"memo/pkg/db"
	"memo/pkg/db/query"
	"memo/pkg/logger"
	"memo/pkg/storage"

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
	service.RegisterMethod(epc.MakeMethod("ReviewNote", api.ReviewNote, "string", "Review note"))
	service.RegisterMethod(epc.MakeMethod("FindNoteFirst", api.FindNoteFirst, "string", "Find note with query"))
	service.RegisterMethod(epc.MakeMethod("FindNoteList", api.FindNoteList, "string", "Find notes with query"))
	service.RegisterMethod(epc.MakeMethod("GetFileHasNewCard", api.GetFileHasNewCard, "string", "Get file has new card"))
	service.RegisterMethod(epc.MakeMethod("GetFileChildrenCard", api.GetFileChildrenCard, "string", "Get file children card"))
	service.RegisterMethod(epc.MakeMethod("GetHeadChildrenCard", api.GetHeadChildrenCard, "string", "Get head children card"))
	service.RegisterMethod(epc.MakeMethod("GetHeadContentByID", api.GetHeadContentByID, "string", "Get headline by id"))
	return service
}

func (api *CardApi) GetFileHasNewCard() db.Result {
	cardDB, err := query.NewCardDB()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	fileids, err := cardDB.GetFileHasNewCard()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	fileInfos, err := db.GetCacheManager().GetFilesFromCache(fileids)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: fileInfos, Err: nil}
}

func (api *CardApi) GetHeadContentByID(headid string) db.Result {
	headDB, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	head, err := headDB.GetHeadlineByID(headid)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: head.Content, Err: nil}
}

func (api *CardApi) GetFileChildrenCard(fileid string) db.Result {
	childrens, err := db.GetChildrenByFileID(fileid)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: childrens, Err: nil}
}

func (api *CardApi) GetHeadChildrenCard(headid, fileid string, notetype int) db.Result {
	childrens, err := db.GetChildrenByHeadlineID(headid, fileid, notetype)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: childrens, Err: nil}
}

func (api *CardApi) FindNoteList(q []string) db.Result {
	var notes []db.HeadlineWithFsrs

	bq, err := query.BuildQueryFromSyntax(q)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	err = bq.ExecuteScan(notes)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	if len(notes) == 0 {
		return db.Result{Data: nil, Err: nil}
	}
	return db.Result{Data: notes, Err: nil}
}

func (api *CardApi) FindNoteFirst(q []string) db.Result {
	var notes []db.HeadlineWithFsrs

	bq, err := query.BuildQueryFromSyntax(q)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	err = bq.ExecuteScan(notes)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	if len(notes) == 0 {
		return db.Result{Data: nil, Err: nil}
	}
	return db.Result{Data: notes[0], Err: nil}
}

func (api *CardApi) ReviewNote(orgID string, rating string) db.Result {
	if orgID == "" {
		return db.Result{Data: nil, Err: errors.New("orgID is empty When review note")}
	}
	fsrsRate := storage.StringToRate(rating)
	err := ReviewCard(orgID, fsrsRate)
	logger.Debugf("Review note success, orgID: %s, rating: %s", orgID, rating)
	if err != nil {
		return db.Result{Data: false, Err: err}
	} else {
		return db.Result{Data: true, Err: nil}
	}
}
