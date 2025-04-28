package card

import (
	"errors"
	"memo/pkg/db"
	"memo/pkg/db/query"
	"memo/pkg/logger"
	"memo/pkg/storage"

	epc "github.com/dancewhale/go-elrpc"

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
	service.RegisterMethod(epc.MakeMethod("UndoReviewNote", api.UndoReviewNote, "string", "Undo today latest review record by headID"))
	service.RegisterMethod(epc.MakeMethod("FindNote", api.FindNote, "string", "Find note with query"))
	service.RegisterMethod(epc.MakeMethod("FindNoteList", api.FindNoteList, "string", "Find notes with query"))
	service.RegisterMethod(epc.MakeMethod("GetFileHasNewCard", api.GetFileHasNewCard, "string", "Get file has new card"))
	service.RegisterMethod(epc.MakeMethod("GetFileByID", api.GetFileByID, "string", "Get file has new card"))
	service.RegisterMethod(epc.MakeMethod("GetFileChildrenCard", api.GetFileChildrenCard, "string", "Get file children card"))
	service.RegisterMethod(epc.MakeMethod("GetHeadChildrenCard", api.GetHeadChildrenCard, "string", "Get head children card"))
	service.RegisterMethod(epc.MakeMethod("GetHeadContentByID", api.GetHeadContentByID, "string", "Get headline by id"))
	service.RegisterMethod(epc.MakeMethod("GetHeadFilePath", api.GetHeadFilePath, "string", "Get headline file path"))
	service.RegisterMethod(epc.MakeMethod("GetNextReviewCard", api.GetNextReviewCard, "", "Get next reviewed card info"))             // Added
	service.RegisterMethod(epc.MakeMethod("GetPreviousReviewCard", api.GetPreviousReviewCard, "", "Get previous reviewed card info")) // Added
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

func (api *CardApi) GetFileByID(fileid string) db.Result {
	fileInfo, err := db.GetCacheManager().GetFileFromCache(fileid)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: fileInfo, Err: nil}

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

func (api *CardApi) GetHeadChildrenCard(headid, fileid string) db.Result {
	childrens, err := db.GetChildrenByHeadlineID(headid, fileid)
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
	err = bq.ExecuteScan(&notes)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	if len(notes) == 0 {
		return db.Result{Data: nil, Err: nil}
	}
	return db.Result{Data: notes, Err: nil}
}

func (api *CardApi) FindNote(q []string) db.Result {
	var notes []db.HeadlineWithFsrs

	bq, err := query.BuildQueryFromSyntax(q)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	err = bq.ExecuteScan(&notes)
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

func (api *CardApi) UndoReviewNote(headlineID string) db.Result {
	success, err := UndoReviewCard(headlineID)
	if err != nil {
		return db.Result{Data: false, Err: err}
	} else {
		return db.Result{Data: success, Err: nil}
	}
}

func (api *CardApi) GetHeadFilePath(headid string) db.Result {
	headDB, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: "", Err: err}
	}
	filepath, err := headDB.GetHeadFilePath(headid)
	if err != nil {
		return db.Result{Data: "", Err: err}
	}
	return db.Result{Data: filepath, Err: nil}
}

// GetNextReviewCard gets the next reviewed card details
func (api *CardApi) GetNextReviewCard() db.Result {
	headID, err := GetNextReviewHeadID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	if headID == "" {
		return db.Result{Data: nil, Err: errors.New("no next review card found")}
	}

	headineWithFsrs, err := api.getHeadlineWithFsrsByID(headID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	return db.Result{Data: headineWithFsrs, Err: nil}
}

// GetPreviousReviewCard gets the previous reviewed card details
func (api *CardApi) GetPreviousReviewCard() db.Result {
	headID, err := GetPreviousReviewHeadID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	if headID == "" {
		return db.Result{Data: nil, Err: errors.New("no previous review card found")}
	}

	headineWithFsrs, err := api.getHeadlineWithFsrsByID(headID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	return db.Result{Data: headineWithFsrs, Err: nil}
}

// Helper function to get HeadlineWithFsrs by ID using the cache
func (api *CardApi) getHeadlineWithFsrsByID(headID string) (*db.HeadlineWithFsrs, error) {
	// 1. Get FileID from HeadID
	headDB, err := db.NewOrgHeadlineDB()
	if err != nil {
		return nil, logger.Errorf("Failed to get OrgHeadlineDB: %v", err)
	}
	fileID, err := headDB.GetFileIDByOrgID(headID)
	if err != nil {
		return nil, logger.Errorf("Failed to get FileID for head %s: %v", headID, err)
	}
	if fileID == nil {
		// It might be a virtual card, need to handle this case if necessary.
		// For now, assume it must belong to a file.
		return nil, logger.Errorf("FileID not found for head %s", headID)
	}

	// 2. Get File Cache
	cacheManager := db.GetCacheManager()
	fileCache, err := cacheManager.GetFileCacheFromCache(*fileID)
	if err != nil {
		return nil, logger.Errorf("Failed to get file cache for file %s: %v", *fileID, err)
	}

	// 3. Get HeadlineStats from HeadMap
	headStats, ok := fileCache.HeadMap[headID]
	if !ok || headStats == nil {
		// If not found in cache, maybe invalidate and retry?
		// Or the cache structure/logic needs adjustment.
		return nil, logger.Errorf("Headline %s not found in cache for file %s", headID, *fileID)
	}

	return headStats.Info, nil
}
