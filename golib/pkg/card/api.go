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
	service.RegisterMethod(epc.MakeMethod("GetFileByFileID", api.GetFileByFileID, "string", "Get file has new card"))
	service.RegisterMethod(epc.MakeMethod("GetFileChildrenCard", api.GetFileChildrenCard, "string", "Get file children card"))
	service.RegisterMethod(epc.MakeMethod("GetFirstFileHeadCard", api.GetFirstFileHeadCard, "string", "Get file head card, even by virt head id."))
	service.RegisterMethod(epc.MakeMethod("GetHeadChildrenCard", api.GetHeadChildrenCard, "string", "Get head children card"))
	service.RegisterMethod(epc.MakeMethod("GetHeadChildrenVirtCard", api.GetHeadChildrenVirtCard, "string", "Get head virt children note"))
	service.RegisterMethod(epc.MakeMethod("GetHeadContentByID", api.GetHeadContentByID, "string", "Get headline by id"))
	service.RegisterMethod(epc.MakeMethod("GetHeadFilePath", api.GetHeadFilePath, "string", "Get headline file path"))
	service.RegisterMethod(epc.MakeMethod("GetNextReviewCard", api.GetNextReviewCard, "", "Get next reviewed card info"))             // Added
	service.RegisterMethod(epc.MakeMethod("GetPreviousReviewCard", api.GetPreviousReviewCard, "", "Get previous reviewed card info")) // Added
	service.RegisterMethod(epc.MakeMethod("GetNextNewCard", api.GetNextNewCard, "string", "Get headlines in reading order for a file"))
	service.RegisterMethod(epc.MakeMethod("GetPreviousNewCard", api.GetPreviousNewCard, "string", "Get headlines in reading order for a file"))
	service.RegisterMethod(epc.MakeMethod("GetNextFileNewCard", api.GetNextFileNewCard, "string", "Get headlines in reading order for a file"))
	service.RegisterMethod(epc.MakeMethod("GetPreviousFileNewCard", api.GetPreviousFileNewCard, "string", "Get headlines in reading order for a file"))
	return service
}

func (api *CardApi) GetFileHasNewCard() db.Result {
	fileids, err := db.GetFileHasNewCard()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	fileInfos, err := db.GetCacheManager().GetFilesFromCache(fileids)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: fileInfos, Err: nil}
}

func (api *CardApi) GetFileByFileID(fileid string) db.Result {
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

func (api *CardApi) GetHeadChildrenVirtCard(id string) db.Result {
	head, err := db.GetFirstFileHeadByID(id)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	cache, err := db.GetCacheManager().GetFileCacheFromCache(*head.FileID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	// 获取所有子headline的ID
	childrens := cache.GetVirtHeadChildren(id)
	return db.Result{Data: childrens, Err: nil}
}

func (api *CardApi) GetHeadChildrenCard(headid, fileid string) db.Result {
	cache, err := db.GetCacheManager().GetFileCacheFromCache(fileid)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	// 获取所有子headline的ID
	childrens := cache.GetFileHeadChildren(headid)
	return db.Result{Data: childrens, Err: nil}
}

func (api *CardApi) GetFirstFileHeadCard(headid string) db.Result {
	head, err := db.GetFirstFileHeadByID(headid)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	// Get File Cache
	cacheManager := db.GetCacheManager()
	fileCache, err := cacheManager.GetFileCacheFromCache(*head.FileID)
	if err != nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to get file cache for file %s: %v", *head.FileID, err)}
	}

	// Get HeadlineStats from HeadMap
	headStats, ok := fileCache.HeadMap[head.ID]
	if !ok || headStats == nil {
		// If not found in cache, maybe invalidate and retry?
		// Or the cache structure/logic needs adjustment.
		return db.Result{Data: nil, Err: logger.Errorf("Headline %s not found in cache for file %s", head.ID, *head.FileID)}
	}

	return db.Result{Data: headStats.Info, Err: nil}
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
	note, err := api.getHeadlineWithFsrsByID(notes[0].ID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	return db.Result{Data: note, Err: nil}
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

func (api *CardApi) GetPreviousNewCard() db.Result {
	queue := db.GetReadingQueue()
	headID, err := queue.PreviousNewCard()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	} else if headID == "" {
		return db.Result{Data: nil, Err: nil}
	}
	cacheManager := db.GetCacheManager()
	head := cacheManager.GetHeadFromCache(headID)
	if head == nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to get head %s in cache.", headID)}
	}
	return db.Result{Data: head.Info, Err: nil}
}

func (api *CardApi) GetPreviousFileNewCard() db.Result {
	queue := db.GetReadingQueue()
	currentFileID, err := queue.GetCurrentFileID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	for {
		id, err := queue.PreviousFileID()
		if err != nil {
			return db.Result{Data: nil, Err: err}
		}
		if id == currentFileID {
			return db.Result{Data: nil, Err: errors.New("No next file has new card to read")}
		}
		finished, err := queue.IfCurrentFileReadFinished()
		if err != nil {
			return db.Result{Data: nil, Err: err}
		}
		if finished {
			continue
		} else {
			break
		}
	}
	headID, err := queue.GetCurrentHeadID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	cacheManager := db.GetCacheManager()
	head := cacheManager.GetHeadFromCache(headID)
	if head == nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to get head %s in cache.", headID)}
	}

	return db.Result{Data: head.Info, Err: nil}
}

func (api *CardApi) GetNextNewCard() db.Result {
	queue := db.GetReadingQueue()
	headID, err := queue.NextNewCard()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	} else if headID == "" {
		return db.Result{Data: nil, Err: nil}
	}
	cacheManager := db.GetCacheManager()
	head := cacheManager.GetHeadFromCache(headID)
	if head == nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to get head %s in cache.", headID)}
	}

	return db.Result{Data: head.Info, Err: nil}
}

func (api *CardApi) GetNextFileNewCard() db.Result {
	queue := db.GetReadingQueue()
	currentFileID, err := queue.GetCurrentFileID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	for {
		id, err := queue.NextFileID()
		if err != nil {
			return db.Result{Data: nil, Err: err}
		}
		if id == currentFileID {
			return db.Result{Data: nil, Err: errors.New("No next file has new card to read")}
		}
		finished, err := queue.IfCurrentFileReadFinished()
		if err != nil {
			return db.Result{Data: nil, Err: err}
		}
		if finished {
			continue
		} else {
			break
		}
	}
	headID, err := queue.GetCurrentHeadID()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	cacheManager := db.GetCacheManager()
	head := cacheManager.GetHeadFromCache(headID)
	if head == nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to get head %s in cache.", headID)}
	}

	return db.Result{Data: head.Info, Err: nil}
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
	cacheManager := db.GetCacheManager()
	head := cacheManager.GetHeadFromCache(headID)

	// 3. Get HeadlineStats from HeadMap
	if head == nil {
		// If not found in cache, maybe invalidate and retry?
		// Or the cache structure/logic needs adjustment.
		return nil, logger.Errorf("Headline %s not found in cache", headID)
	}
	return head.Info, nil
}
