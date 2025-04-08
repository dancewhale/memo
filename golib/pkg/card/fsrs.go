package card

import (
	// Added for returning errors
	"errors"
	"memo/pkg/db" // Corrected import path assumed based on previous steps
	"memo/pkg/logger"
	"memo/pkg/storage"
	"sync"
	"time"

	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
)

var muteDB = sync.Mutex{}

var Scheduler = gfsrs.NewFSRS(gfsrs.DefaultParam())

// State for navigating reviewed cards
var (
	todayReviewedHeadIDs []string
	currentReviewIndex   int = -1 // -1 indicates not initialized or empty
	reviewCacheMutex     sync.Mutex
	cacheValid           bool = false // Flag to indicate if the cache is valid
)

// Function to fetch and cache today's reviewed head IDs
func fetchAndCacheReviewedIDs() error {
	reviewCacheMutex.Lock()
	defer reviewCacheMutex.Unlock()

	fsrsDB, err := db.NewFsrsDB()
	if err != nil {
		return logger.Errorf("Failed to get FsrsDB instance: %v", err)
	}
	ids, err := fsrsDB.GetTodayReviewedHeadIDs()
	if err != nil {
		currentReviewIndex = -1
		todayReviewedHeadIDs = nil
		cacheValid = false
		return logger.Errorf("Failed to get today's reviewed head IDs: %v", err)
	}
	todayReviewedHeadIDs = ids
	if len(todayReviewedHeadIDs) == 0 {
		currentReviewIndex = -1
	} else {
		// Maintain current index if possible, otherwise reset
		if currentReviewIndex >= len(todayReviewedHeadIDs) {
			currentReviewIndex = len(todayReviewedHeadIDs) - 1
		} else if currentReviewIndex < -1 { // Reset if index was somehow invalid (< -1)
			currentReviewIndex = -1 // Let GetNext/Previous handle initial state
		}
		// If the index was -1, it remains -1, indicating start state for GetNext/Previous
	}
	cacheValid = true
	logger.Infof("Fetched and cached %d reviewed head IDs for today.", len(todayReviewedHeadIDs))
	return nil
}

// Function to invalidate the review cache
func invalidateReviewCache() {
	reviewCacheMutex.Lock()
	defer reviewCacheMutex.Unlock()
	cacheValid = false
	logger.Info("Review navigation cache invalidated.")
}

// ensureCacheValid ensures the review cache is loaded and valid
func ensureCacheValid() error {
	reviewCacheMutex.Lock()
	isValid := cacheValid
	reviewCacheMutex.Unlock()

	if !isValid {
		return fetchAndCacheReviewedIDs()
	}
	return nil
}

// GetNextReviewHeadID gets the next head ID from today's reviewed list
func GetNextReviewHeadID() (string, error) {
	if err := ensureCacheValid(); err != nil {
		return "", logger.Errorf("Failed to ensure review cache validity: %v", err)
	}

	reviewCacheMutex.Lock()
	defer reviewCacheMutex.Unlock()

	if len(todayReviewedHeadIDs) == 0 {
		return "", errors.New("no cards reviewed today")
	}

	currentReviewIndex++
	if currentReviewIndex >= len(todayReviewedHeadIDs) {
		currentReviewIndex = len(todayReviewedHeadIDs) - 1 // Clamp at the end
		logger.Debugf("Reached end of reviewed list, staying at index %d", currentReviewIndex)
	} else if currentReviewIndex < 0 {
		// This case might happen if the list became non-empty after being empty
		currentReviewIndex = 0
	}

	headID := todayReviewedHeadIDs[currentReviewIndex]
	logger.Debugf("GetNextReviewHeadID: index %d, ID %s", currentReviewIndex, headID)
	return headID, nil
}

// GetPreviousReviewHeadID gets the previous head ID from today's reviewed list
func GetPreviousReviewHeadID() (string, error) {
	if err := ensureCacheValid(); err != nil {
		return "", logger.Errorf("Failed to ensure review cache validity: %v", err)
	}

	reviewCacheMutex.Lock()
	defer reviewCacheMutex.Unlock()

	if len(todayReviewedHeadIDs) == 0 {
		return "", errors.New("no cards reviewed today")
	}

	// If index is -1 (initial state or empty list previously), getting previous should point to the first item.
	if currentReviewIndex == -1 {
		currentReviewIndex = 0
	} else {
		currentReviewIndex--
	}

	if currentReviewIndex < 0 {
		currentReviewIndex = 0 // Clamp at the beginning
		logger.Debugf("Reached beginning of reviewed list, staying at index %d", currentReviewIndex)
	}

	headID := todayReviewedHeadIDs[currentReviewIndex]
	logger.Debugf("GetPreviousReviewHeadID: index %d, ID %s", currentReviewIndex, headID)
	return headID, nil
}

func ReviewCard(headlineID string, rating gfsrs.Rating) error {
	invalidateReviewCache() // Invalidate cache on review
	fsrsDB, err := db.NewFsrsDB()
	if err != nil {
		return err
	}

	logger.Debugf("Start Review Headline with headlineID: %s, rating: %d", headlineID, rating)

	needReview, err := fsrsDB.IfCardIsDue(headlineID)

	if needReview && err == nil {
		fsrsInfo := fsrsDB.GetFsrsInfoByOrgID(headlineID)
		if fsrsInfo == nil {
			_ = logger.Errorf("not found card [orgid=%s] to review", headlineID)
			return nil
		}

		now := time.Now()
		schedulingInfo := Scheduler.Repeat(fsrsInfo.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := &storage.ReviewLog{HeadlineID: headlineID, ReviewLog: rLog}
		reviewlog.SetPreCard(fsrsInfo.Card)

		fsrsInfo.Card = updatedCard
		err = fsrsDB.CreateReviewLog(reviewlog)
		if err != nil {
			return err
		}

		// 清除缓存
		if cacheManager := db.GetCacheManager(); cacheManager != nil {
			err = cacheManager.InvalidateCacheByHeadlineID(headlineID)
			if err != nil {
				logger.Errorf("Invalidate Cache by headlineid %s failed: %v.", headlineID, err)
			}
		}

		return fsrsDB.UpdateFsrs(fsrsInfo)
	}
	logger.Infof("Card %s no need to review.", headlineID)
	return err
}

// UndoReviewCard 撤销最近一次复习操作
// 传入headlineID，查询关联的ReviewLog记录，按Review时间从新到旧排序
// 如果最新记录的Review时间是当天，则将headlineID关联的Fsrs记录修改为ReviewLog中的Card数据并删除该记录
// 成功返回true，否则返回false，同时处理并返回error
func UndoReviewCard(headlineID string) (bool, error) {
	fsrsDB, err := db.NewFsrsDB()
	if err != nil {
		return false, err
	}
	latestLog, err := fsrsDB.GetTodayLatestReviewLog(headlineID)
	if err != nil {
		logger.Infof("No review log found for headline %s today: %v", headlineID, err)
		return false, nil
	}

	err = fsrsDB.UndoReview(latestLog)
	if err != nil {
		return false, err
	}

	// 清除缓存
	if cacheManager := db.GetCacheManager(); cacheManager != nil {
		err = cacheManager.InvalidateCacheByHeadlineID(headlineID)
		if err != nil {
			logger.Errorf("Invalidate Cache by headlineid %s failed: %v.", headlineID, err)
		}
	}

	logger.Infof("Successfully undid review for headline %s", headlineID)
	return true, nil
}

func ScanInitFsrs() {
	go func() {
		muteDB.Lock()
		defer muteDB.Unlock()
		logger.Infof("Start to scan org for headline init.")
		fsrsDB, err := db.NewFsrsDB()
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
