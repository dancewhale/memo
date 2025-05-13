package db

import (
	"context"
	"errors"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"sync"
	"time"

	"github.com/emirpasic/gods/lists/doublylinkedlist"
	"github.com/samber/lo"
)

const (
	defaultCacheDuration = 10 * time.Hour // 默认文件卡片列表缓存有效期
)

// FileCardList 存储单个文件的卡片列表及其阅读状态
type FileCardList struct {
	FileID           string
	HeadList         *doublylinkedlist.List // 存储 headID (string)
	CurrentHeadIndex int
	ExpirationTime   time.Time
	ReadFinished     bool // 是否已完成阅读
}

// ReadingQueue 管理多个文件的阅读列表队列
type ReadingQueue struct {
	FileLists        []*FileCardList // 按顺序存储文件的卡片列表
	CurrentFileIndex int             // 当前正在阅读的文件在 FileLists 中的索引
	mutex            sync.RWMutex
	cacheDuration    time.Duration
}

var (
	globalReadingQueue *ReadingQueue
	once               sync.Once
)

// GetReadingQueue 获取全局唯一的 ReadingQueue 实例
func GetReadingQueue() *ReadingQueue {
	once.Do(func() {
		globalReadingQueue = &ReadingQueue{
			FileLists:        make([]*FileCardList, 0),
			CurrentFileIndex: -1, // 初始表示没有选中的文件
			cacheDuration:    defaultCacheDuration,
		}
		// 可以在这里调用 InitQueue，或者让用户显式调用
		if err := globalReadingQueue.InitQueue(); err != nil {
			logger.Errorf("Failed to initialize reading queue: %v", err)
		}
	})
	return globalReadingQueue
}

// buildCardMapRecursive 递归构建卡片ID列表，基于HeadlineStats的层级结构
// head参数是来自filecache.go中的HeadlineStats类型
func buildCardMapRecursive(list *doublylinkedlist.List, head *HeadlineStats) {
	if head == nil || head.Info == nil {
		return
	}
	list.Add(head.Info.ID) // 添加当前 head 的 ID

	// 递归处理实体文件子节点 (按Order排序)
	for _, item := range head.FileChildren {
		buildCardMapRecursive(list, item)
	}
	// 递归处理虚拟子节点 (按Order排序)
	for _, item := range head.VirtChildren {
		buildCardMapRecursive(list, item)
	}
}

// buildCardListForFile 为指定文件构建完整的卡片ID列表
func buildCardListForFile(rootHeads []*HeadlineStats) *doublylinkedlist.List {
	list := doublylinkedlist.New()
	for _, rootHead := range rootHeads {
		buildCardMapRecursive(list, rootHead)
	}
	return list
}

// InitQueue 初始化阅读队列，加载所有包含新卡片的文件
func (rq *ReadingQueue) InitQueue() error {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	fileIDs, err := GetFileHasNewCard()
	if err != nil {
		return logger.Errorf("Error getting files with new cards: %v", err)
	}

	rq.FileLists = make([]*FileCardList, 0, len(fileIDs))
	for _, fileID := range fileIDs {
		fcl, err := rq.initializeFileCardList(fileID)
		if err != nil {
			logger.Warnf("Error initializing card list for file %s: %v. Skipping this file.", fileID, err)
			continue
		}
		rq.FileLists = append(rq.FileLists, fcl)
	}

	if len(rq.FileLists) > 0 {
		rq.CurrentFileIndex = 0
	} else {
		rq.CurrentFileIndex = -1
	}
	return nil
}

// initializeFileCardList (内部方法) 为指定 fileID 初始化或刷新 FileCardList
// oldHeadIDIfRefreshing 用于在刷新时尝试定位到之前的卡片
func (rq *ReadingQueue) initializeFileCardList(fileID string, oldHeadIDIfRefreshing ...string) (*FileCardList, error) {
	cacheMgr := GetCacheManager()
	fileCache, err := cacheMgr.GetFileCacheFromCache(fileID) // GetFileCacheFromCache 来自 filecache.go
	if err != nil {
		return nil, logger.Errorf("Failed to get file cache for %s: %v", fileID, err)
	}

	headList := buildCardListForFile(fileCache.RootHeads)
	if headList.Empty() {
		// 文件中没有卡片，可以记录日志或返回一个空的列表
		logger.Infof("No cards found in file %s after building list.", fileID)
	}

	newFCL := &FileCardList{
		FileID:           fileID,
		HeadList:         headList,
		CurrentHeadIndex: 0,
		ExpirationTime:   time.Now().Add(rq.cacheDuration),
		ReadFinished:     false,
	}

	if len(oldHeadIDIfRefreshing) > 0 && oldHeadIDIfRefreshing[0] != "" {
		oldHeadID := oldHeadIDIfRefreshing[0]
		index := newFCL.HeadList.IndexOf(oldHeadID)
		if index != -1 {
			newFCL.CurrentHeadIndex = index
		} else {
			logger.Warnf("Old headID %s not found in refreshed list for file %s. Resetting to index 0.", oldHeadID, fileID)
		}
	}

	return newFCL, nil
}

// ensureFileCardListValid (内部方法) 确保当前文件索引处的 FileCardList 是有效的（未过期）
// 如果过期，则刷新它
func (rq *ReadingQueue) ensureFileCardListValid(fileIndex int) (*FileCardList, error) {
	if fileIndex < 0 || fileIndex >= len(rq.FileLists) {
		return nil, errors.New("file index out of bounds")
	}

	fcl := rq.FileLists[fileIndex]
	var oldHeadID string
	if fcl.HeadList != nil && !fcl.HeadList.Empty() && fcl.CurrentHeadIndex >= 0 && fcl.CurrentHeadIndex < fcl.HeadList.Size() {
		val, found := fcl.HeadList.Get(fcl.CurrentHeadIndex)
		if found {
			oldHeadID = val.(string)
		}
	}

	if time.Now().After(fcl.ExpirationTime) {
		logger.Infof("Card list for file %s expired. Refreshing.", fcl.FileID)
		newFCL, err := rq.initializeFileCardList(fcl.FileID, oldHeadID)
		if err != nil {
			return nil, logger.Errorf("Failed to refresh card list for file %s: %v", fcl.FileID, err)
		}
		rq.FileLists[fileIndex] = newFCL
		return newFCL, nil
	}
	return fcl, nil
}

func (rq *ReadingQueue) IfCurrentFileReadFinished() (bool, error) {
	rq.mutex.RLock()
	defer rq.mutex.RUnlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		return false, errors.New("no file selected or queue empty")
	}
	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return false, err
	}
	for i := 0; i < fcl.HeadList.Size(); i++ {
		val, found := fcl.HeadList.Get(i)
		if !found {
			return false, errors.New("failed to get head ID at current index")
		}
		headID := val.(string)
		isNew, errIsNew := rq.isCardNew(headID)
		if errIsNew != nil {
			logger.Warnf("Error checking if card %s is new: %v. Skipping.", headID, errIsNew)
			continue
		}
		if isNew {
			return false, nil
		}
	}
	rq.FileLists[rq.CurrentFileIndex].ReadFinished = true
	return true, nil
}

// GetCurrentFileID 获取当前正在阅读的文件ID
func (rq *ReadingQueue) GetCurrentFileID() (string, error) {
	rq.mutex.RLock()
	defer rq.mutex.RUnlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		return "", errors.New("no file is currently selected or queue is empty")
	}
	return rq.FileLists[rq.CurrentFileIndex].FileID, nil
}

// NextFileID 切换到下一个文件，并返回其ID。如果到达末尾则循环到开头。
func (rq *ReadingQueue) NextFileID() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if len(rq.FileLists) == 0 {
		return "", errors.New("reading queue is empty")
	}

	// 如果到达末尾则循环到开头,如果只有一个文件则不切换
	rq.CurrentFileIndex = (rq.CurrentFileIndex + 1) % len(rq.FileLists)
	if len(rq.FileLists) == 1 && rq.CurrentFileIndex == 0 {
		return "", errors.New("only one file in the queue, no switch needed")
	}
	// 确保新选中的文件列表有效
	_, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}
	_, err = rq.IfCurrentFileReadFinished()
	if err != nil {
		return "", err
	}
	return rq.FileLists[rq.CurrentFileIndex].FileID, nil
}

// PreviousFileID 切换到上一个文件，并返回其ID。如果到达开头则循环到末尾。
func (rq *ReadingQueue) PreviousFileID() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if len(rq.FileLists) == 0 {
		return "", errors.New("reading queue is empty")
	}

	// 如果只有一个文件则不切换
	if len(rq.FileLists) == 1 && rq.CurrentFileIndex == 0 {
		return "", errors.New("only one file in the queue, no switch needed")
	}
	// 如果到达开头则循环到末尾。
	rq.CurrentFileIndex--
	if rq.CurrentFileIndex < 0 {
		rq.CurrentFileIndex = len(rq.FileLists) - 1
	}
	// 确保新选中的文件列表有效
	_, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}

	_, err = rq.IfCurrentFileReadFinished()
	if err != nil {
		return "", err
	}
	return rq.FileLists[rq.CurrentFileIndex].FileID, nil
}

// GetCurrentHeadID 获取当前文件当前卡片的ID
func (rq *ReadingQueue) GetCurrentHeadID() (string, error) {
	rq.mutex.RLock()
	defer rq.mutex.RUnlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		return "", errors.New("no file selected or queue empty")
	}

	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex) // 加 RLock 后再调用，或重构 ensureFileCardListValid
	if err != nil {
		return "", err
	}

	if fcl.HeadList.Empty() {
		return "", errors.New("current file has no cards")
	}
	if fcl.CurrentHeadIndex < 0 || fcl.CurrentHeadIndex >= fcl.HeadList.Size() {
		return "", errors.New("current head index out of bounds")
	}

	val, found := fcl.HeadList.Get(fcl.CurrentHeadIndex)
	if !found {
		return "", errors.New("failed to get head ID at current index")
	}
	return val.(string), nil
}

// NextHeadID 切换到当前文件的下一个卡片ID。如果到达文件末尾，则切换到下一个文件。
func (rq *ReadingQueue) NextHeadID() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		// 尝试初始化队列或返回错误
		if err := rq.initQueueIfNeeded(); err != nil {
			return "", err
		}
		if rq.CurrentFileIndex < 0 { // 初始化后仍为空
			return "", errors.New("queue is empty after initialization attempt")
		}
	}

	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}

	if fcl.HeadList.Empty() {
		// 当前文件没有卡片，尝试切换到下一个文件
		logger.Infof("File %s has no cards, attempting to move to next file.", fcl.FileID)
		return rq.switchToNextFileAndGetFirstHead()
	}

	fcl.CurrentHeadIndex++
	if fcl.CurrentHeadIndex >= fcl.HeadList.Size() {
		// 到达当前文件卡片列表末尾，切换到下一个文件
		return rq.switchToNextFileAndGetFirstHead()
	}

	val, _ := fcl.HeadList.Get(fcl.CurrentHeadIndex)
	return val.(string), nil
}

// switchToNextFileAndGetFirstHead 是 NextHeadID 和 PreviousHeadID 的辅助函数
func (rq *ReadingQueue) switchToNextFileAndGetFirstHead() (string, error) {
	// 注意：这个辅助函数应该在已经持有锁的上下文中被调用
	oldFileIndex := rq.CurrentFileIndex
	rq.CurrentFileIndex = (rq.CurrentFileIndex + 1) % len(rq.FileLists)

	// 如果只有一个文件并且我们循环回了它，并且这个文件是空的，那么我们没有卡片可以返回
	if rq.CurrentFileIndex == oldFileIndex && rq.FileLists[rq.CurrentFileIndex].HeadList.Empty() {
		return "", errors.New("no reviewable cards in any file")
	}

	newFCL, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}
	newFCL.CurrentHeadIndex = 0 // 新文件的第一个卡片
	if newFCL.HeadList.Empty() {
		// 如果新文件也没有卡片，这可能意味着所有文件都没有卡片，或者需要更复杂的逻辑来跳过空文件
		// 为简单起见，这里我们假设至少有一个文件有卡片，或者 InitQueue 会处理空文件的情况
		// 在更健壮的实现中，这里可能需要一个循环来找到下一个非空文件
		return "", errors.New("switched to a new file, but it also has no cards")
	}
	val, _ := newFCL.HeadList.Get(newFCL.CurrentHeadIndex)
	return val.(string), nil
}

// PreviousHeadID 切换到当前文件的上一个卡片ID。如果到达文件开头，则切换到上一个文件。
func (rq *ReadingQueue) PreviousHeadID() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		if err := rq.initQueueIfNeeded(); err != nil {
			return "", err
		}
		if rq.CurrentFileIndex < 0 {
			return "", errors.New("queue is empty after initialization attempt")
		}
	}

	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}

	if fcl.HeadList.Empty() {
		logger.Infof("File %s has no cards, attempting to move to previous file.", fcl.FileID)
		return rq.switchToPreviousFileAndGetLastHead()
	}

	fcl.CurrentHeadIndex--
	if fcl.CurrentHeadIndex < 0 {
		// 到达当前文件卡片列表开头，切换到上一个文件
		return rq.switchToPreviousFileAndGetLastHead()
	}

	val, _ := fcl.HeadList.Get(fcl.CurrentHeadIndex)
	return val.(string), nil
}

func (rq *ReadingQueue) switchToPreviousFileAndGetLastHead() (string, error) {
	// 注意：这个辅助函数应该在已经持有锁的上下文中被调用
	oldFileIndex := rq.CurrentFileIndex
	rq.CurrentFileIndex--
	if rq.CurrentFileIndex < 0 {
		rq.CurrentFileIndex = len(rq.FileLists) - 1
	}

	if rq.CurrentFileIndex == oldFileIndex && rq.FileLists[rq.CurrentFileIndex].HeadList.Empty() {
		return "", errors.New("no reviewable cards in any file")
	}

	newFCL, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}
	if newFCL.HeadList.Empty() {
		return "", errors.New("switched to a previous file, but it also has no cards")
	}
	newFCL.CurrentHeadIndex = newFCL.HeadList.Size() - 1 // 新文件的最后一个卡片
	val, _ := newFCL.HeadList.Get(newFCL.CurrentHeadIndex)
	return val.(string), nil
}

// initQueueIfNeeded (内部方法) 如果队列未初始化，则进行初始化
func (rq *ReadingQueue) initQueueIfNeeded() error {
	// 应该在持有写锁的情况下调用
	if len(rq.FileLists) == 0 || rq.CurrentFileIndex == -1 {
		logger.Info("Reading queue is empty or not initialized. Initializing now.")
		// 释放读锁（如果当前是读锁），获取写锁，然后重新检查条件
		// 这里简化处理：假设调用者已经处理了锁的升级，或者这是一个顶级调用
		// 在实际并发场景中，锁的管理需要更细致
		fileIDs, err := GetFileHasNewCard()
		if err != nil {
			return logger.Errorf("Error getting files with new cards during lazy init: %v", err)
		}

		rq.FileLists = make([]*FileCardList, 0, len(fileIDs))
		for _, fileID := range fileIDs {
			fcl, errInit := rq.initializeFileCardList(fileID)
			if errInit != nil {
				logger.Warnf("Error initializing card list for file %s during lazy init: %v. Skipping.", fileID, errInit)
				continue
			}
			rq.FileLists = append(rq.FileLists, fcl)
		}

		if len(rq.FileLists) > 0 {
			rq.CurrentFileIndex = 0
		} else {
			rq.CurrentFileIndex = -1
			return errors.New("no files with new cards found during lazy initialization")
		}
	}
	return nil
}

func (rq *ReadingQueue) isCardNew(headID string) (bool, error) {
	head, ok := headCacheMap[headID]
	if ok {
		if head.Info.State == 0 && head.Info.ScheduledType == storage.NORMAL {
			return true, nil
		} else {
			return false, nil
		}
	}
	return false, errors.New("headID not found in cache")
}

// SetCurrentCard 允许外部设置当前的阅读位置
func (rq *ReadingQueue) SetCurrentCard(fileID string, headID string) error {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	foundFile := false
	for i, fcl := range rq.FileLists {
		if fcl.FileID == fileID {
			rq.CurrentFileIndex = i
			foundFile = true
			// 确保文件列表是最新的
			updatedFCL, err := rq.ensureFileCardListValid(i)
			if err != nil {
				return logger.Errorf("Failed to ensure card list valid for %s: %v", fileID, err)
			}

			index := updatedFCL.HeadList.IndexOf(headID)
			if index != -1 {
				updatedFCL.CurrentHeadIndex = index
			} else {
				return logger.Errorf("HeadID %s not found in file %s", headID, fileID)
			}
			break
		}
	}

	if !foundFile {
		return logger.Errorf("FileID %s not found in the reading queue", fileID)
	}
	return nil
}

func (rq *ReadingQueue) SetCacheDuration(duration time.Duration) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()
	rq.cacheDuration = duration
}

func (rq *ReadingQueue) NextNewCard() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		if err := rq.initQueueIfNeeded(); err != nil {
			return "", err
		}
		if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
			return "", errors.New("reading queue is empty or not initialized")
		}
	}

	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}

	if fcl.HeadList.Empty() {
		return "", nil // No cards in the current file, return empty string as requested
	}

	startIndex := fcl.CurrentHeadIndex + 1
	for i := startIndex; i < fcl.HeadList.Size(); i++ {
		val, found := fcl.HeadList.Get(i)
		if !found {
			return "", errors.New("failed to get headID from list")
		}
		headID := val.(string)
		isNew, errIsNew := rq.isCardNew(headID)
		if errIsNew != nil {
			logger.Warnf("Error checking if card %s is new: %v. Skipping.", headID, errIsNew)
			continue
		}
		if isNew {
			fcl.CurrentHeadIndex = i
			return headID, nil
		}
	}

	return "", nil // No new card found in the rest of the current file
}

func (rq *ReadingQueue) PreviousNewCard() (string, error) {
	rq.mutex.Lock()
	defer rq.mutex.Unlock()

	if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
		if err := rq.initQueueIfNeeded(); err != nil {
			return "", err
		}
		if rq.CurrentFileIndex < 0 || rq.CurrentFileIndex >= len(rq.FileLists) {
			return "", errors.New("reading queue is empty or not initialized")
		}
	}

	fcl, err := rq.ensureFileCardListValid(rq.CurrentFileIndex)
	if err != nil {
		return "", err
	}

	if fcl.HeadList.Empty() {
		return "", nil // No cards in the current file, return empty string as requested
	}

	startIndex := fcl.CurrentHeadIndex - 1
	for i := startIndex; i >= 0; i-- {
		val, found := fcl.HeadList.Get(i)
		if !found {
			return "", errors.New("failed to get headID from list")
		}
		headID := val.(string)
		isNew, errIsNew := rq.isCardNew(headID)
		if errIsNew != nil {
			logger.Warnf("Error checking if card %s is new: %v. Skipping.", headID, errIsNew)
			continue
		}
		if isNew {
			fcl.CurrentHeadIndex = i
			return headID, nil
		}
	}

	return "", nil // No new card found before the current one in this file
}

// GetFileHasNewCard 获取所有包含新卡片（State=0）的文件ID列表
// 这个函数保持不变，因为它被 InitQueue 使用
func GetFileHasNewCard() ([]string, error) {
	head := dal.Headline
	fsrs := dal.FsrsInfo

	heads, err := head.WithContext(context.Background()).
		Join(fsrs, head.ID.EqCol(fsrs.HeadlineID)).
		Select(head.FileID).Distinct().
		Where(fsrs.State.Eq(0)).Where(head.FileID.IsNotNull()).Where(head.FileID.Neq("")).
		Find()
	if err != nil {
		return nil, logger.Errorf("Get file has new card error: %v", err)
	}
	fileIDList := lo.Map(heads, func(item *storage.Headline, index int) string {
		return *item.FileID
	})

	return fileIDList, nil
}
