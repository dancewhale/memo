package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage/dal"
	"sync"
	"time"
)

// HeadlineStats 存储headline及其子headline的统计信息
type HeadlineStats struct {
	HeadlineID     string           // headline的ID
	Info           HeadlineWithFsrs // headline和fsrs的详细信息
	TotalCards     int              // 总卡片数量
	ExpiredCards   int              // 过期卡片数量
	WaitingCards   int              // 等待学习的卡片数量
	ReviewingCards int              // 等待复习的卡片数量
	Children       []*HeadlineStats // 子headline的统计信息
}

// HeadlineWithFsrs 包含headline和fsrsInfo信息的结构体
type HeadlineWithFsrs struct {
	ID            string    `gorm:"primarykey;not null"`
	ScheduledType string    `json:"scheduled_type"`
	Type          int       `json:"type"`
	Title         string    `json:"title"`
	Hash          string    `json:"hash" hash:"ignore"`
	ParentID      *string   `json:"parent_id"`
	FileID        *string   `gorm:"primaryKey"`
	Due           time.Time `json:"Due"`
	Stability     float64   `json:"Stability"`
	Difficulty    float64   `json:"Difficulty"`
	ElapsedDays   uint64    `json:"ElapsedDays"`
	ScheduledDays uint64    `json:"ScheduledDays"`
	Reps          uint64    `json:"Reps"`
	Lapses        uint64    `json:"Lapses"`
	State         uint64    `json:"State"`
	LastReview    time.Time `json:"LastReview"`
}

type FileInfo struct {
	FileID         string // 文件ID
	FilePath       string // 文件路径
	Hash           string
	TotalCards     int // 总卡片数量
	ExpiredCards   int // 过期卡片数量
	WaitingCards   int // 等待学习的卡片数量
	ReviewingCards int // 等待复习的卡片数量
}

// FileHeadlineCache 用于缓存文件中所有headline的树形结构和统计信息
type FileHeadlineCache struct {
	Info        FileInfo
	RootHeads   []*HeadlineStats          // 根headline的统计信息
	HeadMap     map[string]*HeadlineStats // 所有headline的映射，用于快速查找
	LastUpdated time.Time                 // 最后更新时间
}

// FileHeadlineCacheManager 全局缓存管理器
type FileHeadlineCacheManager struct {
	caches map[string]*FileHeadlineCache // 文件ID到缓存的映射
	mutex  sync.RWMutex                  // 读写锁，保证并发安全
	maxAge time.Duration                 // 缓存最大有效期
}

// 全局缓存管理器实例
var cacheManager *FileHeadlineCacheManager
var cacheManagerOnce sync.Once

// GetCacheManager 获取全局缓存管理器实例
func GetCacheManager() *FileHeadlineCacheManager {
	cacheManagerOnce.Do(func() {
		cacheManager = &FileHeadlineCacheManager{
			caches: make(map[string]*FileHeadlineCache),
			maxAge: 5 * time.Minute, // 默认缓存有效期为5分钟
		}
	})
	return cacheManager
}

// NewFileHeadlineCache 创建一个新的headline缓存
func NewFileHeadlineCache(fileID string) (*FileHeadlineCache, error) {
	file := dal.File
	// 查询文件信息
	files, err := file.WithContext(context.Background()).Where(file.ID.Eq(fileID)).Find()
	if err != nil || len(files) == 0 {
		return nil, logger.Errorf("Find file %s failed: %v", fileID, err)
	}
	// 确保文件ID存在
	if len(files) == 0 {
		return nil, logger.Errorf("File %s not found", fileID)
	}
	// 创建新的headline缓存
	cache := &FileHeadlineCache{
		Info:      FileInfo{FileID: fileID, FilePath: files[0].FilePath, Hash: files[0].Hash},
		RootHeads: make([]*HeadlineStats, 0),
		HeadMap:   make(map[string]*HeadlineStats),
	}

	// 构建缓存树
	err = cache.buildCache()
	if err != nil {
		return nil, err
	}

	return cache, nil
}

// buildCache 构建headline缓存树并计算统计信息
func (c *FileHeadlineCache) buildCache() error {
	// 获取当前日期的结束时间，用于判断卡片是否过期
	_, dayEnd := GetDayTime(0)

	// 一次性查询所有headline和fsrsInfo信息
	fsrs := dal.FsrsInfo
	headline := dal.Headline

	// 创建一个临时结构体切片用于存储查询结果
	var results []HeadlineWithFsrs

	// 执行联合查询，获取所有headline和对应的fsrsInfo
	err := headline.WithContext(context.Background()).
		Select(headline.ALL, fsrs.ALL).
		LeftJoin(fsrs, headline.ID.EqCol(fsrs.HeadlineID)).
		Where(headline.FileID.Eq(c.Info.FileID)).
		Scan(&results)

	if err != nil {
		return logger.Errorf("Query headline and fsrs info error: %v", err)
	}

	// 创建headline树结构
	headlineMap := make(map[string]HeadlineWithFsrs)
	parentChildrenMap := make(map[string][]string)

	// 第一遍遍历，构建ID到headline的映射
	for _, result := range results {
		headlineID := result.ID

		// 存储headline信息
		if _, exists := headlineMap[headlineID]; !exists {
			headlineMap[headlineID] = result

			// 初始化每个headline的统计信息
			c.HeadMap[headlineID] = &HeadlineStats{
				HeadlineID: headlineID,
				Info:       result,
				Children:   make([]*HeadlineStats, 0),
			}

			// 记录父子关系
			if result.ParentID != nil && *result.ParentID != "" {
				parentID := *result.ParentID
				if _, ok := parentChildrenMap[parentID]; !ok {
					parentChildrenMap[parentID] = make([]string, 0)
				}
				parentChildrenMap[parentID] = append(parentChildrenMap[parentID], headlineID)
			} else {
				// 根headline
				c.RootHeads = append(c.RootHeads, c.HeadMap[headlineID])
			}
		}

		// 处理fsrsInfo信息，计算统计数据
		if stats, ok := c.HeadMap[headlineID]; ok {
			// 总卡片数量+1
			stats.TotalCards++

			// 根据State判断卡片类型
			if result.State == 0 {
				// 等待学习的卡片
				stats.WaitingCards++
			} else {
				// 判断是否是等待复习的卡片
				if result.Due.Before(dayEnd) {
					stats.ReviewingCards++
				}

				// 判断是否是过期卡片
				if result.Due.Before(time.Now()) {
					stats.ExpiredCards++
				}
			}
		}
	}

	// 第二遍遍历，构建树结构
	for parentID, childrenIDs := range parentChildrenMap {
		if parentStats, ok := c.HeadMap[parentID]; ok {
			for _, childID := range childrenIDs {
				if childStats, ok := c.HeadMap[childID]; ok {
					parentStats.Children = append(parentStats.Children, childStats)
				}
			}
		}
	}

	// 递归计算每个根headline的统计信息
	for _, rootHead := range c.RootHeads {
		c.aggregateStats(rootHead)
	}

	c.Info.TotalCards, c.Info.ExpiredCards, c.Info.WaitingCards, c.Info.ReviewingCards = c.getFileStats()
	return nil
}

// aggregateStats 递归聚合headline及其子headline的统计信息
func (c *FileHeadlineCache) aggregateStats(stats *HeadlineStats) {
	for _, child := range stats.Children {
		c.aggregateStats(child)

		// 将子headline的统计信息累加到父headline
		stats.TotalCards += child.TotalCards
		stats.ExpiredCards += child.ExpiredCards
		stats.WaitingCards += child.WaitingCards
		stats.ReviewingCards += child.ReviewingCards
	}
}

// GetHeadlineStats 获取指定headline的统计信息
func (c *FileHeadlineCache) getHeadlineStats(headlineID string) *HeadlineStats {
	return c.HeadMap[headlineID]
}

// GetFileStats 获取整个文件的统计信息
func (c *FileHeadlineCache) getFileStats() (totalCards, expiredCards, waitingCards, reviewingCards int) {
	for _, rootHead := range c.RootHeads {
		totalCards += rootHead.TotalCards
		expiredCards += rootHead.ExpiredCards
		waitingCards += rootHead.WaitingCards
		reviewingCards += rootHead.ReviewingCards
	}
	return
}

// GetFileFromCache 从缓存管理器获取指定文件的缓存，如果不存在或已过期则创建新缓存
func (m *FileHeadlineCacheManager) GetFileFromCache(fileID string) (*FileHeadlineCache, error) {
	// 先尝试读取缓存
	m.mutex.RLock()
	cache, exists := m.caches[fileID]
	m.mutex.RUnlock()

	// 如果缓存存在且未过期，直接返回
	if exists && time.Since(cache.LastUpdated) < m.maxAge {
		return cache, nil
	}

	// 缓存不存在或已过期，创建新缓存
	m.mutex.Lock()
	defer m.mutex.Unlock()

	// 双重检查，避免在获取锁的过程中其他协程已经创建了缓存
	cache, exists = m.caches[fileID]
	if exists && time.Since(cache.LastUpdated) < m.maxAge {
		return cache, nil
	}

	// 创建新缓存
	newCache, err := NewFileHeadlineCache(fileID)
	if err != nil {
		return nil, err
	}

	// 设置最后更新时间
	newCache.LastUpdated = time.Now()

	// 更新缓存
	m.caches[fileID] = newCache
	return newCache, nil
}

// GetFilesFromCache 从缓存管理器获取多个指定文件的缓存，如果不存在或已过期则创建新缓存
func (m *FileHeadlineCacheManager) GetFilesFromCache(fileIDs []string) ([]*FileInfo, error) {
	result := make([]*FileInfo, 0, len(fileIDs))
	var firstErr error

	// 遍历所有fileID，获取各自的缓存
	for _, fileID := range fileIDs {
		cache, err := m.GetFileFromCache(fileID)
		if err != nil {
			// 记录第一个错误，但继续处理其他文件
			if firstErr == nil {
				firstErr = logger.Errorf("Get file %s cache failed: %v", fileID, err)
			}
			continue
		}
		result = append(result, &cache.Info)
	}

	// 如果没有成功获取任何缓存且有错误发生，则返回错误
	if len(result) == 0 && firstErr != nil {
		return nil, firstErr
	}

	return result, firstErr
}

// InvalidateCache 使指定文件的缓存失效
func (m *FileHeadlineCacheManager) InvalidateCache(fileID string) {
	m.mutex.Lock()
	delete(m.caches, fileID)
	m.mutex.Unlock()
}

// InvalidateCacheByHeadlineID 根据HeadlineID使相关缓存失效
func (m *FileHeadlineCacheManager) InvalidateCacheByHeadlineID(headlineID string) error {
	// 查询headline所属的文件ID
	headline := dal.Headline
	heads, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headlineID)).Find()
	if err != nil || len(heads) == 0 {
		return logger.Errorf("Find headline %s failed: %v", headlineID, err)
	}

	// 使相关文件的缓存失效
	if heads[0].FileID != nil {
		m.InvalidateCache(*heads[0].FileID)
	}

	return nil
}

// GetFileStatsByHeadlineID 获取指定headline及其所有子headline的统计信息
func GetFileStatsByHeadlineID(fileID string, headlineID string) (*HeadlineStats, error) {
	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileFromCache(fileID)
	if err != nil {
		return nil, err
	}

	stats := cache.getHeadlineStats(headlineID)
	if stats == nil {
		return nil, logger.Errorf("Headline %s not found", headlineID)
	}

	return stats, nil
}

// GetFileStats 获取整个文件的统计信息
func GetFileStats(fileID string) (totalCards, expiredCards, waitingCards, reviewingCards int, err error) {
	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileFromCache(fileID)
	if err != nil {
		return 0, 0, 0, 0, err
	}

	return cache.Info.TotalCards, cache.Info.ExpiredCards, cache.Info.WaitingCards, cache.Info.ReviewingCards, nil
}

// getChildrenIDs 递归获取指定headline的所有子headline ID
func (c *FileHeadlineCache) getChildrenIDs(headlineID string) []string {
	var childrenIDs []string

	// 获取当前headline的统计信息
	stats := c.getHeadlineStats(headlineID)
	if stats == nil {
		return childrenIDs
	}

	// 递归获取所有子headline的ID
	for _, child := range stats.Children {
		childrenIDs = append(childrenIDs, child.HeadlineID)
		childrenIDs = append(childrenIDs, c.getChildrenIDs(child.HeadlineID)...)
	}

	return childrenIDs
}

// GetHeadIDByAncestorID 获取指定headline及其所有子headline的ID列表
func GetHeadIDByAncestorID(ancestorID string) []string {
	var headIDs []string

	// 先将自身ID添加到结果中
	headIDs = append(headIDs, ancestorID)

	// 查询headline所属的文件ID
	headline := dal.Headline
	heads, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(ancestorID)).Find()
	if err != nil || len(heads) == 0 {
		return headIDs
	}

	// 确保文件ID存在
	if heads[0].FileID == nil {
		return headIDs
	}

	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileFromCache(*heads[0].FileID)
	if err != nil {
		return headIDs
	}

	// 获取所有子headline的ID
	childrenIDs := cache.getChildrenIDs(ancestorID)
	headIDs = append(headIDs, childrenIDs...)

	return headIDs
}

// GetHeadIDByAncestorIDs 获取多个headline及其所有子headline的ID列表
func GetHeadIDByAncestorIDs(ancestorIDs []string) []string {
	var headIDs []string

	// 遍历所有ancestorID，获取各自的子headline ID
	for _, ancestorID := range ancestorIDs {
		headIDs = append(headIDs, GetHeadIDByAncestorID(ancestorID)...)
	}

	// 去重
	uniqMap := make(map[string]bool)
	var result []string

	for _, id := range headIDs {
		if !uniqMap[id] {
			uniqMap[id] = true
			result = append(result, id)
		}
	}

	return result
}
