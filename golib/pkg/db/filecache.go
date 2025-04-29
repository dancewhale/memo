package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage/dal"
	"sort"
	"sync"
	"time"
)

// HeadlineStats 存储headline及其子headline的统计信息
type HeadlineStats struct {
	Info     *HeadlineWithFsrs // headline和fsrs的详细信息
	Children []*HeadlineStats  // 子headline的统计信息
}

// HeadlineWithFsrs 包含headline和fsrsInfo信息的结构体
type HeadlineWithFsrs struct {
	ID            string    `gorm:"primarykey;not null"`
	Path          []string  `gorm:"-"`
	Weight        int64     `json:"weight"`
	Source        string    `json:"source"`
	ScheduledType string    `json:"scheduled_type"`
	Title         string    `json:"title"`
	Hash          string    `json:"hash" hash:"ignore"`
	ParentID      *string   `json:"parent_id"`
	FileID        *string   `gorm:"primaryKey"`
	HeadlineID    *string   `json:"headline_id"`
	VirtFileHash  *string   `json:"virt_file_hash"`
	Level         int       `json:"level"`
	Order         int       `json:"order"`
	Status        string    `json:"status"`
	Priority      string    `json:"priority"`
	Due           time.Time `json:"Due"`
	Stability     float64   `json:"Stability"`
	Difficulty    float64   `json:"Difficulty"`
	ElapsedDays   uint64    `json:"ElapsedDays"`
	ScheduledDays uint64    `json:"ScheduledDays"`
	Reps          uint64    `json:"Reps"`
	Lapses        uint64    `json:"Lapses"`
	State         uint64    `json:"State"`
	NeedReview    bool      `json:"NeedReview"`
	LastReview    time.Time `json:"LastReview"`

	ChildVirtCards int // 下级虚拟卡片数量
	TotalCards     int // 总卡片数量
	TotalVirtCards int // 总虚拟卡片数量
	ExpiredCards   int // 超期未复习的卡片数量
	WaitingCards   int // 新创建等待第一次阅读的卡片数量
	ReviewingCards int // 今天等待复习的卡片数量
}

type FileInfo struct {
	FileID         string // 文件ID
	FilePath       string // 文件路径
	Hash           string
	TotalCards     int // 总卡片数量
	TotalVirtCards int // 总虚拟卡片数量
	ExpiredCards   int // 超期未复习的卡片数量
	WaitingCards   int // 新创建等待第一次阅读的卡片数量
	ReviewingCards int // 今天等待复习的卡片数量
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
		Info:        FileInfo{FileID: fileID, FilePath: files[0].FilePath, Hash: files[0].Hash},
		RootHeads:   make([]*HeadlineStats, 0),
		HeadMap:     make(map[string]*HeadlineStats),
		LastUpdated: time.Time{},
	}

	// 构建缓存树
	err = cache.buildCache()
	if err != nil {
		return nil, err
	}

	return cache, nil
}

// fetchHeadlineAndFsrsData 从数据库获取指定文件的 headline 和 FSRS 信息
func fetchHeadlineAndFsrsData(fileID string) ([]*HeadlineWithFsrs, error) {
	fsrs := dal.FsrsInfo
	headline := dal.Headline

	var results []*HeadlineWithFsrs
	err := headline.WithContext(context.Background()).
		Select(headline.ALL, fsrs.ALL).
		LeftJoin(fsrs, headline.ID.EqCol(fsrs.HeadlineID)).
		Where(headline.FileID.Eq(fileID)).
		Scan(&results)

	if err != nil {
		return nil, logger.Errorf("Query headline and fsrs info for file %s error: %v", fileID, err)
	}
	return results, nil
}

// initializeCacheMaps 根据查询结果初始化缓存映射和计算初始统计信息
func (c *FileHeadlineCache) initializeCacheMaps(results []*HeadlineWithFsrs, dayStart, dayEnd time.Time) (map[string][]string, error) {
	headlineMap := make(map[string]*HeadlineWithFsrs)
	parentChildrenMap := make(map[string][]string)

	for _, result := range results {
		// 存储 headline 信息
		if _, exists := headlineMap[result.ID]; !exists {
			headlineMap[result.ID] = result

			// 初始化每个 headline 的统计信息
			c.HeadMap[result.ID] = &HeadlineStats{
				Info:     result,
				Children: make([]*HeadlineStats, 0),
			}

			// 记录父子关系
			if result.ParentID != nil && *result.ParentID != "" {
				parentID := *result.ParentID
				if _, ok := parentChildrenMap[parentID]; !ok {
					parentChildrenMap[parentID] = make([]string, 0)
				}
				parentChildrenMap[parentID] = append(parentChildrenMap[parentID], result.ID)
			} else {
				// 根 headline
				c.RootHeads = append(c.RootHeads, c.HeadMap[result.ID])
			}
		}

		// 处理 fsrsInfo 信息，计算统计数据
		if stats, ok := c.HeadMap[result.ID]; ok {
			// 总卡片数量+1
			stats.Info.TotalCards++
			if result.HeadlineID != nil {
				// 虚拟卡片数量+1
				stats.Info.TotalVirtCards++
			}

			// 根据 State 判断卡片类型
			if result.State == 0 {
				// 等待学习的卡片
				stats.Info.WaitingCards++
				stats.Info.NeedReview = false
			} else {
				// 判断是否是今天等待复习的卡片
				if result.Due.Before(dayEnd) && result.Due.After(dayStart) {
					stats.Info.ReviewingCards++
					stats.Info.NeedReview = true
				}

				// 判断是否是过期卡片
				if result.Due.Before(dayStart) {
					stats.Info.ExpiredCards++
					stats.Info.NeedReview = true
				}
				if result.Due.After(dayEnd) {
					stats.Info.NeedReview = false
				}
			}
		}
	}
	return parentChildrenMap, nil
}

// buildHeadlinePaths 构建 headline 的路径信息
func (c *FileHeadlineCache) buildHeadlinePaths() {
	levelMap := make(map[int][]string)
	maxLevel := 1

	// 按 level 对 headline 进行分组
	for headID, stats := range c.HeadMap {
		level := stats.Info.Level
		if level > maxLevel {
			maxLevel = level
		}
		if _, ok := levelMap[level]; !ok {
			levelMap[level] = make([]string, 0)
		}
		levelMap[level] = append(levelMap[level], headID)
	}

	// 从 level 1 开始，逐层构建 Path
	for level := 1; level <= maxLevel; level++ {
		headIDs, ok := levelMap[level]
		if !ok {
			continue
		}

		for _, headID := range headIDs {
			stats, ok := c.HeadMap[headID]
			if !ok {
				continue
			}

			if level == 1 {
				stats.Info.Path = []string{c.Info.FileID}
			}

			if stats.Info.ParentID != nil && *stats.Info.ParentID != "" {
				parentID := *stats.Info.ParentID
				if parentStats, ok := c.HeadMap[parentID]; ok && len(parentStats.Info.Path) > 0 {
					stats.Info.Path = append(append([]string{}, parentStats.Info.Path...), parentID)
				}
			}
		}
	}
}

// buildHeadlineTree 构建 headline 的树形结构
func (c *FileHeadlineCache) buildHeadlineTree(parentChildrenMap map[string][]string) {
	for parentID, childrenIDs := range parentChildrenMap {
		if parentStats, ok := c.HeadMap[parentID]; ok {
			for _, childID := range childrenIDs {
				if childStats, ok := c.HeadMap[childID]; ok {
					parentStats.Children = append(parentStats.Children, childStats)
				}
			}
			// 按 Order 值从小到大排序 Children 数组
			sort.Slice(parentStats.Children, func(i, j int) bool {
				return parentStats.Children[i].Info.Order < parentStats.Children[j].Info.Order
			})
		}
	}
}

// buildCache 构建headline缓存树并计算统计信息
func (c *FileHeadlineCache) buildCache() error {
	// 获取当前日期的起止时间
	dayStart, dayEnd := GetDayTime(0)

	// 1. 获取数据
	results, err := fetchHeadlineAndFsrsData(c.Info.FileID)
	if err != nil {
		return err
	}

	// 2. 初始化映射和计算初始统计
	parentChildrenMap, err := c.initializeCacheMaps(results, dayStart, dayEnd)
	if err != nil {
		// 虽然当前 initializeCacheMaps 不会返回 error，但保留以防未来修改
		return err
	}

	// 3. 构建路径
	c.buildHeadlinePaths()

	// 4. 构建树结构
	c.buildHeadlineTree(parentChildrenMap)

	// 5. 聚合统计信息
	for _, rootHead := range c.RootHeads {
		c.aggregateStats(rootHead)
	}

	// 6. 计算文件整体统计信息
	c.Info.TotalCards, c.Info.TotalVirtCards, c.Info.ExpiredCards, c.Info.WaitingCards, c.Info.ReviewingCards = c.getFileStats()

	return nil
}

// aggregateStats 递归聚合headline及其子headline的统计信息
func (c *FileHeadlineCache) aggregateStats(stats *HeadlineStats) {
	for _, child := range stats.Children {
		c.aggregateStats(child)

		// 将子headline的统计信息累加到父headline
		stats.Info.TotalCards += child.Info.TotalCards
		stats.Info.TotalVirtCards += child.Info.TotalVirtCards
		stats.Info.ExpiredCards += child.Info.ExpiredCards
		stats.Info.WaitingCards += child.Info.WaitingCards
		stats.Info.ReviewingCards += child.Info.ReviewingCards
	}
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
		childrenIDs = append(childrenIDs, child.Info.ID)
		childrenIDs = append(childrenIDs, c.getChildrenIDs(child.Info.ID)...)
	}

	return childrenIDs
}

// getChildren 获取指定headline的子headlineWithFsrs
func (c *FileHeadlineCache) getChildren(headlineID string) []*HeadlineWithFsrs {
	var children []*HeadlineWithFsrs
	// 获取当前headline的统计信息
	stats := c.getHeadlineStats(headlineID)
	if stats == nil {
		return children
	}
	// 递归获取所有子headline的Fsrs
	for _, child := range stats.Children {
		children = append(children, child.Info)
	}
	return children
}

// GetHeadlineStats 获取指定headline的统计信息
func (c *FileHeadlineCache) getHeadlineStats(headlineID string) *HeadlineStats {
	return c.HeadMap[headlineID]
}

// GetFileStats 获取整个文件的统计信息
func (c *FileHeadlineCache) getFileStats() (totalCards, totalVirtCards, expiredCards, waitingCards, reviewingCards int) {
	for _, rootHead := range c.RootHeads {
		totalCards += rootHead.Info.TotalCards
		totalVirtCards += rootHead.Info.TotalVirtCards
		expiredCards += rootHead.Info.ExpiredCards
		waitingCards += rootHead.Info.WaitingCards
		reviewingCards += rootHead.Info.ReviewingCards
	}
	return
}

// FindNextNew performs a depth-first search to find the first HeadlineWithFsrs with State = 0.
// It returns the found HeadlineWithFsrs or nil if none is found.
func (c *FileHeadlineCache) FindNextNew() *HeadlineWithFsrs {
	var result *HeadlineWithFsrs
	for _, rootHead := range c.RootHeads {
		result = c.findNextNewDFS(rootHead)
		if result != nil {
			return result
		}
	}
	return nil
}

// findNextNewDFS is a helper function for depth-first search.
func (c *FileHeadlineCache) findNextNewDFS(stats *HeadlineStats) *HeadlineWithFsrs {
	// Check the current headline
	if stats.Info != nil && stats.Info.State == 0 {
		return stats.Info
	}

	// Recursively search children
	for _, child := range stats.Children {
		found := c.findNextNewDFS(child)
		if found != nil {
			return found
		}
	}

	// Not found in this subtree
	return nil
}

// GetFileFromCache 从缓存管理器获取指定文件的缓存，如果不存在或已过期则创建新缓存
func (m *FileHeadlineCacheManager) GetFileCacheFromCache(fileID string) (*FileHeadlineCache, error) {
	// 先尝试读取缓存
	m.mutex.RLock()
	cache, exists := m.caches[fileID]
	m.mutex.RUnlock()

	// 如果缓存存在且未过期，直接返回
	if exists && time.Since(cache.LastUpdated) < m.maxAge {
		return cache, nil
	}

	// 在获取写锁之前先创建新缓存，避免在持有锁的过程中进行耗时的数据库操作
	// 创建新缓存
	newCache, err := NewFileHeadlineCache(fileID)
	if err != nil {
		return nil, err
	}

	// 设置最后更新时间
	newCache.LastUpdated = time.Now()

	// 双重检查，避免在获取锁的过程中其他协程已经创建了缓存
	cache, exists = m.caches[fileID]
	if exists && time.Since(cache.LastUpdated) < m.maxAge {
		return cache, nil
	}

	// 缓存不存在或已过期，获取写锁更新缓存
	m.mutex.Lock()
	defer m.mutex.Unlock()

	// 更新缓存
	m.caches[fileID] = newCache
	return newCache, nil
}

func (m *FileHeadlineCacheManager) GetFileFromCache(fileID string) (*FileInfo, error) {
	cache, err := m.GetFileCacheFromCache(fileID)
	if cache != nil {
		return &cache.Info, err
	} else {
		return nil, err
	}
}

// GetFilesFromCache 从缓存管理器获取多个指定文件的缓存，如果不存在或已过期则创建新缓存
func (m *FileHeadlineCacheManager) GetFilesFromCache(fileIDs []string) ([]*FileInfo, error) {
	result := make([]*FileInfo, 0, len(fileIDs))
	var firstErr error

	// 遍历所有fileID，获取各自的缓存
	for _, fileID := range fileIDs {
		cache, err := m.GetFileCacheFromCache(fileID)
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

// InvalidateCache 刷新指定文件的缓存
func (m *FileHeadlineCacheManager) InvalidateCache(fileID string) {
	m.mutex.Lock()
	delete(m.caches, fileID)
	m.mutex.Unlock()
}

// InvalidateAllCaches 使所有缓存失效
func (m *FileHeadlineCacheManager) InvalidateAllCache() {
	m.mutex.Lock()
	m.caches = make(map[string]*FileHeadlineCache)
	m.mutex.Unlock()
}

// RefreshCacheByFileID 刷新指定文件的缓存
func (m *FileHeadlineCacheManager) RefreshCacheByFileID(fileID string) {
	m.mutex.Lock()
	delete(m.caches, fileID)
	m.mutex.Unlock()
	go func() {
		_, err := m.GetFileCacheFromCache(fileID)
		logger.Errorf("Refresh cache for file %s failed: %v", fileID, err)
	}()
}

// RefreshCacheByHeadlineID 根据HeadlineID 刷新相关缓存失效
func (m *FileHeadlineCacheManager) RefreshCacheByHeadlineID(headlineID string) error {
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

	go func() {
		_, err = m.GetFileCacheFromCache(*heads[0].FileID)
		logger.Errorf("Refresh cache for file %s failed: %v", *heads[0].FileID, err)
	}()

	return nil
}

// GetFileStatsByHeadlineID 获取指定headline及其所有子headline的统计信息
func GetFileStatsByHeadlineID(fileID string, headlineID string) (*HeadlineStats, error) {
	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileCacheFromCache(fileID)
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
	cache, err := GetCacheManager().GetFileCacheFromCache(fileID)
	if err != nil {
		return 0, 0, 0, 0, err
	}

	return cache.Info.TotalCards, cache.Info.ExpiredCards, cache.Info.WaitingCards, cache.Info.ReviewingCards, nil
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
	cache, err := GetCacheManager().GetFileCacheFromCache(*heads[0].FileID)
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

func GetChildrenByFileID(fileID string) ([]*HeadlineWithFsrs, error) {
	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileCacheFromCache(fileID)
	if err != nil {
		return nil, err
	}

	// 获取所有子headline的ID
	var children []*HeadlineWithFsrs
	for _, rootHead := range cache.RootHeads {
		children = append(children, rootHead.Info)
	}
	return children, nil
}

func GetChildrenByHeadlineID(headlineID, fileid string) ([]*HeadlineWithFsrs, error) {
	// 从缓存管理器获取缓存
	cache, err := GetCacheManager().GetFileCacheFromCache(fileid)
	if err != nil {
		return nil, err
	}
	// 获取所有子headline的ID
	children := cache.getChildren(headlineID)
	return children, nil
}
