package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage/dal"
	"memo/pkg/util"
	"sync"
	"time"
)

// HeadlineStats 存储headline及其子headline的统计信息
type HeadlineStats struct {
	HeadlineID     string           // headline的ID
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

// HeadlineCache 用于缓存文件中所有headline的树形结构和统计信息
type HeadlineCache struct {
	FileID      string                    // 文件ID
	RootHeads   []*HeadlineStats          // 根headline的统计信息
	HeadMap     map[string]*HeadlineStats // 所有headline的映射，用于快速查找
	Hash        string
	LastUpdated time.Time // 最后更新时间
}

// HeadlineCacheManager 全局缓存管理器
type HeadlineCacheManager struct {
	caches map[string]*HeadlineCache // 文件ID到缓存的映射
	mutex  sync.RWMutex              // 读写锁，保证并发安全
	maxAge time.Duration             // 缓存最大有效期
}

// 全局缓存管理器实例
var cacheManager *HeadlineCacheManager
var cacheManagerOnce sync.Once

// GetCacheManager 获取全局缓存管理器实例
func GetCacheManager() *HeadlineCacheManager {
	cacheManagerOnce.Do(func() {
		cacheManager = &HeadlineCacheManager{
			caches: make(map[string]*HeadlineCache),
			maxAge: 5 * time.Minute, // 默认缓存有效期为5分钟
		}
	})
	return cacheManager
}

// NewHeadlineCache 创建一个新的headline缓存
func NewHeadlineCache(fileID string) (*HeadlineCache, error) {

	cache := &HeadlineCache{
		FileID:    fileID,
		RootHeads: make([]*HeadlineStats, 0),
		HeadMap:   make(map[string]*HeadlineStats),
	}

	// 构建缓存树
	err := cache.buildCache()
	if err != nil {
		return nil, err
	}

	return cache, nil
}

// buildCache 构建headline缓存树并计算统计信息
func (c *HeadlineCache) buildCache() error {
	// 获取当前日期的结束时间，用于判断卡片是否过期
	_, dayEnd := util.GetDayTime(0)

	// 一次性查询所有headline和fsrsInfo信息
	fsrs := dal.FsrsInfo
	headline := dal.Headline

	// 创建一个临时结构体切片用于存储查询结果
	var results []HeadlineWithFsrs

	// 执行联合查询，获取所有headline和对应的fsrsInfo
	err := headline.WithContext(context.Background()).
		Select(headline.ALL, fsrs.ALL).
		LeftJoin(fsrs, headline.ID.EqCol(fsrs.HeadlineID)).
		Where(headline.FileID.Eq(c.FileID)).
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

	return nil
}

// aggregateStats 递归聚合headline及其子headline的统计信息
func (c *HeadlineCache) aggregateStats(stats *HeadlineStats) {
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
func (c *HeadlineCache) getHeadlineStats(headlineID string) *HeadlineStats {
	return c.HeadMap[headlineID]
}

// GetFileStats 获取整个文件的统计信息
func (c *HeadlineCache) getFileStats() (totalCards, expiredCards, waitingCards, reviewingCards int) {
	for _, rootHead := range c.RootHeads {
		totalCards += rootHead.TotalCards
		expiredCards += rootHead.ExpiredCards
		waitingCards += rootHead.WaitingCards
		reviewingCards += rootHead.ReviewingCards
	}
	return
}

// GetCache 从缓存管理器获取指定文件的缓存，如果不存在或已过期则创建新缓存
func (m *HeadlineCacheManager) GetCache(fileID string) (*HeadlineCache, error) {
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
	newCache, err := NewHeadlineCache(fileID)
	if err != nil {
		return nil, err
	}

	// 设置最后更新时间
	newCache.LastUpdated = time.Now()

	// 更新缓存
	m.caches[fileID] = newCache
	return newCache, nil
}

// InvalidateCache 使指定文件的缓存失效
func (m *HeadlineCacheManager) InvalidateCache(fileID string) {
	m.mutex.Lock()
	delete(m.caches, fileID)
	m.mutex.Unlock()
}

// InvalidateCacheByHeadlineID 根据HeadlineID使相关缓存失效
func (m *HeadlineCacheManager) InvalidateCacheByHeadlineID(headlineID string) error {
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
	cache, err := GetCacheManager().GetCache(fileID)
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
	cache, err := GetCacheManager().GetCache(fileID)
	if err != nil {
		return 0, 0, 0, 0, err
	}

	totalCards, expiredCards, waitingCards, reviewingCards = cache.getFileStats()
	return
}
