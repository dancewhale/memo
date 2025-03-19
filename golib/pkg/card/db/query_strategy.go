package db

import (
	"memo/pkg/storage"
)

// QueryStrategy 定义查询策略接口
type QueryStrategy interface {
	// Apply 应用查询策略到CardDB
	Apply(db *CardDB) *CardDB
}

// DueTimeStrategy 到期时间查询策略
type DueTimeStrategy struct {
	DayOffset []int64 // 相对于今天的天数偏移，0表示今天
	TimeRange string  // "at", "before", "after"
	Operater  string  // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *DueTimeStrategy) Apply(db *CardDB) *CardDB {
	switch s.TimeRange {
	case "at":
		return db.DueAtDaysFilter(s.Operater, s.DayOffset)
	case "before":
		return db.DueBeforeDayFilter(s.Operater, s.DayOffset[0])
	case "after":
		return db.DueAfterDayFilter(s.Operater, s.DayOffset[0])
	default:
		return db.DueAtDaysFilter(s.Operater, s.DayOffset)
	}
}

// LimitFilterStrategy 限制数量查询策略
type LimitFilterStrategy struct {
	Limit int
}

// Apply 实现QueryStrategy接口
func (s *LimitFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.LimitFilter(s.Limit)
}

// TypeFilterStrategy 类型过滤策略
type TypeFilterStrategy struct {
	Type     []string
	Operater string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *TypeFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.TypeFilter(s.Operater, s.Type)
}

// StateFilterStrategy 状态过滤策略
type StateFilterStrategy struct {
	State    []string
	Operater string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *StateFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.StateFilter(s.Operater, s.State)
}

// FileFilterStrategy 文件过滤策略
type FileFilterStrategy struct {
	FileID   []string
	Operater string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *FileFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.FileFilter(OrderType, s.FileID)
}

// ParentFilterStrategy 父卡片过滤策略
type ParentFilterStrategy struct {
	ParentID []string
	Operater string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *ParentFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.ParentFilter(s.Operater, s.ParentID)
}

// AncestorFilterStrategy 祖先卡片过滤策略
type AncestorFilterStrategy struct {
	AncestorID []string
	Operater   string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *AncestorFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.AncestorFilter(s.Operater, s.AncestorID)
}

// TagFilterStrategy 标签过滤策略
type TagFilterStrategy struct {
	Tag      []string
	Operater string // "+", "-"
}

// Apply 实现QueryStrategy接口
func (s *TagFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.TagFilter(s.Operater, s.Tag)
}

// PropertyFilterStrategy 属性过滤策略
type PropertyFilterStrategy struct {
	Operater   string // "+", "-"
	Key, Value string
}

// PropertyFilterStrategy 实现QueryStrategy接口
func (s *PropertyFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.PropertyFilter(s.Operater, s.Key, s.Value)
}

// OrderStrategy 排序策略
type OrderStrategy struct {
	OrderBy string // "weight", "due" "level", "seq" "random"
	Order   string // AscOrder, DescOrder
}

// Apply 实现QueryStrategy接口
func (s *OrderStrategy) Apply(db *CardDB) *CardDB {
	switch s.OrderBy {
	case "weight":
		return db.OrderByWeight(s.Order)
	case "due":
		return db.OrderByDue(s.Order)
	case "level":
		return db.OrderByLevel(s.Order)
	case "seq":
		return db.OrderBySeq(s.Order)
	default:
		return db.OrderByWeight(s.Order)
	}
}

// QueryBuilder 查询构建器
type QueryBuilder struct {
	strategies []QueryStrategy
	cardDB     *CardDB
}

// NewQueryBuilder 创建新的查询构建器
func NewQueryBuilder() (*QueryBuilder, error) {
	cardDB, err := NewCardDB()
	if err != nil {
		return nil, err
	}

	return &QueryBuilder{
		strategies: make([]QueryStrategy, 0),
		cardDB:     cardDB,
	}, nil
}

// WithJoinFsrs 添加FSRS关联
func (b *QueryBuilder) WithJoinFsrs() *QueryBuilder {
	b.cardDB = b.cardDB.JoinFsrs()
	return b
}

// WithDueTime 添加到期时间策略
func (b *QueryBuilder) WithDueTime(dayOffset []int64, timeRange string) *QueryBuilder {
	b.strategies = append(b.strategies, &DueTimeStrategy{
		DayOffset: dayOffset,
		TimeRange: timeRange,
	})
	return b
}

// WithLimitFilter 添加限制数量策略
func (b *QueryBuilder) WithLimitFilter(limit int) *QueryBuilder {
	b.strategies = append(b.strategies, &LimitFilterStrategy{
		Limit: limit,
	})
	return b
}

// WithTypeFilter 添加类型过滤策略
func (b *QueryBuilder) WithTypeFilter(operater string, cardType []string) *QueryBuilder {
	b.strategies = append(b.strategies, &TypeFilterStrategy{
		Type:     cardType,
		Operater: operater,
	})
	return b
}

// WithStateFilter 添加状态过滤策略
func (b *QueryBuilder) WithStateFilter(operater string, states []string) *QueryBuilder {
	b.strategies = append(b.strategies, &StateFilterStrategy{
		State:    states,
		Operater: operater,
	})
	return b
}

// WithParentFilter 添加父卡片过滤策略
func (b *QueryBuilder) WithParentFilter(operater string, parentID []string) *QueryBuilder {
	b.strategies = append(b.strategies, &ParentFilterStrategy{
		ParentID: parentID,
		Operater: operater,
	})
	return b
}

// WithAncestorFilter 添加祖先卡片过滤策略
func (b *QueryBuilder) WithAncestorFilter(operater string, ancestorID []string) *QueryBuilder {
	b.strategies = append(b.strategies, &AncestorFilterStrategy{
		AncestorID: ancestorID,
		Operater:   operater,
	})
	return b
}

// WithTagFilter 添加标签过滤策略
func (b *QueryBuilder) WithTagFilter(operater string, tag []string) *QueryBuilder {
	b.strategies = append(b.strategies, &TagFilterStrategy{
		Tag:      tag,
		Operater: operater,
	})
	return b
}

// WithPropertyFilter 添加属性过滤策略
func (b *QueryBuilder) WithPropertyFilter(operater, key, value string) *QueryBuilder {
	b.strategies = append(b.strategies, &PropertyFilterStrategy{
		Key:      key,
		Value:    value,
		Operater: operater,
	})
	return b
}

// WithFileFilter 添加文件过滤策略
func (b *QueryBuilder) WithFileFilter(operator string, fileID []string) *QueryBuilder {
	b.strategies = append(b.strategies, &FileFilterStrategy{
		FileID:   fileID,
		Operater: operator,
	})
	return b
}

// WithOrder 添加排序策略
func (b *QueryBuilder) WithOrder(orderBy string, order string) *QueryBuilder {
	b.strategies = append(b.strategies, &OrderStrategy{
		OrderBy: orderBy,
		Order:   order,
	})
	return b
}

// ExecuteList 执行查询
func (b *QueryBuilder) ExecuteList() ([]*storage.Headline, error) {
	// 应用所有策略
	db := b.cardDB
	for _, strategy := range b.strategies {
		db = strategy.Apply(db)
	}

	// 执行查询
	return db.Find()
}

// ExecuteFirst 执行查询并返回第一个结果
func (b *QueryBuilder) ExecuteFirst() (*storage.Headline, error) {
	cards, err := b.ExecuteList()
	if err != nil {
		return nil, err
	}

	if len(cards) == 0 {
		return nil, nil
	}

	return cards[0], nil
}

// ExecuteCount 执行查询并返回结果数量
func (b *QueryBuilder) ExecuteCount() (int64, error) {
	// 应用所有策略
	db := b.cardDB
	for _, strategy := range b.strategies {
		db = strategy.Apply(db)
	}

	// 执行查询
	return db.Count()
}
