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
	DayOffset int64  // 相对于今天的天数偏移，0表示今天
	TimeRange string // "at", "before", "after"
}

// Apply 实现QueryStrategy接口
func (s *DueTimeStrategy) Apply(db *CardDB) *CardDB {
	switch s.TimeRange {
	case "at":
		return db.DueAtDay(s.DayOffset)
	case "before":
		return db.DueBeforeDay(s.DayOffset)
	case "after":
		return db.DueAfterDay(s.DayOffset)
	default:
		return db.DueAtDay(s.DayOffset)
	}
}

// TypeFilterStrategy 类型过滤策略
type TypeFilterStrategy struct {
	Type string
}

// Apply 实现QueryStrategy接口
func (s *TypeFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.TypeFilter(s.Type)
}

// FileFilterStrategy 文件过滤策略
type FileFilterStrategy struct {
	FileID string
}

// Apply 实现QueryStrategy接口
func (s *FileFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.FileFilter(s.FileID)
}

// ParentFilterStrategy 父卡片过滤策略
type ParentFilterStrategy struct {
	ParentID string
}

// Apply 实现QueryStrategy接口
func (s *ParentFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.ParentFilter(s.ParentID)
}

// AncestorFilterStrategy 祖先卡片过滤策略
type AncestorFilterStrategy struct {
	AncestorID string
}

// Apply 实现QueryStrategy接口
func (s *AncestorFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.AncestorFilter(s.AncestorID)
}

// TagFilterStrategy 标签过滤策略
type TagFilterStrategy struct {
	Tag string
}

// Apply 实现QueryStrategy接口
func (s *TagFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.TagFilter(s.Tag)
}

// PropertyFilterStrategy 属性过滤策略
type PropertyFilterStrategy struct {
	Key   string
	Value string
}

// PropertyFilterStrategy 实现QueryStrategy接口
func (s *PropertyFilterStrategy) Apply(db *CardDB) *CardDB {
	return db.PropertyFilter(s.Key, s.Value)
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
func (b *QueryBuilder) WithDueTime(dayOffset int64, timeRange string) *QueryBuilder {
	b.strategies = append(b.strategies, &DueTimeStrategy{
		DayOffset: dayOffset,
		TimeRange: timeRange,
	})
	return b
}

// WithTypeFilter 添加类型过滤策略
func (b *QueryBuilder) WithTypeFilter(cardType string) *QueryBuilder {
	b.strategies = append(b.strategies, &TypeFilterStrategy{
		Type: cardType,
	})
	return b
}

// WithParentFilter 添加父卡片过滤策略
func (b *QueryBuilder) WithParentFilter(parentID string) *QueryBuilder {
	b.strategies = append(b.strategies, &ParentFilterStrategy{
		ParentID: parentID,
	})
	return b
}

// WithAncestorFilter 添加祖先卡片过滤策略
func (b *QueryBuilder) WithAncestorFilter(ancestorID string) *QueryBuilder {
	b.strategies = append(b.strategies, &AncestorFilterStrategy{
		AncestorID: ancestorID,
	})
	return b
}

// WithTagFilter 添加标签过滤策略
func (b *QueryBuilder) WithTagFilter(tag string) *QueryBuilder {
	b.strategies = append(b.strategies, &TagFilterStrategy{
		Tag: tag,
	})
	return b
}

// WithPropertyFilter 添加属性过滤策略
func (b *QueryBuilder) WithPropertyFilter(key, value string) *QueryBuilder {
	b.strategies = append(b.strategies, &PropertyFilterStrategy{
		Key:   key,
		Value: value,
	})
	return b
}

// WithFileFilter 添加文件过滤策略
func (b *QueryBuilder) WithFileFilter(fileID string) *QueryBuilder {
	b.strategies = append(b.strategies, &FileFilterStrategy{
		FileID: fileID,
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

// Execute 执行查询
func (b *QueryBuilder) Execute() ([]*storage.Headline, error) {
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
	cards, err := b.Execute()
	if err != nil {
		return nil, err
	}

	if len(cards) == 0 {
		return nil, nil
	}

	return cards[0], nil
}
