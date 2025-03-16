package db

import (
	"fmt"
	"memo/pkg/logger"
	"strconv"
	"strings"
)

// 定义查询语法错误
type QuerySyntaxError struct {
	Message string
}

func (e *QuerySyntaxError) Error() string {
	return fmt.Sprintf("查询语法错误: %s", e.Message)
}

// 定义查询冲突错误
type QueryConflictError struct {
	Message string
}

func (e *QueryConflictError) Error() string {
	return fmt.Sprintf("查询冲突错误: %s", e.Message)
}

// 定义查询语法类型
const (
	OrderType  = "order"
	FilterType = "filter"
)

// 定义排序类型
const (
	WeightOrder = "weight"
	DueOrder    = "due"
	RandomOrder = "random"
)

// 定义过滤器类型
const (
	OrgIDFilter     = "orgid"
	FileIDFilter    = "fileid"
	DueAtFilter     = "dueAt"
	DueBeforeFilter = "dueBefore"
	DueAfterFilter  = "dueAfter"
	ParentIDFilter  = "parentid"
	TypeFilter      = "type"
	TagFilter       = "tag"
	PropertyFilter  = "property"
)

// 查询语法单元
type QueryUnit struct {
	Type      string // order 或 filter
	Field     string // 字段名
	SubField  string // 子字段名（可选）
	Value     string // 值
	RawSyntax string // 原始语法字符串
}

// 查询解析器
type QueryParser struct {
	queryUnits     []QueryUnit
	orderFields    map[string]bool // 记录已经使用的排序字段
	hasRandomOrder bool            // 是否有随机排序
}

// 创建新的查询解析器
func NewQueryParser() *QueryParser {
	return &QueryParser{
		queryUnits:     make([]QueryUnit, 0),
		orderFields:    make(map[string]bool),
		hasRandomOrder: false,
	}
}

// 解析查询语句列表
func (p *QueryParser) Parse(queryList []string) error {
	for _, query := range queryList {
		unit, err := p.parseQueryUnit(query)
		if err != nil {
			return err
		}

		// 检查冲突
		if err := p.checkConflict(unit); err != nil {
			return err
		}

		// 添加到查询单元列表
		p.queryUnits = append(p.queryUnits, unit)
	}

	return nil
}

// 解析单个查询语句
func (p *QueryParser) parseQueryUnit(query string) (QueryUnit, error) {
	parts := strings.Split(query, ":")
	if len(parts) < 2 {
		return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("无效的查询语法: %s", query)}
	}

	unit := QueryUnit{RawSyntax: query}

	// 解析查询类型
	switch parts[0] {
	case OrderType:
		unit.Type = OrderType
		if len(parts) < 2 {
			return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("排序语法缺少字段: %s", query)}
		}

		// 处理排序字段
		if parts[1] == RandomOrder {
			// 随机排序特殊处理
			unit.Field = RandomOrder
		} else if len(parts) >= 3 {
			// 常规排序: order:field:direction
			unit.Field = parts[1]
			unit.SubField = parts[2]

			// 验证排序字段
			if !isValidOrderField(unit.Field) {
				return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("无效的排序字段: %s", unit.Field)}
			}

			// 验证排序方向
			if !isValidOrderDirection(unit.SubField) {
				return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("无效的排序方向: %s", unit.SubField)}
			}
		} else {
			return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("排序语法不完整: %s", query)}
		}

	case FilterType:
		unit.Type = FilterType
		if len(parts) < 3 {
			return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("过滤语法不完整: %s", query)}
		}

		unit.Field = parts[1]
		if len(parts) == 3 {
			unit.Value = parts[2]
		} else if len(parts) == 4 {
			unit.SubField = parts[2]
			unit.Value = parts[3]
		}

		// 验证过滤字段
		if !isValidFilterField(unit.Field) {
			return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("无效的过滤字段: %s", unit.Field)}
		}

		// 验证过滤值
		if err := validateFilterValue(unit.Field, unit.Value); err != nil {
			return QueryUnit{}, err
		}

	default:
		return QueryUnit{}, &QuerySyntaxError{Message: fmt.Sprintf("未知的查询类型: %s", parts[0])}
	}

	return unit, nil
}

// 检查查询冲突
func (p *QueryParser) checkConflict(unit QueryUnit) error {
	if unit.Type == OrderType {
		// 检查随机排序冲突
		if unit.Field == RandomOrder {
			if len(p.orderFields) > 0 {
				return &QueryConflictError{Message: "随机排序不能与其他排序方式同时使用"}
			}
			p.hasRandomOrder = true
		} else {
			// 检查是否已经有随机排序
			if p.hasRandomOrder {
				return &QueryConflictError{Message: "其他排序方式不能与随机排序同时使用"}
			}

			// 检查字段是否已经被使用
			if _, exists := p.orderFields[unit.Field]; exists {
				return &QueryConflictError{Message: fmt.Sprintf("排序字段 '%s' 已经被使用", unit.Field)}
			}

			p.orderFields[unit.Field] = true
		}
	}

	return nil
}

// 构建查询
func (p *QueryParser) BuildQuery() (*QueryBuilder, error) {
	queryBuilder, err := NewQueryBuilder()
	if err != nil {
		return nil, err
	}

	// 添加FSRS关联
	queryBuilder = queryBuilder.WithJoinFsrs()

	// 按照查询单元顺序应用策略
	for _, unit := range p.queryUnits {
		switch unit.Type {
		case OrderType:
			if unit.Field == RandomOrder {
				// 随机排序特殊处理
				queryBuilder.cardDB = queryBuilder.cardDB.OrderByRandom()
			} else {
				// 添加排序策略
				queryBuilder = queryBuilder.WithOrder(unit.Field, unit.SubField)
			}

		case FilterType:
			// 应用过滤策略
			switch unit.Field {
			case FileIDFilter:
				queryBuilder = queryBuilder.WithFileFilter(unit.Value)

			case TypeFilter:
				queryBuilder = queryBuilder.WithTypeFilter(unit.Value)

			case DueAtFilter:
				dayOffset, _ := strconv.ParseInt(unit.Value, 10, 64)
				queryBuilder = queryBuilder.WithDueTime(dayOffset, "at")

			case DueBeforeFilter:
				dayOffset, _ := strconv.ParseInt(unit.Value, 10, 64)
				queryBuilder = queryBuilder.WithDueTime(dayOffset, "before")

			case DueAfterFilter:
				dayOffset, _ := strconv.ParseInt(unit.Value, 10, 64)
				queryBuilder = queryBuilder.WithDueTime(dayOffset, "after")

			case TagFilter:
				queryBuilder = queryBuilder.WithTagFilter(unit.Value)

			case PropertyFilter:
				queryBuilder = queryBuilder.WithPropertyFilter(unit.SubField, unit.Value)

			case ParentIDFilter:
				// 这里需要实现ParentID过滤策略
				// TODO: 实现ParentID过滤
				logger.Warnf("ParentID过滤尚未实现: %s", unit.Value)

			case OrgIDFilter:
				// 这里需要实现OrgID过滤策略
				// TODO: 实现OrgID过滤
				logger.Warnf("OrgID过滤尚未实现: %s", unit.Value)
			}
		}
	}

	return queryBuilder, nil
}

// 验证排序字段是否有效
func isValidOrderField(field string) bool {
	validFields := map[string]bool{
		WeightOrder: true,
		DueOrder:    true,
		RandomOrder: true,
	}

	_, valid := validFields[field]
	return valid
}

// 验证排序方向是否有效
func isValidOrderDirection(direction string) bool {
	return direction == AscOrder || direction == DescOrder
}

// 验证过滤字段是否有效
func isValidFilterField(field string) bool {
	validFields := map[string]bool{
		OrgIDFilter:     true,
		FileIDFilter:    true,
		DueAtFilter:     true,
		DueBeforeFilter: true,
		DueAfterFilter:  true,
		ParentIDFilter:  true,
		TypeFilter:      true,
		TagFilter:       true,
		PropertyFilter:  true,
	}

	_, valid := validFields[field]
	return valid
}

// 验证过滤值
func validateFilterValue(field, value string) error {
	switch field {
	case DueAtFilter, DueBeforeFilter, DueAfterFilter:
		// 验证日期偏移值是否为整数
		_, err := strconv.ParseInt(value, 10, 64)
		if err != nil {
			return &QuerySyntaxError{Message: fmt.Sprintf("日期偏移值必须是整数: %s", value)}
		}
	}

	return nil
}

// 从查询语句列表构建查询
func BuildQueryFromSyntax(queryList []string) (*QueryBuilder, error) {
	parser := NewQueryParser()

	// 解析查询语句
	err := parser.Parse(queryList)
	if err != nil {
		return nil, err
	}

	// 构建查询
	return parser.BuildQuery()
}
