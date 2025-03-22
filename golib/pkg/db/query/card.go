package query

import (
	"context"
	"github.com/samber/lo"
	"gorm.io/gorm"
	"gorm.io/gorm/clause"
	"memo/pkg/db"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

func NewCardDB() (*CardDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	if dal.Headline == nil {
		dal.SetDefault(db)
	}

	do := dal.Headline.WithContext(context.Background())

	return &CardDB{
		db:     db,
		headDO: do,
	}, nil
}

type CardDB struct {
	db     *gorm.DB
	headDO dal.IHeadlineDo
	fsrsDO dal.IFsrsInfoDo
}

func (c *CardDB) joinFsrs() *CardDB {
	fsrs := dal.FsrsInfo

	c.headDO = dal.Headline.WithContext(context.Background()).
		Join(dal.FsrsInfo, dal.Headline.ID.EqCol(dal.FsrsInfo.HeadlineID)).
		Where(fsrs.CreatedAt.IsNotNull())

	return c
}

func (c *CardDB) dueBeforeDayFilter(op string, n int64) *CardDB {
	fsrs := dal.FsrsInfo

	dayStart, _ := db.GetDayTime(n)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.Due.Gte(dayStart))
	} else {
		c.headDO = c.headDO.Where(fsrs.Due.Lte(dayStart))
	}
	return c
}

// the card is due at that day.
func (c *CardDB) dueAtDaysFilter(op string, n []int64) *CardDB {
	fsrs := dal.FsrsInfo

	var headIDs []string
	for _, n := range n {
		dayStart, dayEnd := db.GetDayTime(n)
		f, err := fsrs.WithContext(context.Background()).Where(fsrs.Due.Gte(dayStart)).
			Where(fsrs.Due.Lte(dayEnd)).Find()

		if err != nil {
			logger.Errorf("Get card DueAtDay %d error: %v", n, err)
		}
		headIDs = append(headIDs, lo.Map(f, func(f *storage.FsrsInfo, index int) string {
			return f.HeadlineID
		})...)
	}
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.HeadlineID.NotIn(headIDs...))
	} else {
		c.headDO = c.headDO.Where(fsrs.HeadlineID.In(headIDs...))
	}

	return c
}

func (c *CardDB) dueAfterDayFilter(op string, n int64) *CardDB {
	fsrs := dal.FsrsInfo

	_, dayEnd := db.GetDayTime(n)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.Due.Lte(dayEnd))
	} else {
		c.headDO = c.headDO.Where(fsrs.Due.Gte(dayEnd))
	}
	return c
}

func (c *CardDB) limitFilter(limit int) *CardDB {
	c.headDO = c.headDO.Limit(limit)
	return c
}

func (c *CardDB) stateFilter(op string, states []string) *CardDB {
	fsrs := dal.FsrsInfo
	stateList := db.ParseStateList(states)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.State.NotIn(stateList...))
	} else {
		c.headDO = c.headDO.Where(fsrs.State.In(stateList...))
	}
	return c
}

// get card which due time is today
// op: '+' means include types (In), '-' means exclude types (Not In)
func (c *CardDB) typeFilter(op string, stype []string) *CardDB {
	h := dal.Headline
	if op == "-" {
		c.headDO = c.headDO.Not(h.ScheduledType.In(stype...))
	} else {
		c.headDO = c.headDO.Where(h.ScheduledType.In(stype...))
	}
	return c
}

func (c *CardDB) tagFilter(op string, tag []string) *CardDB {
	h := dal.Headline
	t := dal.Tag

	// 使用子查询获取包含指定标签的 headline ID
	tagQuery := t.WithContext(context.Background()).Select(t.HeadlineID).Where(t.Name.In(tag...))

	if op == "-" {
		c.headDO = c.headDO.Not(h.Columns(h.ID).In(tagQuery))
	} else {
		c.headDO = c.headDO.Where(h.Columns(h.ID).In(tagQuery))
	}
	return c
}

// get card which has specific properties
// op: '+' means include properties (In), '-' means exclude properties (Not In)
// properties: map of key-value pairs to filter by
func (c *CardDB) propertyFilter(op, key, value string) *CardDB {
	h := dal.Headline
	p := dal.Property

	// 使用子查询获取包含指定属性的 headline ID
	propertyQuery := p.WithContext(context.Background()).Select(p.HeadlineID).
		Where(p.Key.Eq(key), p.Value.Eq(value))

	// 根据操作符决定使用 In 还是 Not In
	if op == "-" {
		c.headDO = c.headDO.Not(h.Columns(h.ID).In(propertyQuery))
	} else {
		c.headDO = c.headDO.Where(h.Columns(h.ID).In(propertyQuery))
	}
	return c
}

func (c *CardDB) fileFilter(op string, fileID []string) *CardDB {
	if len(fileID) == 1 && fileID[0] == "NULL" {
		if op == "-" {
			c.headDO = c.headDO.Where(dal.Headline.FileID.IsNotNull())
		} else {
			c.headDO = c.headDO.Where(dal.Headline.FileID.IsNull())
		}
		return c
	}
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.FileID.NotIn(fileID...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.FileID.In(fileID...))
	}

	return c
}

func (c *CardDB) parentFilter(op string, parentID []string) *CardDB {
	if len(parentID) == 1 && parentID[0] == "NULL" {
		if op == "-" {
			c.headDO = c.headDO.Where(dal.Headline.ParentID.IsNotNull())
		} else {
			c.headDO = c.headDO.Where(dal.Headline.ParentID.IsNull())
		}
		return c
	}
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.ParentID.NotIn(parentID...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.ParentID.In(parentID...))
	}
	return c
}

func (c *CardDB) ancestorFilter(op string, ancestorID []string) *CardDB {
	idList := db.GetHeadIDByAncestorIDs(ancestorID)
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.ID.NotIn(idList...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.ID.In(idList...))
	}
	return c
}

func (c *CardDB) orderByWeight(order string) *CardDB {
	if order == AscOrder {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Asc())
	} else if order == DescOrder {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Desc())
	} else {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Desc())
	}
	return c
}

func (c *CardDB) orderByDue(order string) *CardDB {
	fsrs := dal.FsrsInfo
	if order == AscOrder {
		c.headDO = c.headDO.Order(fsrs.Due.Asc())
	} else if order == DescOrder {
		c.headDO = c.headDO.Order(fsrs.Due.Desc())
	} else {
		c.headDO = c.headDO.Order(fsrs.Due.Desc())
	}
	return c
}

func (c *CardDB) orderByLevel(order string) *CardDB {
	h := dal.Headline
	if order == AscOrder {
		c.headDO = c.headDO.Order(h.Level.Asc())
	} else if order == DescOrder {
		c.headDO = c.headDO.Order(h.Level.Desc())
	} else {
		c.headDO = c.headDO.Order(h.Level.Desc())
	}
	return c
}

func (c *CardDB) orderBySeq(order string) *CardDB {
	h := dal.Headline
	if order == AscOrder {
		c.headDO = c.headDO.Order(h.Order_.Asc())
	} else if order == DescOrder {
		c.headDO = c.headDO.Order(h.Order_.Desc())
	} else {
		c.headDO = c.headDO.Order(h.Order_.Desc())
	}
	return c
}

func (c *CardDB) orderByRandom() *CardDB {
	DB := c.headDO.UnderlyingDB().
		Clauses(clause.OrderBy{Expression: clause.Expr{SQL: "RANDOM()"}})
	c.headDO.ReplaceDB(DB)
	return c
}

func (c *CardDB) find() ([]*storage.Headline, error) {
	cards, err := c.headDO.Find()
	if err != nil {
		return nil, logger.Errorf("Get review card failed: %v", err)
	}
	return cards, nil
}

func (c *CardDB) Scan(result interface{}) error {
	return c.headDO.Scan(result)
}

func (c *CardDB) count() (int64, error) {
	count, err := c.headDO.Count()
	if err != nil {
		return 0, logger.Errorf("Count card failed: %v", err)
	}
	return count, nil
}

// Use in other module to get card list
func (c *CardDB) GetFileHasNewCard() ([]*storage.File, error) {
	head := dal.Headline
	fsrs := dal.FsrsInfo
	file := dal.File

	heads, err := head.WithContext(context.Background()).
		Join(fsrs, head.ID.EqCol(fsrs.HeadlineID)).
		Select(head.FileID).Distinct().
		Where(fsrs.State.Eq(0)).Where(head.FileID.IsNotNull()).Find()
	if err != nil {
		return nil, logger.Errorf("Get file has new card error: %v", err)
	}
	fileIDList := lo.Map(heads, func(item *storage.Headline, index int) string {
		return *item.FileID
	})

	files, err := file.WithContext(context.Background()).
		Where(file.ID.In(fileIDList...)).Find()
	if err != nil {
		return nil, logger.Errorf("Get file has new card error: %v", err)
	}
	return files, nil
}
