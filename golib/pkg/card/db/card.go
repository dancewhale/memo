package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	"github.com/samber/lo"
	"gorm.io/gorm"
	"gorm.io/gorm/clause"
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

func (c *CardDB) JoinFsrs() *CardDB {
	fsrs := dal.FsrsInfo

	c.headDO = dal.Headline.WithContext(context.Background()).
		Join(dal.FsrsInfo, dal.Headline.ID.EqCol(dal.FsrsInfo.HeadlineID)).
		Where(fsrs.CreatedAt.IsNotNull())

	return c
}

func (c *CardDB) DueBeforeDayFilter(op string, n int64) *CardDB {
	fsrs := dal.FsrsInfo

	dayStart, _ := util.GetDayTime(n)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.Due.Gte(dayStart))
	} else {
		c.headDO = c.headDO.Where(fsrs.Due.Lte(dayStart))
	}
	return c
}

// the card is due at that day.
func (c *CardDB) DueAtDaysFilter(op string, n []int64) *CardDB {
	fsrs := dal.FsrsInfo

	var headIDs []string
	for _, n := range n {
		dayStart, dayEnd := util.GetDayTime(n)
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

func (c *CardDB) DueAfterDayFilter(op string, n int64) *CardDB {
	fsrs := dal.FsrsInfo

	_, dayEnd := util.GetDayTime(n)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.Due.Lte(dayEnd))
	} else {
		c.headDO = c.headDO.Where(fsrs.Due.Gte(dayEnd))
	}
	return c
}

func (c *CardDB) StateFilter(op string, states []string) *CardDB {
	fsrs := dal.FsrsInfo
	stateList := ParseStateList(states)
	if op == "-" {
		c.headDO = c.headDO.Where(fsrs.State.NotIn(stateList...))
	} else {
		c.headDO = c.headDO.Where(fsrs.State.In(stateList...))
	}
	return c
}

// get card which due time is today
// op: '+' means include types (In), '-' means exclude types (Not In)
func (c *CardDB) TypeFilter(op string, stype []string) *CardDB {
	h := dal.Headline
	if op == "-" {
		c.headDO = c.headDO.Not(h.ScheduledType.In(stype...))
	} else {
		c.headDO = c.headDO.Where(h.ScheduledType.In(stype...))
	}
	return c
}

func (c *CardDB) TagFilter(op string, tag []string) *CardDB {
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
func (c *CardDB) PropertyFilter(op, key, value string) *CardDB {
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

func (c *CardDB) FileFilter(op string, fileID []string) *CardDB {
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.FileID.NotIn(fileID...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.FileID.In(fileID...))
	}

	return c
}

func (c *CardDB) ParentFilter(op string, parentID []string) *CardDB {
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.ParentID.NotIn(parentID...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.ParentID.In(parentID...))
	}
	return c
}

func (c *CardDB) AncestorFilter(op string, ancestorID []string) *CardDB {
	idList := getHeadIDByAncestorIDs(ancestorID)
	if op == "-" {
		c.headDO = c.headDO.Where(dal.Headline.ID.NotIn(idList...))
	} else {
		c.headDO = c.headDO.Where(dal.Headline.ID.In(idList...))
	}
	return c
}

func (c *CardDB) OrderByWeight(order string) *CardDB {
	if order == AscOrder {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Asc())
	} else if order == DescOrder {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Desc())
	} else {
		c.headDO = c.headDO.Order(dal.Headline.Weight.Desc())
	}
	return c
}

func (c *CardDB) OrderByDue(order string) *CardDB {
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

func (c *CardDB) OrderByLevel(order string) *CardDB {
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

func (c *CardDB) OrderBySeq(order string) *CardDB {
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

func (c *CardDB) OrderByRandom() *CardDB {
	DB := c.headDO.UnderlyingDB().
		Clauses(clause.OrderBy{Expression: clause.Expr{SQL: "RANDOM()"}})
	c.headDO.ReplaceDB(DB)
	return c
}

func (c *CardDB) Find() ([]*storage.Headline, error) {
	cards, err := c.headDO.Find()
	if err != nil {
		return nil, logger.Errorf("Get review card failed: %v", err)
	}
	return cards, nil
}

func (c *CardDB) Count() (int64, error) {
	count, err := c.headDO.Count()
	if err != nil {
		return 0, logger.Errorf("Count card failed: %v", err)
	}
	return count, nil
}
