package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

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

func (c *CardDB) DueBeforeDay(n int64) *CardDB {
	fsrs := dal.FsrsInfo

	dayStart, _ := util.GetDayTime(n)
	c.headDO = c.headDO.Where(fsrs.Due.Lte(dayStart))
	return c
}

// the card is due at that day.
func (c *CardDB) DueAtDay(n int64) *CardDB {
	fsrs := dal.FsrsInfo
	dayStart, dayEnd := util.GetDayTime(n)
	c.headDO = c.headDO.Where(fsrs.Due.Gte(dayStart)).Where(fsrs.Due.Lte(dayEnd))
	return c
}

func (c *CardDB) DueAfterDay(n int64) *CardDB {
	fsrs := dal.FsrsInfo

	_, dayEnd := util.GetDayTime(n)
	c.headDO = c.headDO.Where(fsrs.Due.Gte(dayEnd))
	return c
}

// get card which due time is today
func (c *CardDB) TypeFilter(stype string) *CardDB {
	h := dal.Headline
	switch stype {
	case storage.NORMAL:
		c.headDO = c.headDO.Where(h.ScheduledType.Eq(storage.NORMAL))
	case storage.POSTPONE:
		c.headDO = c.headDO.Where(h.ScheduledType.Eq(storage.POSTPONE))
	case storage.DELTED:
		c.headDO = c.headDO.Where(h.ScheduledType.Eq(storage.DELTED))
	default:
		c.headDO = c.headDO.Where(h.ScheduledType.Eq(storage.NORMAL))
	}
	return c
}

func (c *CardDB) TagFilter(tag string) *CardDB {
	h := dal.Headline
	t := dal.Tag

	// 使用子查询获取包含指定标签的 headline ID
	tagQuery := t.WithContext(context.Background()).Select(t.HeadlineID).Where(t.Name.Eq(tag))

	// 使用 In 操作符筛选这些 ID
	c.headDO.Where(h.Columns(h.ID).In(tagQuery))
	return c
}

func (c *CardDB) PropertyFilter(key, value string) *CardDB {
	h := dal.Headline
	p := dal.Property

	// 使用子查询获取包含指定标签的 headline ID
	propertyQuery := p.WithContext(context.Background()).Select(p.HeadlineID).Where(p.Key.Eq(key), p.Value.Eq(value))
	// 使用 In 操作符筛选这些 ID
	c.headDO.Where(h.Columns(h.ID).In(propertyQuery))
	return c
}

func (c *CardDB) FileFilter(fileID string) *CardDB {
	c.headDO = c.headDO.Where(dal.Headline.FileID.Eq(fileID))
	return c
}

func (c *CardDB) ParentFilter(parentID string) *CardDB {
	c.headDO = c.headDO.Where(dal.Headline.ParentID.Eq(parentID))
	return c
}

func (c *CardDB) AncestorFilter(ancestorID string) *CardDB {
	idList := getHeadIDByAncestorID(ancestorID)
	c.headDO = c.headDO.Where(dal.Headline.ID.In(idList...))
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
