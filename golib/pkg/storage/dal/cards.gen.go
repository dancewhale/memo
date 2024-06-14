// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.

package dal

import (
	"context"

	"gorm.io/gorm"
	"gorm.io/gorm/clause"
	"gorm.io/gorm/schema"

	"gorm.io/gen"
	"gorm.io/gen/field"

	"gorm.io/plugin/dbresolver"

	"memo/pkg/storage"
)

func newCard(db *gorm.DB, opts ...gen.DOOption) card {
	_card := card{}

	_card.cardDo.UseDB(db, opts...)
	_card.cardDo.UseModel(&storage.Card{})

	tableName := _card.cardDo.TableName()
	_card.ALL = field.NewAsterisk(tableName)
	_card.ID = field.NewUint(tableName, "id")
	_card.CreatedAt = field.NewTime(tableName, "created_at")
	_card.UpdatedAt = field.NewTime(tableName, "updated_at")
	_card.Due = field.NewTime(tableName, "due")
	_card.Stability = field.NewFloat64(tableName, "stability")
	_card.Difficulty = field.NewFloat64(tableName, "difficulty")
	_card.ElapsedDays = field.NewUint64(tableName, "elapsed_days")
	_card.ScheduledDays = field.NewUint64(tableName, "scheduled_days")
	_card.Reps = field.NewUint64(tableName, "reps")
	_card.Lapses = field.NewUint64(tableName, "lapses")
	_card.State = field.NewInt8(tableName, "state")
	_card.LastReview = field.NewTime(tableName, "last_review")
	_card.NoteID = field.NewUint(tableName, "note_id")

	_card.fillFieldMap()

	return _card
}

type card struct {
	cardDo

	ALL           field.Asterisk
	ID            field.Uint
	CreatedAt     field.Time
	UpdatedAt     field.Time
	Due           field.Time
	Stability     field.Float64
	Difficulty    field.Float64
	ElapsedDays   field.Uint64
	ScheduledDays field.Uint64
	Reps          field.Uint64
	Lapses        field.Uint64
	State         field.Int8
	LastReview    field.Time
	NoteID        field.Uint

	fieldMap map[string]field.Expr
}

func (c card) Table(newTableName string) *card {
	c.cardDo.UseTable(newTableName)
	return c.updateTableName(newTableName)
}

func (c card) As(alias string) *card {
	c.cardDo.DO = *(c.cardDo.As(alias).(*gen.DO))
	return c.updateTableName(alias)
}

func (c *card) updateTableName(table string) *card {
	c.ALL = field.NewAsterisk(table)
	c.ID = field.NewUint(table, "id")
	c.CreatedAt = field.NewTime(table, "created_at")
	c.UpdatedAt = field.NewTime(table, "updated_at")
	c.Due = field.NewTime(table, "due")
	c.Stability = field.NewFloat64(table, "stability")
	c.Difficulty = field.NewFloat64(table, "difficulty")
	c.ElapsedDays = field.NewUint64(table, "elapsed_days")
	c.ScheduledDays = field.NewUint64(table, "scheduled_days")
	c.Reps = field.NewUint64(table, "reps")
	c.Lapses = field.NewUint64(table, "lapses")
	c.State = field.NewInt8(table, "state")
	c.LastReview = field.NewTime(table, "last_review")
	c.NoteID = field.NewUint(table, "note_id")

	c.fillFieldMap()

	return c
}

func (c *card) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := c.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (c *card) fillFieldMap() {
	c.fieldMap = make(map[string]field.Expr, 13)
	c.fieldMap["id"] = c.ID
	c.fieldMap["created_at"] = c.CreatedAt
	c.fieldMap["updated_at"] = c.UpdatedAt
	c.fieldMap["due"] = c.Due
	c.fieldMap["stability"] = c.Stability
	c.fieldMap["difficulty"] = c.Difficulty
	c.fieldMap["elapsed_days"] = c.ElapsedDays
	c.fieldMap["scheduled_days"] = c.ScheduledDays
	c.fieldMap["reps"] = c.Reps
	c.fieldMap["lapses"] = c.Lapses
	c.fieldMap["state"] = c.State
	c.fieldMap["last_review"] = c.LastReview
	c.fieldMap["note_id"] = c.NoteID
}

func (c card) clone(db *gorm.DB) card {
	c.cardDo.ReplaceConnPool(db.Statement.ConnPool)
	return c
}

func (c card) replaceDB(db *gorm.DB) card {
	c.cardDo.ReplaceDB(db)
	return c
}

type cardDo struct{ gen.DO }

func (c cardDo) Debug() *cardDo {
	return c.withDO(c.DO.Debug())
}

func (c cardDo) WithContext(ctx context.Context) *cardDo {
	return c.withDO(c.DO.WithContext(ctx))
}

func (c cardDo) ReadDB() *cardDo {
	return c.Clauses(dbresolver.Read)
}

func (c cardDo) WriteDB() *cardDo {
	return c.Clauses(dbresolver.Write)
}

func (c cardDo) Session(config *gorm.Session) *cardDo {
	return c.withDO(c.DO.Session(config))
}

func (c cardDo) Clauses(conds ...clause.Expression) *cardDo {
	return c.withDO(c.DO.Clauses(conds...))
}

func (c cardDo) Returning(value interface{}, columns ...string) *cardDo {
	return c.withDO(c.DO.Returning(value, columns...))
}

func (c cardDo) Not(conds ...gen.Condition) *cardDo {
	return c.withDO(c.DO.Not(conds...))
}

func (c cardDo) Or(conds ...gen.Condition) *cardDo {
	return c.withDO(c.DO.Or(conds...))
}

func (c cardDo) Select(conds ...field.Expr) *cardDo {
	return c.withDO(c.DO.Select(conds...))
}

func (c cardDo) Where(conds ...gen.Condition) *cardDo {
	return c.withDO(c.DO.Where(conds...))
}

func (c cardDo) Order(conds ...field.Expr) *cardDo {
	return c.withDO(c.DO.Order(conds...))
}

func (c cardDo) Distinct(cols ...field.Expr) *cardDo {
	return c.withDO(c.DO.Distinct(cols...))
}

func (c cardDo) Omit(cols ...field.Expr) *cardDo {
	return c.withDO(c.DO.Omit(cols...))
}

func (c cardDo) Join(table schema.Tabler, on ...field.Expr) *cardDo {
	return c.withDO(c.DO.Join(table, on...))
}

func (c cardDo) LeftJoin(table schema.Tabler, on ...field.Expr) *cardDo {
	return c.withDO(c.DO.LeftJoin(table, on...))
}

func (c cardDo) RightJoin(table schema.Tabler, on ...field.Expr) *cardDo {
	return c.withDO(c.DO.RightJoin(table, on...))
}

func (c cardDo) Group(cols ...field.Expr) *cardDo {
	return c.withDO(c.DO.Group(cols...))
}

func (c cardDo) Having(conds ...gen.Condition) *cardDo {
	return c.withDO(c.DO.Having(conds...))
}

func (c cardDo) Limit(limit int) *cardDo {
	return c.withDO(c.DO.Limit(limit))
}

func (c cardDo) Offset(offset int) *cardDo {
	return c.withDO(c.DO.Offset(offset))
}

func (c cardDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *cardDo {
	return c.withDO(c.DO.Scopes(funcs...))
}

func (c cardDo) Unscoped() *cardDo {
	return c.withDO(c.DO.Unscoped())
}

func (c cardDo) Create(values ...*storage.Card) error {
	if len(values) == 0 {
		return nil
	}
	return c.DO.Create(values)
}

func (c cardDo) CreateInBatches(values []*storage.Card, batchSize int) error {
	return c.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (c cardDo) Save(values ...*storage.Card) error {
	if len(values) == 0 {
		return nil
	}
	return c.DO.Save(values)
}

func (c cardDo) First() (*storage.Card, error) {
	if result, err := c.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Card), nil
	}
}

func (c cardDo) Take() (*storage.Card, error) {
	if result, err := c.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Card), nil
	}
}

func (c cardDo) Last() (*storage.Card, error) {
	if result, err := c.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Card), nil
	}
}

func (c cardDo) Find() ([]*storage.Card, error) {
	result, err := c.DO.Find()
	return result.([]*storage.Card), err
}

func (c cardDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Card, err error) {
	buf := make([]*storage.Card, 0, batchSize)
	err = c.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (c cardDo) FindInBatches(result *[]*storage.Card, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return c.DO.FindInBatches(result, batchSize, fc)
}

func (c cardDo) Attrs(attrs ...field.AssignExpr) *cardDo {
	return c.withDO(c.DO.Attrs(attrs...))
}

func (c cardDo) Assign(attrs ...field.AssignExpr) *cardDo {
	return c.withDO(c.DO.Assign(attrs...))
}

func (c cardDo) Joins(fields ...field.RelationField) *cardDo {
	for _, _f := range fields {
		c = *c.withDO(c.DO.Joins(_f))
	}
	return &c
}

func (c cardDo) Preload(fields ...field.RelationField) *cardDo {
	for _, _f := range fields {
		c = *c.withDO(c.DO.Preload(_f))
	}
	return &c
}

func (c cardDo) FirstOrInit() (*storage.Card, error) {
	if result, err := c.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Card), nil
	}
}

func (c cardDo) FirstOrCreate() (*storage.Card, error) {
	if result, err := c.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Card), nil
	}
}

func (c cardDo) FindByPage(offset int, limit int) (result []*storage.Card, count int64, err error) {
	result, err = c.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = c.Offset(-1).Limit(-1).Count()
	return
}

func (c cardDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = c.Count()
	if err != nil {
		return
	}

	err = c.Offset(offset).Limit(limit).Scan(result)
	return
}

func (c cardDo) Scan(result interface{}) (err error) {
	return c.DO.Scan(result)
}

func (c cardDo) Delete(models ...*storage.Card) (result gen.ResultInfo, err error) {
	return c.DO.Delete(models)
}

func (c *cardDo) withDO(do gen.Dao) *cardDo {
	c.DO = *do.(*gen.DO)
	return c
}
