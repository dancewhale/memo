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

func newClock(db *gorm.DB, opts ...gen.DOOption) clock {
	_clock := clock{}

	_clock.clockDo.UseDB(db, opts...)
	_clock.clockDo.UseModel(&storage.Clock{})

	tableName := _clock.clockDo.TableName()
	_clock.ALL = field.NewAsterisk(tableName)
	_clock.ID = field.NewUint(tableName, "id")
	_clock.HeadlineID = field.NewString(tableName, "headline_id")
	_clock.Start = field.NewTime(tableName, "start")
	_clock.End = field.NewTime(tableName, "end")
	_clock.Headline = clockBelongsToHeadline{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Headline", "storage.Headline"),
		File: struct {
			field.RelationField
			Headlines struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headline.File", "storage.File"),
			Headlines: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headline.File.Headlines", "storage.Headline"),
			},
		},
		Properties: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headline.Properties", "storage.Property"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headline.Properties.Headline", "storage.Headline"),
			},
		},
		Children: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Headline.Children", "storage.Headline"),
		},
		LogBook: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headline.LogBook", "storage.Clock"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headline.LogBook.Headline", "storage.Headline"),
			},
		},
		Tags: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headline.Tags", "storage.Tag"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headline.Tags.Headline", "storage.Headline"),
			},
		},
	}

	_clock.fillFieldMap()

	return _clock
}

type clock struct {
	clockDo

	ALL        field.Asterisk
	ID         field.Uint
	HeadlineID field.String
	Start      field.Time
	End        field.Time
	Headline   clockBelongsToHeadline

	fieldMap map[string]field.Expr
}

func (c clock) Table(newTableName string) *clock {
	c.clockDo.UseTable(newTableName)
	return c.updateTableName(newTableName)
}

func (c clock) As(alias string) *clock {
	c.clockDo.DO = *(c.clockDo.As(alias).(*gen.DO))
	return c.updateTableName(alias)
}

func (c *clock) updateTableName(table string) *clock {
	c.ALL = field.NewAsterisk(table)
	c.ID = field.NewUint(table, "id")
	c.HeadlineID = field.NewString(table, "headline_id")
	c.Start = field.NewTime(table, "start")
	c.End = field.NewTime(table, "end")

	c.fillFieldMap()

	return c
}

func (c *clock) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := c.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (c *clock) fillFieldMap() {
	c.fieldMap = make(map[string]field.Expr, 5)
	c.fieldMap["id"] = c.ID
	c.fieldMap["headline_id"] = c.HeadlineID
	c.fieldMap["start"] = c.Start
	c.fieldMap["end"] = c.End

}

func (c clock) clone(db *gorm.DB) clock {
	c.clockDo.ReplaceConnPool(db.Statement.ConnPool)
	return c
}

func (c clock) replaceDB(db *gorm.DB) clock {
	c.clockDo.ReplaceDB(db)
	return c
}

type clockBelongsToHeadline struct {
	db *gorm.DB

	field.RelationField

	File struct {
		field.RelationField
		Headlines struct {
			field.RelationField
		}
	}
	Properties struct {
		field.RelationField
		Headline struct {
			field.RelationField
		}
	}
	Children struct {
		field.RelationField
	}
	LogBook struct {
		field.RelationField
		Headline struct {
			field.RelationField
		}
	}
	Tags struct {
		field.RelationField
		Headline struct {
			field.RelationField
		}
	}
}

func (a clockBelongsToHeadline) Where(conds ...field.Expr) *clockBelongsToHeadline {
	if len(conds) == 0 {
		return &a
	}

	exprs := make([]clause.Expression, 0, len(conds))
	for _, cond := range conds {
		exprs = append(exprs, cond.BeCond().(clause.Expression))
	}
	a.db = a.db.Clauses(clause.Where{Exprs: exprs})
	return &a
}

func (a clockBelongsToHeadline) WithContext(ctx context.Context) *clockBelongsToHeadline {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a clockBelongsToHeadline) Session(session *gorm.Session) *clockBelongsToHeadline {
	a.db = a.db.Session(session)
	return &a
}

func (a clockBelongsToHeadline) Model(m *storage.Clock) *clockBelongsToHeadlineTx {
	return &clockBelongsToHeadlineTx{a.db.Model(m).Association(a.Name())}
}

type clockBelongsToHeadlineTx struct{ tx *gorm.Association }

func (a clockBelongsToHeadlineTx) Find() (result *storage.Headline, err error) {
	return result, a.tx.Find(&result)
}

func (a clockBelongsToHeadlineTx) Append(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a clockBelongsToHeadlineTx) Replace(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a clockBelongsToHeadlineTx) Delete(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a clockBelongsToHeadlineTx) Clear() error {
	return a.tx.Clear()
}

func (a clockBelongsToHeadlineTx) Count() int64 {
	return a.tx.Count()
}

type clockDo struct{ gen.DO }

func (c clockDo) Debug() *clockDo {
	return c.withDO(c.DO.Debug())
}

func (c clockDo) WithContext(ctx context.Context) *clockDo {
	return c.withDO(c.DO.WithContext(ctx))
}

func (c clockDo) ReadDB() *clockDo {
	return c.Clauses(dbresolver.Read)
}

func (c clockDo) WriteDB() *clockDo {
	return c.Clauses(dbresolver.Write)
}

func (c clockDo) Session(config *gorm.Session) *clockDo {
	return c.withDO(c.DO.Session(config))
}

func (c clockDo) Clauses(conds ...clause.Expression) *clockDo {
	return c.withDO(c.DO.Clauses(conds...))
}

func (c clockDo) Returning(value interface{}, columns ...string) *clockDo {
	return c.withDO(c.DO.Returning(value, columns...))
}

func (c clockDo) Not(conds ...gen.Condition) *clockDo {
	return c.withDO(c.DO.Not(conds...))
}

func (c clockDo) Or(conds ...gen.Condition) *clockDo {
	return c.withDO(c.DO.Or(conds...))
}

func (c clockDo) Select(conds ...field.Expr) *clockDo {
	return c.withDO(c.DO.Select(conds...))
}

func (c clockDo) Where(conds ...gen.Condition) *clockDo {
	return c.withDO(c.DO.Where(conds...))
}

func (c clockDo) Order(conds ...field.Expr) *clockDo {
	return c.withDO(c.DO.Order(conds...))
}

func (c clockDo) Distinct(cols ...field.Expr) *clockDo {
	return c.withDO(c.DO.Distinct(cols...))
}

func (c clockDo) Omit(cols ...field.Expr) *clockDo {
	return c.withDO(c.DO.Omit(cols...))
}

func (c clockDo) Join(table schema.Tabler, on ...field.Expr) *clockDo {
	return c.withDO(c.DO.Join(table, on...))
}

func (c clockDo) LeftJoin(table schema.Tabler, on ...field.Expr) *clockDo {
	return c.withDO(c.DO.LeftJoin(table, on...))
}

func (c clockDo) RightJoin(table schema.Tabler, on ...field.Expr) *clockDo {
	return c.withDO(c.DO.RightJoin(table, on...))
}

func (c clockDo) Group(cols ...field.Expr) *clockDo {
	return c.withDO(c.DO.Group(cols...))
}

func (c clockDo) Having(conds ...gen.Condition) *clockDo {
	return c.withDO(c.DO.Having(conds...))
}

func (c clockDo) Limit(limit int) *clockDo {
	return c.withDO(c.DO.Limit(limit))
}

func (c clockDo) Offset(offset int) *clockDo {
	return c.withDO(c.DO.Offset(offset))
}

func (c clockDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *clockDo {
	return c.withDO(c.DO.Scopes(funcs...))
}

func (c clockDo) Unscoped() *clockDo {
	return c.withDO(c.DO.Unscoped())
}

func (c clockDo) Create(values ...*storage.Clock) error {
	if len(values) == 0 {
		return nil
	}
	return c.DO.Create(values)
}

func (c clockDo) CreateInBatches(values []*storage.Clock, batchSize int) error {
	return c.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (c clockDo) Save(values ...*storage.Clock) error {
	if len(values) == 0 {
		return nil
	}
	return c.DO.Save(values)
}

func (c clockDo) First() (*storage.Clock, error) {
	if result, err := c.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Clock), nil
	}
}

func (c clockDo) Take() (*storage.Clock, error) {
	if result, err := c.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Clock), nil
	}
}

func (c clockDo) Last() (*storage.Clock, error) {
	if result, err := c.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Clock), nil
	}
}

func (c clockDo) Find() ([]*storage.Clock, error) {
	result, err := c.DO.Find()
	return result.([]*storage.Clock), err
}

func (c clockDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Clock, err error) {
	buf := make([]*storage.Clock, 0, batchSize)
	err = c.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (c clockDo) FindInBatches(result *[]*storage.Clock, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return c.DO.FindInBatches(result, batchSize, fc)
}

func (c clockDo) Attrs(attrs ...field.AssignExpr) *clockDo {
	return c.withDO(c.DO.Attrs(attrs...))
}

func (c clockDo) Assign(attrs ...field.AssignExpr) *clockDo {
	return c.withDO(c.DO.Assign(attrs...))
}

func (c clockDo) Joins(fields ...field.RelationField) *clockDo {
	for _, _f := range fields {
		c = *c.withDO(c.DO.Joins(_f))
	}
	return &c
}

func (c clockDo) Preload(fields ...field.RelationField) *clockDo {
	for _, _f := range fields {
		c = *c.withDO(c.DO.Preload(_f))
	}
	return &c
}

func (c clockDo) FirstOrInit() (*storage.Clock, error) {
	if result, err := c.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Clock), nil
	}
}

func (c clockDo) FirstOrCreate() (*storage.Clock, error) {
	if result, err := c.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Clock), nil
	}
}

func (c clockDo) FindByPage(offset int, limit int) (result []*storage.Clock, count int64, err error) {
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

func (c clockDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = c.Count()
	if err != nil {
		return
	}

	err = c.Offset(offset).Limit(limit).Scan(result)
	return
}

func (c clockDo) Scan(result interface{}) (err error) {
	return c.DO.Scan(result)
}

func (c clockDo) Delete(models ...*storage.Clock) (result gen.ResultInfo, err error) {
	return c.DO.Delete(models)
}

func (c *clockDo) withDO(do gen.Dao) *clockDo {
	c.DO = *do.(*gen.DO)
	return c
}
