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

func newReviewLog(db *gorm.DB, opts ...gen.DOOption) reviewLog {
	_reviewLog := reviewLog{}

	_reviewLog.reviewLogDo.UseDB(db, opts...)
	_reviewLog.reviewLogDo.UseModel(&storage.ReviewLog{})

	tableName := _reviewLog.reviewLogDo.TableName()
	_reviewLog.ALL = field.NewAsterisk(tableName)
	_reviewLog.ID = field.NewUint(tableName, "id")
	_reviewLog.CreatedAt = field.NewTime(tableName, "created_at")
	_reviewLog.UpdatedAt = field.NewTime(tableName, "updated_at")
	_reviewLog.Rating = field.NewInt8(tableName, "rating")
	_reviewLog.ScheduledDays = field.NewUint64(tableName, "scheduled_days")
	_reviewLog.ElapsedDays = field.NewUint64(tableName, "elapsed_days")
	_reviewLog.Review = field.NewTime(tableName, "review")
	_reviewLog.State = field.NewInt8(tableName, "state")
	_reviewLog.NoteOrgid = field.NewString(tableName, "note_orgid")

	_reviewLog.fillFieldMap()

	return _reviewLog
}

type reviewLog struct {
	reviewLogDo

	ALL           field.Asterisk
	ID            field.Uint
	CreatedAt     field.Time
	UpdatedAt     field.Time
	Rating        field.Int8
	ScheduledDays field.Uint64
	ElapsedDays   field.Uint64
	Review        field.Time
	State         field.Int8
	NoteOrgid     field.String

	fieldMap map[string]field.Expr
}

func (r reviewLog) Table(newTableName string) *reviewLog {
	r.reviewLogDo.UseTable(newTableName)
	return r.updateTableName(newTableName)
}

func (r reviewLog) As(alias string) *reviewLog {
	r.reviewLogDo.DO = *(r.reviewLogDo.As(alias).(*gen.DO))
	return r.updateTableName(alias)
}

func (r *reviewLog) updateTableName(table string) *reviewLog {
	r.ALL = field.NewAsterisk(table)
	r.ID = field.NewUint(table, "id")
	r.CreatedAt = field.NewTime(table, "created_at")
	r.UpdatedAt = field.NewTime(table, "updated_at")
	r.Rating = field.NewInt8(table, "rating")
	r.ScheduledDays = field.NewUint64(table, "scheduled_days")
	r.ElapsedDays = field.NewUint64(table, "elapsed_days")
	r.Review = field.NewTime(table, "review")
	r.State = field.NewInt8(table, "state")
	r.NoteOrgid = field.NewString(table, "note_orgid")

	r.fillFieldMap()

	return r
}

func (r *reviewLog) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := r.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (r *reviewLog) fillFieldMap() {
	r.fieldMap = make(map[string]field.Expr, 9)
	r.fieldMap["id"] = r.ID
	r.fieldMap["created_at"] = r.CreatedAt
	r.fieldMap["updated_at"] = r.UpdatedAt
	r.fieldMap["rating"] = r.Rating
	r.fieldMap["scheduled_days"] = r.ScheduledDays
	r.fieldMap["elapsed_days"] = r.ElapsedDays
	r.fieldMap["review"] = r.Review
	r.fieldMap["state"] = r.State
	r.fieldMap["note_orgid"] = r.NoteOrgid
}

func (r reviewLog) clone(db *gorm.DB) reviewLog {
	r.reviewLogDo.ReplaceConnPool(db.Statement.ConnPool)
	return r
}

func (r reviewLog) replaceDB(db *gorm.DB) reviewLog {
	r.reviewLogDo.ReplaceDB(db)
	return r
}

type reviewLogDo struct{ gen.DO }

func (r reviewLogDo) Debug() *reviewLogDo {
	return r.withDO(r.DO.Debug())
}

func (r reviewLogDo) WithContext(ctx context.Context) *reviewLogDo {
	return r.withDO(r.DO.WithContext(ctx))
}

func (r reviewLogDo) ReadDB() *reviewLogDo {
	return r.Clauses(dbresolver.Read)
}

func (r reviewLogDo) WriteDB() *reviewLogDo {
	return r.Clauses(dbresolver.Write)
}

func (r reviewLogDo) Session(config *gorm.Session) *reviewLogDo {
	return r.withDO(r.DO.Session(config))
}

func (r reviewLogDo) Clauses(conds ...clause.Expression) *reviewLogDo {
	return r.withDO(r.DO.Clauses(conds...))
}

func (r reviewLogDo) Returning(value interface{}, columns ...string) *reviewLogDo {
	return r.withDO(r.DO.Returning(value, columns...))
}

func (r reviewLogDo) Not(conds ...gen.Condition) *reviewLogDo {
	return r.withDO(r.DO.Not(conds...))
}

func (r reviewLogDo) Or(conds ...gen.Condition) *reviewLogDo {
	return r.withDO(r.DO.Or(conds...))
}

func (r reviewLogDo) Select(conds ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Select(conds...))
}

func (r reviewLogDo) Where(conds ...gen.Condition) *reviewLogDo {
	return r.withDO(r.DO.Where(conds...))
}

func (r reviewLogDo) Order(conds ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Order(conds...))
}

func (r reviewLogDo) Distinct(cols ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Distinct(cols...))
}

func (r reviewLogDo) Omit(cols ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Omit(cols...))
}

func (r reviewLogDo) Join(table schema.Tabler, on ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Join(table, on...))
}

func (r reviewLogDo) LeftJoin(table schema.Tabler, on ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.LeftJoin(table, on...))
}

func (r reviewLogDo) RightJoin(table schema.Tabler, on ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.RightJoin(table, on...))
}

func (r reviewLogDo) Group(cols ...field.Expr) *reviewLogDo {
	return r.withDO(r.DO.Group(cols...))
}

func (r reviewLogDo) Having(conds ...gen.Condition) *reviewLogDo {
	return r.withDO(r.DO.Having(conds...))
}

func (r reviewLogDo) Limit(limit int) *reviewLogDo {
	return r.withDO(r.DO.Limit(limit))
}

func (r reviewLogDo) Offset(offset int) *reviewLogDo {
	return r.withDO(r.DO.Offset(offset))
}

func (r reviewLogDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *reviewLogDo {
	return r.withDO(r.DO.Scopes(funcs...))
}

func (r reviewLogDo) Unscoped() *reviewLogDo {
	return r.withDO(r.DO.Unscoped())
}

func (r reviewLogDo) Create(values ...*storage.ReviewLog) error {
	if len(values) == 0 {
		return nil
	}
	return r.DO.Create(values)
}

func (r reviewLogDo) CreateInBatches(values []*storage.ReviewLog, batchSize int) error {
	return r.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (r reviewLogDo) Save(values ...*storage.ReviewLog) error {
	if len(values) == 0 {
		return nil
	}
	return r.DO.Save(values)
}

func (r reviewLogDo) First() (*storage.ReviewLog, error) {
	if result, err := r.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.ReviewLog), nil
	}
}

func (r reviewLogDo) Take() (*storage.ReviewLog, error) {
	if result, err := r.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.ReviewLog), nil
	}
}

func (r reviewLogDo) Last() (*storage.ReviewLog, error) {
	if result, err := r.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.ReviewLog), nil
	}
}

func (r reviewLogDo) Find() ([]*storage.ReviewLog, error) {
	result, err := r.DO.Find()
	return result.([]*storage.ReviewLog), err
}

func (r reviewLogDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.ReviewLog, err error) {
	buf := make([]*storage.ReviewLog, 0, batchSize)
	err = r.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (r reviewLogDo) FindInBatches(result *[]*storage.ReviewLog, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return r.DO.FindInBatches(result, batchSize, fc)
}

func (r reviewLogDo) Attrs(attrs ...field.AssignExpr) *reviewLogDo {
	return r.withDO(r.DO.Attrs(attrs...))
}

func (r reviewLogDo) Assign(attrs ...field.AssignExpr) *reviewLogDo {
	return r.withDO(r.DO.Assign(attrs...))
}

func (r reviewLogDo) Joins(fields ...field.RelationField) *reviewLogDo {
	for _, _f := range fields {
		r = *r.withDO(r.DO.Joins(_f))
	}
	return &r
}

func (r reviewLogDo) Preload(fields ...field.RelationField) *reviewLogDo {
	for _, _f := range fields {
		r = *r.withDO(r.DO.Preload(_f))
	}
	return &r
}

func (r reviewLogDo) FirstOrInit() (*storage.ReviewLog, error) {
	if result, err := r.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.ReviewLog), nil
	}
}

func (r reviewLogDo) FirstOrCreate() (*storage.ReviewLog, error) {
	if result, err := r.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.ReviewLog), nil
	}
}

func (r reviewLogDo) FindByPage(offset int, limit int) (result []*storage.ReviewLog, count int64, err error) {
	result, err = r.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = r.Offset(-1).Limit(-1).Count()
	return
}

func (r reviewLogDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = r.Count()
	if err != nil {
		return
	}

	err = r.Offset(offset).Limit(limit).Scan(result)
	return
}

func (r reviewLogDo) Scan(result interface{}) (err error) {
	return r.DO.Scan(result)
}

func (r reviewLogDo) Delete(models ...*storage.ReviewLog) (result gen.ResultInfo, err error) {
	return r.DO.Delete(models)
}

func (r *reviewLogDo) withDO(do gen.Dao) *reviewLogDo {
	r.DO = *do.(*gen.DO)
	return r
}
