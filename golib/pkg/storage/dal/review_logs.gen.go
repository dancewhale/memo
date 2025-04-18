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
	_reviewLog.DeletedAt = field.NewField(tableName, "deleted_at")
	_reviewLog.Rating = field.NewInt8(tableName, "rating")
	_reviewLog.ScheduledDays = field.NewUint64(tableName, "scheduled_days")
	_reviewLog.ElapsedDays = field.NewUint64(tableName, "elapsed_days")
	_reviewLog.Review = field.NewTime(tableName, "review")
	_reviewLog.State = field.NewInt8(tableName, "state")
	_reviewLog.CardDue = field.NewTime(tableName, "card_due")
	_reviewLog.CardStability = field.NewFloat64(tableName, "card_stability")
	_reviewLog.CardDifficulty = field.NewFloat64(tableName, "card_difficulty")
	_reviewLog.CardElapsedDays = field.NewUint64(tableName, "card_elapsed_days")
	_reviewLog.CardScheduledDays = field.NewUint64(tableName, "card_scheduled_days")
	_reviewLog.CardReps = field.NewUint64(tableName, "card_reps")
	_reviewLog.CardLapses = field.NewUint64(tableName, "card_lapses")
	_reviewLog.CardState = field.NewInt8(tableName, "card_state")
	_reviewLog.CardLastReview = field.NewTime(tableName, "card_last_review")
	_reviewLog.HeadlineID = field.NewString(tableName, "headline_id")

	_reviewLog.fillFieldMap()

	return _reviewLog
}

type reviewLog struct {
	reviewLogDo

	ALL               field.Asterisk
	ID                field.Uint
	CreatedAt         field.Time
	UpdatedAt         field.Time
	DeletedAt         field.Field
	Rating            field.Int8
	ScheduledDays     field.Uint64
	ElapsedDays       field.Uint64
	Review            field.Time
	State             field.Int8
	CardDue           field.Time
	CardStability     field.Float64
	CardDifficulty    field.Float64
	CardElapsedDays   field.Uint64
	CardScheduledDays field.Uint64
	CardReps          field.Uint64
	CardLapses        field.Uint64
	CardState         field.Int8
	CardLastReview    field.Time
	HeadlineID        field.String

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
	r.DeletedAt = field.NewField(table, "deleted_at")
	r.Rating = field.NewInt8(table, "rating")
	r.ScheduledDays = field.NewUint64(table, "scheduled_days")
	r.ElapsedDays = field.NewUint64(table, "elapsed_days")
	r.Review = field.NewTime(table, "review")
	r.State = field.NewInt8(table, "state")
	r.CardDue = field.NewTime(table, "card_due")
	r.CardStability = field.NewFloat64(table, "card_stability")
	r.CardDifficulty = field.NewFloat64(table, "card_difficulty")
	r.CardElapsedDays = field.NewUint64(table, "card_elapsed_days")
	r.CardScheduledDays = field.NewUint64(table, "card_scheduled_days")
	r.CardReps = field.NewUint64(table, "card_reps")
	r.CardLapses = field.NewUint64(table, "card_lapses")
	r.CardState = field.NewInt8(table, "card_state")
	r.CardLastReview = field.NewTime(table, "card_last_review")
	r.HeadlineID = field.NewString(table, "headline_id")

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
	r.fieldMap = make(map[string]field.Expr, 19)
	r.fieldMap["id"] = r.ID
	r.fieldMap["created_at"] = r.CreatedAt
	r.fieldMap["updated_at"] = r.UpdatedAt
	r.fieldMap["deleted_at"] = r.DeletedAt
	r.fieldMap["rating"] = r.Rating
	r.fieldMap["scheduled_days"] = r.ScheduledDays
	r.fieldMap["elapsed_days"] = r.ElapsedDays
	r.fieldMap["review"] = r.Review
	r.fieldMap["state"] = r.State
	r.fieldMap["card_due"] = r.CardDue
	r.fieldMap["card_stability"] = r.CardStability
	r.fieldMap["card_difficulty"] = r.CardDifficulty
	r.fieldMap["card_elapsed_days"] = r.CardElapsedDays
	r.fieldMap["card_scheduled_days"] = r.CardScheduledDays
	r.fieldMap["card_reps"] = r.CardReps
	r.fieldMap["card_lapses"] = r.CardLapses
	r.fieldMap["card_state"] = r.CardState
	r.fieldMap["card_last_review"] = r.CardLastReview
	r.fieldMap["headline_id"] = r.HeadlineID
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

type IReviewLogDo interface {
	gen.SubQuery
	Debug() IReviewLogDo
	WithContext(ctx context.Context) IReviewLogDo
	WithResult(fc func(tx gen.Dao)) gen.ResultInfo
	ReplaceDB(db *gorm.DB)
	ReadDB() IReviewLogDo
	WriteDB() IReviewLogDo
	As(alias string) gen.Dao
	Session(config *gorm.Session) IReviewLogDo
	Columns(cols ...field.Expr) gen.Columns
	Clauses(conds ...clause.Expression) IReviewLogDo
	Not(conds ...gen.Condition) IReviewLogDo
	Or(conds ...gen.Condition) IReviewLogDo
	Select(conds ...field.Expr) IReviewLogDo
	Where(conds ...gen.Condition) IReviewLogDo
	Order(conds ...field.Expr) IReviewLogDo
	Distinct(cols ...field.Expr) IReviewLogDo
	Omit(cols ...field.Expr) IReviewLogDo
	Join(table schema.Tabler, on ...field.Expr) IReviewLogDo
	LeftJoin(table schema.Tabler, on ...field.Expr) IReviewLogDo
	RightJoin(table schema.Tabler, on ...field.Expr) IReviewLogDo
	Group(cols ...field.Expr) IReviewLogDo
	Having(conds ...gen.Condition) IReviewLogDo
	Limit(limit int) IReviewLogDo
	Offset(offset int) IReviewLogDo
	Count() (count int64, err error)
	Scopes(funcs ...func(gen.Dao) gen.Dao) IReviewLogDo
	Unscoped() IReviewLogDo
	Create(values ...*storage.ReviewLog) error
	CreateInBatches(values []*storage.ReviewLog, batchSize int) error
	Save(values ...*storage.ReviewLog) error
	First() (*storage.ReviewLog, error)
	Take() (*storage.ReviewLog, error)
	Last() (*storage.ReviewLog, error)
	Find() ([]*storage.ReviewLog, error)
	FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.ReviewLog, err error)
	FindInBatches(result *[]*storage.ReviewLog, batchSize int, fc func(tx gen.Dao, batch int) error) error
	Pluck(column field.Expr, dest interface{}) error
	Delete(...*storage.ReviewLog) (info gen.ResultInfo, err error)
	Update(column field.Expr, value interface{}) (info gen.ResultInfo, err error)
	UpdateSimple(columns ...field.AssignExpr) (info gen.ResultInfo, err error)
	Updates(value interface{}) (info gen.ResultInfo, err error)
	UpdateColumn(column field.Expr, value interface{}) (info gen.ResultInfo, err error)
	UpdateColumnSimple(columns ...field.AssignExpr) (info gen.ResultInfo, err error)
	UpdateColumns(value interface{}) (info gen.ResultInfo, err error)
	UpdateFrom(q gen.SubQuery) gen.Dao
	Attrs(attrs ...field.AssignExpr) IReviewLogDo
	Assign(attrs ...field.AssignExpr) IReviewLogDo
	Joins(fields ...field.RelationField) IReviewLogDo
	Preload(fields ...field.RelationField) IReviewLogDo
	FirstOrInit() (*storage.ReviewLog, error)
	FirstOrCreate() (*storage.ReviewLog, error)
	FindByPage(offset int, limit int) (result []*storage.ReviewLog, count int64, err error)
	ScanByPage(result interface{}, offset int, limit int) (count int64, err error)
	Scan(result interface{}) (err error)
	Returning(value interface{}, columns ...string) IReviewLogDo
	UnderlyingDB() *gorm.DB
	schema.Tabler
}

func (r reviewLogDo) Debug() IReviewLogDo {
	return r.withDO(r.DO.Debug())
}

func (r reviewLogDo) WithContext(ctx context.Context) IReviewLogDo {
	return r.withDO(r.DO.WithContext(ctx))
}

func (r reviewLogDo) ReadDB() IReviewLogDo {
	return r.Clauses(dbresolver.Read)
}

func (r reviewLogDo) WriteDB() IReviewLogDo {
	return r.Clauses(dbresolver.Write)
}

func (r reviewLogDo) Session(config *gorm.Session) IReviewLogDo {
	return r.withDO(r.DO.Session(config))
}

func (r reviewLogDo) Clauses(conds ...clause.Expression) IReviewLogDo {
	return r.withDO(r.DO.Clauses(conds...))
}

func (r reviewLogDo) Returning(value interface{}, columns ...string) IReviewLogDo {
	return r.withDO(r.DO.Returning(value, columns...))
}

func (r reviewLogDo) Not(conds ...gen.Condition) IReviewLogDo {
	return r.withDO(r.DO.Not(conds...))
}

func (r reviewLogDo) Or(conds ...gen.Condition) IReviewLogDo {
	return r.withDO(r.DO.Or(conds...))
}

func (r reviewLogDo) Select(conds ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Select(conds...))
}

func (r reviewLogDo) Where(conds ...gen.Condition) IReviewLogDo {
	return r.withDO(r.DO.Where(conds...))
}

func (r reviewLogDo) Order(conds ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Order(conds...))
}

func (r reviewLogDo) Distinct(cols ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Distinct(cols...))
}

func (r reviewLogDo) Omit(cols ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Omit(cols...))
}

func (r reviewLogDo) Join(table schema.Tabler, on ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Join(table, on...))
}

func (r reviewLogDo) LeftJoin(table schema.Tabler, on ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.LeftJoin(table, on...))
}

func (r reviewLogDo) RightJoin(table schema.Tabler, on ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.RightJoin(table, on...))
}

func (r reviewLogDo) Group(cols ...field.Expr) IReviewLogDo {
	return r.withDO(r.DO.Group(cols...))
}

func (r reviewLogDo) Having(conds ...gen.Condition) IReviewLogDo {
	return r.withDO(r.DO.Having(conds...))
}

func (r reviewLogDo) Limit(limit int) IReviewLogDo {
	return r.withDO(r.DO.Limit(limit))
}

func (r reviewLogDo) Offset(offset int) IReviewLogDo {
	return r.withDO(r.DO.Offset(offset))
}

func (r reviewLogDo) Scopes(funcs ...func(gen.Dao) gen.Dao) IReviewLogDo {
	return r.withDO(r.DO.Scopes(funcs...))
}

func (r reviewLogDo) Unscoped() IReviewLogDo {
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

func (r reviewLogDo) Attrs(attrs ...field.AssignExpr) IReviewLogDo {
	return r.withDO(r.DO.Attrs(attrs...))
}

func (r reviewLogDo) Assign(attrs ...field.AssignExpr) IReviewLogDo {
	return r.withDO(r.DO.Assign(attrs...))
}

func (r reviewLogDo) Joins(fields ...field.RelationField) IReviewLogDo {
	for _, _f := range fields {
		r = *r.withDO(r.DO.Joins(_f))
	}
	return &r
}

func (r reviewLogDo) Preload(fields ...field.RelationField) IReviewLogDo {
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
