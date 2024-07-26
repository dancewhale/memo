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

func newFsrsInfo(db *gorm.DB, opts ...gen.DOOption) fsrsInfo {
	_fsrsInfo := fsrsInfo{}

	_fsrsInfo.fsrsInfoDo.UseDB(db, opts...)
	_fsrsInfo.fsrsInfoDo.UseModel(&storage.FsrsInfo{})

	tableName := _fsrsInfo.fsrsInfoDo.TableName()
	_fsrsInfo.ALL = field.NewAsterisk(tableName)
	_fsrsInfo.ID = field.NewUint(tableName, "id")
	_fsrsInfo.CreatedAt = field.NewTime(tableName, "created_at")
	_fsrsInfo.UpdatedAt = field.NewTime(tableName, "updated_at")
	_fsrsInfo.Due = field.NewTime(tableName, "due")
	_fsrsInfo.Stability = field.NewFloat64(tableName, "stability")
	_fsrsInfo.Difficulty = field.NewFloat64(tableName, "difficulty")
	_fsrsInfo.ElapsedDays = field.NewUint64(tableName, "elapsed_days")
	_fsrsInfo.ScheduledDays = field.NewUint64(tableName, "scheduled_days")
	_fsrsInfo.Reps = field.NewUint64(tableName, "reps")
	_fsrsInfo.Lapses = field.NewUint64(tableName, "lapses")
	_fsrsInfo.State = field.NewInt8(tableName, "state")
	_fsrsInfo.LastReview = field.NewTime(tableName, "last_review")
	_fsrsInfo.NoteID = field.NewUint(tableName, "note_id")

	_fsrsInfo.fillFieldMap()

	return _fsrsInfo
}

type fsrsInfo struct {
	fsrsInfoDo

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

func (f fsrsInfo) Table(newTableName string) *fsrsInfo {
	f.fsrsInfoDo.UseTable(newTableName)
	return f.updateTableName(newTableName)
}

func (f fsrsInfo) As(alias string) *fsrsInfo {
	f.fsrsInfoDo.DO = *(f.fsrsInfoDo.As(alias).(*gen.DO))
	return f.updateTableName(alias)
}

func (f *fsrsInfo) updateTableName(table string) *fsrsInfo {
	f.ALL = field.NewAsterisk(table)
	f.ID = field.NewUint(table, "id")
	f.CreatedAt = field.NewTime(table, "created_at")
	f.UpdatedAt = field.NewTime(table, "updated_at")
	f.Due = field.NewTime(table, "due")
	f.Stability = field.NewFloat64(table, "stability")
	f.Difficulty = field.NewFloat64(table, "difficulty")
	f.ElapsedDays = field.NewUint64(table, "elapsed_days")
	f.ScheduledDays = field.NewUint64(table, "scheduled_days")
	f.Reps = field.NewUint64(table, "reps")
	f.Lapses = field.NewUint64(table, "lapses")
	f.State = field.NewInt8(table, "state")
	f.LastReview = field.NewTime(table, "last_review")
	f.NoteID = field.NewUint(table, "note_id")

	f.fillFieldMap()

	return f
}

func (f *fsrsInfo) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := f.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (f *fsrsInfo) fillFieldMap() {
	f.fieldMap = make(map[string]field.Expr, 13)
	f.fieldMap["id"] = f.ID
	f.fieldMap["created_at"] = f.CreatedAt
	f.fieldMap["updated_at"] = f.UpdatedAt
	f.fieldMap["due"] = f.Due
	f.fieldMap["stability"] = f.Stability
	f.fieldMap["difficulty"] = f.Difficulty
	f.fieldMap["elapsed_days"] = f.ElapsedDays
	f.fieldMap["scheduled_days"] = f.ScheduledDays
	f.fieldMap["reps"] = f.Reps
	f.fieldMap["lapses"] = f.Lapses
	f.fieldMap["state"] = f.State
	f.fieldMap["last_review"] = f.LastReview
	f.fieldMap["note_id"] = f.NoteID
}

func (f fsrsInfo) clone(db *gorm.DB) fsrsInfo {
	f.fsrsInfoDo.ReplaceConnPool(db.Statement.ConnPool)
	return f
}

func (f fsrsInfo) replaceDB(db *gorm.DB) fsrsInfo {
	f.fsrsInfoDo.ReplaceDB(db)
	return f
}

type fsrsInfoDo struct{ gen.DO }

func (f fsrsInfoDo) Debug() *fsrsInfoDo {
	return f.withDO(f.DO.Debug())
}

func (f fsrsInfoDo) WithContext(ctx context.Context) *fsrsInfoDo {
	return f.withDO(f.DO.WithContext(ctx))
}

func (f fsrsInfoDo) ReadDB() *fsrsInfoDo {
	return f.Clauses(dbresolver.Read)
}

func (f fsrsInfoDo) WriteDB() *fsrsInfoDo {
	return f.Clauses(dbresolver.Write)
}

func (f fsrsInfoDo) Session(config *gorm.Session) *fsrsInfoDo {
	return f.withDO(f.DO.Session(config))
}

func (f fsrsInfoDo) Clauses(conds ...clause.Expression) *fsrsInfoDo {
	return f.withDO(f.DO.Clauses(conds...))
}

func (f fsrsInfoDo) Returning(value interface{}, columns ...string) *fsrsInfoDo {
	return f.withDO(f.DO.Returning(value, columns...))
}

func (f fsrsInfoDo) Not(conds ...gen.Condition) *fsrsInfoDo {
	return f.withDO(f.DO.Not(conds...))
}

func (f fsrsInfoDo) Or(conds ...gen.Condition) *fsrsInfoDo {
	return f.withDO(f.DO.Or(conds...))
}

func (f fsrsInfoDo) Select(conds ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Select(conds...))
}

func (f fsrsInfoDo) Where(conds ...gen.Condition) *fsrsInfoDo {
	return f.withDO(f.DO.Where(conds...))
}

func (f fsrsInfoDo) Order(conds ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Order(conds...))
}

func (f fsrsInfoDo) Distinct(cols ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Distinct(cols...))
}

func (f fsrsInfoDo) Omit(cols ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Omit(cols...))
}

func (f fsrsInfoDo) Join(table schema.Tabler, on ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Join(table, on...))
}

func (f fsrsInfoDo) LeftJoin(table schema.Tabler, on ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.LeftJoin(table, on...))
}

func (f fsrsInfoDo) RightJoin(table schema.Tabler, on ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.RightJoin(table, on...))
}

func (f fsrsInfoDo) Group(cols ...field.Expr) *fsrsInfoDo {
	return f.withDO(f.DO.Group(cols...))
}

func (f fsrsInfoDo) Having(conds ...gen.Condition) *fsrsInfoDo {
	return f.withDO(f.DO.Having(conds...))
}

func (f fsrsInfoDo) Limit(limit int) *fsrsInfoDo {
	return f.withDO(f.DO.Limit(limit))
}

func (f fsrsInfoDo) Offset(offset int) *fsrsInfoDo {
	return f.withDO(f.DO.Offset(offset))
}

func (f fsrsInfoDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *fsrsInfoDo {
	return f.withDO(f.DO.Scopes(funcs...))
}

func (f fsrsInfoDo) Unscoped() *fsrsInfoDo {
	return f.withDO(f.DO.Unscoped())
}

func (f fsrsInfoDo) Create(values ...*storage.FsrsInfo) error {
	if len(values) == 0 {
		return nil
	}
	return f.DO.Create(values)
}

func (f fsrsInfoDo) CreateInBatches(values []*storage.FsrsInfo, batchSize int) error {
	return f.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (f fsrsInfoDo) Save(values ...*storage.FsrsInfo) error {
	if len(values) == 0 {
		return nil
	}
	return f.DO.Save(values)
}

func (f fsrsInfoDo) First() (*storage.FsrsInfo, error) {
	if result, err := f.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.FsrsInfo), nil
	}
}

func (f fsrsInfoDo) Take() (*storage.FsrsInfo, error) {
	if result, err := f.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.FsrsInfo), nil
	}
}

func (f fsrsInfoDo) Last() (*storage.FsrsInfo, error) {
	if result, err := f.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.FsrsInfo), nil
	}
}

func (f fsrsInfoDo) Find() ([]*storage.FsrsInfo, error) {
	result, err := f.DO.Find()
	return result.([]*storage.FsrsInfo), err
}

func (f fsrsInfoDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.FsrsInfo, err error) {
	buf := make([]*storage.FsrsInfo, 0, batchSize)
	err = f.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (f fsrsInfoDo) FindInBatches(result *[]*storage.FsrsInfo, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return f.DO.FindInBatches(result, batchSize, fc)
}

func (f fsrsInfoDo) Attrs(attrs ...field.AssignExpr) *fsrsInfoDo {
	return f.withDO(f.DO.Attrs(attrs...))
}

func (f fsrsInfoDo) Assign(attrs ...field.AssignExpr) *fsrsInfoDo {
	return f.withDO(f.DO.Assign(attrs...))
}

func (f fsrsInfoDo) Joins(fields ...field.RelationField) *fsrsInfoDo {
	for _, _f := range fields {
		f = *f.withDO(f.DO.Joins(_f))
	}
	return &f
}

func (f fsrsInfoDo) Preload(fields ...field.RelationField) *fsrsInfoDo {
	for _, _f := range fields {
		f = *f.withDO(f.DO.Preload(_f))
	}
	return &f
}

func (f fsrsInfoDo) FirstOrInit() (*storage.FsrsInfo, error) {
	if result, err := f.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.FsrsInfo), nil
	}
}

func (f fsrsInfoDo) FirstOrCreate() (*storage.FsrsInfo, error) {
	if result, err := f.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.FsrsInfo), nil
	}
}

func (f fsrsInfoDo) FindByPage(offset int, limit int) (result []*storage.FsrsInfo, count int64, err error) {
	result, err = f.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = f.Offset(-1).Limit(-1).Count()
	return
}

func (f fsrsInfoDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = f.Count()
	if err != nil {
		return
	}

	err = f.Offset(offset).Limit(limit).Scan(result)
	return
}

func (f fsrsInfoDo) Scan(result interface{}) (err error) {
	return f.DO.Scan(result)
}

func (f fsrsInfoDo) Delete(models ...*storage.FsrsInfo) (result gen.ResultInfo, err error) {
	return f.DO.Delete(models)
}

func (f *fsrsInfoDo) withDO(do gen.Dao) *fsrsInfoDo {
	f.DO = *do.(*gen.DO)
	return f
}
