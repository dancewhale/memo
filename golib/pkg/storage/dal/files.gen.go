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

func newFile(db *gorm.DB, opts ...gen.DOOption) file {
	_file := file{}

	_file.fileDo.UseDB(db, opts...)
	_file.fileDo.UseModel(&storage.File{})

	tableName := _file.fileDo.TableName()
	_file.ALL = field.NewAsterisk(tableName)
	_file.ID = field.NewString(tableName, "id")
	_file.CreatedAt = field.NewTime(tableName, "created_at")
	_file.UpdatedAt = field.NewTime(tableName, "updated_at")
	_file.DeletedAt = field.NewField(tableName, "deleted_at")
	_file.FilePath = field.NewString(tableName, "file_path")
	_file.Hash = field.NewString(tableName, "hash")
	_file.Headlines = fileHasManyHeadlines{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Headlines", "storage.Headline"),
		File: struct {
			field.RelationField
			Headlines struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headlines.File", "storage.File"),
			Headlines: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headlines.File.Headlines", "storage.Headline"),
			},
		},
		Fsrs: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Headlines.Fsrs", "storage.FsrsInfo"),
		},
		Children: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Headlines.Children", "storage.Headline"),
		},
		ReviewLogs: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Headlines.ReviewLogs", "storage.ReviewLog"),
		},
		LogBook: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headlines.LogBook", "storage.Clock"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headlines.LogBook.Headline", "storage.Headline"),
			},
		},
		Locations: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Headlines.Locations", "storage.Location"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Headlines.Locations.Headline", "storage.Headline"),
			},
		},
	}

	_file.fillFieldMap()

	return _file
}

type file struct {
	fileDo

	ALL       field.Asterisk
	ID        field.String
	CreatedAt field.Time
	UpdatedAt field.Time
	DeletedAt field.Field
	FilePath  field.String
	Hash      field.String
	Headlines fileHasManyHeadlines

	fieldMap map[string]field.Expr
}

func (f file) Table(newTableName string) *file {
	f.fileDo.UseTable(newTableName)
	return f.updateTableName(newTableName)
}

func (f file) As(alias string) *file {
	f.fileDo.DO = *(f.fileDo.As(alias).(*gen.DO))
	return f.updateTableName(alias)
}

func (f *file) updateTableName(table string) *file {
	f.ALL = field.NewAsterisk(table)
	f.ID = field.NewString(table, "id")
	f.CreatedAt = field.NewTime(table, "created_at")
	f.UpdatedAt = field.NewTime(table, "updated_at")
	f.DeletedAt = field.NewField(table, "deleted_at")
	f.FilePath = field.NewString(table, "file_path")
	f.Hash = field.NewString(table, "hash")

	f.fillFieldMap()

	return f
}

func (f *file) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := f.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (f *file) fillFieldMap() {
	f.fieldMap = make(map[string]field.Expr, 7)
	f.fieldMap["id"] = f.ID
	f.fieldMap["created_at"] = f.CreatedAt
	f.fieldMap["updated_at"] = f.UpdatedAt
	f.fieldMap["deleted_at"] = f.DeletedAt
	f.fieldMap["file_path"] = f.FilePath
	f.fieldMap["hash"] = f.Hash

}

func (f file) clone(db *gorm.DB) file {
	f.fileDo.ReplaceConnPool(db.Statement.ConnPool)
	return f
}

func (f file) replaceDB(db *gorm.DB) file {
	f.fileDo.ReplaceDB(db)
	return f
}

type fileHasManyHeadlines struct {
	db *gorm.DB

	field.RelationField

	File struct {
		field.RelationField
		Headlines struct {
			field.RelationField
		}
	}
	Fsrs struct {
		field.RelationField
	}
	Children struct {
		field.RelationField
	}
	ReviewLogs struct {
		field.RelationField
	}
	LogBook struct {
		field.RelationField
		Headline struct {
			field.RelationField
		}
	}
	Locations struct {
		field.RelationField
		Headline struct {
			field.RelationField
		}
	}
}

func (a fileHasManyHeadlines) Where(conds ...field.Expr) *fileHasManyHeadlines {
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

func (a fileHasManyHeadlines) WithContext(ctx context.Context) *fileHasManyHeadlines {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a fileHasManyHeadlines) Session(session *gorm.Session) *fileHasManyHeadlines {
	a.db = a.db.Session(session)
	return &a
}

func (a fileHasManyHeadlines) Model(m *storage.File) *fileHasManyHeadlinesTx {
	return &fileHasManyHeadlinesTx{a.db.Model(m).Association(a.Name())}
}

type fileHasManyHeadlinesTx struct{ tx *gorm.Association }

func (a fileHasManyHeadlinesTx) Find() (result []*storage.Headline, err error) {
	return result, a.tx.Find(&result)
}

func (a fileHasManyHeadlinesTx) Append(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a fileHasManyHeadlinesTx) Replace(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a fileHasManyHeadlinesTx) Delete(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a fileHasManyHeadlinesTx) Clear() error {
	return a.tx.Clear()
}

func (a fileHasManyHeadlinesTx) Count() int64 {
	return a.tx.Count()
}

type fileDo struct{ gen.DO }

func (f fileDo) Debug() *fileDo {
	return f.withDO(f.DO.Debug())
}

func (f fileDo) WithContext(ctx context.Context) *fileDo {
	return f.withDO(f.DO.WithContext(ctx))
}

func (f fileDo) ReadDB() *fileDo {
	return f.Clauses(dbresolver.Read)
}

func (f fileDo) WriteDB() *fileDo {
	return f.Clauses(dbresolver.Write)
}

func (f fileDo) Session(config *gorm.Session) *fileDo {
	return f.withDO(f.DO.Session(config))
}

func (f fileDo) Clauses(conds ...clause.Expression) *fileDo {
	return f.withDO(f.DO.Clauses(conds...))
}

func (f fileDo) Returning(value interface{}, columns ...string) *fileDo {
	return f.withDO(f.DO.Returning(value, columns...))
}

func (f fileDo) Not(conds ...gen.Condition) *fileDo {
	return f.withDO(f.DO.Not(conds...))
}

func (f fileDo) Or(conds ...gen.Condition) *fileDo {
	return f.withDO(f.DO.Or(conds...))
}

func (f fileDo) Select(conds ...field.Expr) *fileDo {
	return f.withDO(f.DO.Select(conds...))
}

func (f fileDo) Where(conds ...gen.Condition) *fileDo {
	return f.withDO(f.DO.Where(conds...))
}

func (f fileDo) Order(conds ...field.Expr) *fileDo {
	return f.withDO(f.DO.Order(conds...))
}

func (f fileDo) Distinct(cols ...field.Expr) *fileDo {
	return f.withDO(f.DO.Distinct(cols...))
}

func (f fileDo) Omit(cols ...field.Expr) *fileDo {
	return f.withDO(f.DO.Omit(cols...))
}

func (f fileDo) Join(table schema.Tabler, on ...field.Expr) *fileDo {
	return f.withDO(f.DO.Join(table, on...))
}

func (f fileDo) LeftJoin(table schema.Tabler, on ...field.Expr) *fileDo {
	return f.withDO(f.DO.LeftJoin(table, on...))
}

func (f fileDo) RightJoin(table schema.Tabler, on ...field.Expr) *fileDo {
	return f.withDO(f.DO.RightJoin(table, on...))
}

func (f fileDo) Group(cols ...field.Expr) *fileDo {
	return f.withDO(f.DO.Group(cols...))
}

func (f fileDo) Having(conds ...gen.Condition) *fileDo {
	return f.withDO(f.DO.Having(conds...))
}

func (f fileDo) Limit(limit int) *fileDo {
	return f.withDO(f.DO.Limit(limit))
}

func (f fileDo) Offset(offset int) *fileDo {
	return f.withDO(f.DO.Offset(offset))
}

func (f fileDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *fileDo {
	return f.withDO(f.DO.Scopes(funcs...))
}

func (f fileDo) Unscoped() *fileDo {
	return f.withDO(f.DO.Unscoped())
}

func (f fileDo) Create(values ...*storage.File) error {
	if len(values) == 0 {
		return nil
	}
	return f.DO.Create(values)
}

func (f fileDo) CreateInBatches(values []*storage.File, batchSize int) error {
	return f.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (f fileDo) Save(values ...*storage.File) error {
	if len(values) == 0 {
		return nil
	}
	return f.DO.Save(values)
}

func (f fileDo) First() (*storage.File, error) {
	if result, err := f.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.File), nil
	}
}

func (f fileDo) Take() (*storage.File, error) {
	if result, err := f.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.File), nil
	}
}

func (f fileDo) Last() (*storage.File, error) {
	if result, err := f.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.File), nil
	}
}

func (f fileDo) Find() ([]*storage.File, error) {
	result, err := f.DO.Find()
	return result.([]*storage.File), err
}

func (f fileDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.File, err error) {
	buf := make([]*storage.File, 0, batchSize)
	err = f.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (f fileDo) FindInBatches(result *[]*storage.File, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return f.DO.FindInBatches(result, batchSize, fc)
}

func (f fileDo) Attrs(attrs ...field.AssignExpr) *fileDo {
	return f.withDO(f.DO.Attrs(attrs...))
}

func (f fileDo) Assign(attrs ...field.AssignExpr) *fileDo {
	return f.withDO(f.DO.Assign(attrs...))
}

func (f fileDo) Joins(fields ...field.RelationField) *fileDo {
	for _, _f := range fields {
		f = *f.withDO(f.DO.Joins(_f))
	}
	return &f
}

func (f fileDo) Preload(fields ...field.RelationField) *fileDo {
	for _, _f := range fields {
		f = *f.withDO(f.DO.Preload(_f))
	}
	return &f
}

func (f fileDo) FirstOrInit() (*storage.File, error) {
	if result, err := f.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.File), nil
	}
}

func (f fileDo) FirstOrCreate() (*storage.File, error) {
	if result, err := f.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.File), nil
	}
}

func (f fileDo) FindByPage(offset int, limit int) (result []*storage.File, count int64, err error) {
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

func (f fileDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = f.Count()
	if err != nil {
		return
	}

	err = f.Offset(offset).Limit(limit).Scan(result)
	return
}

func (f fileDo) Scan(result interface{}) (err error) {
	return f.DO.Scan(result)
}

func (f fileDo) Delete(models ...*storage.File) (result gen.ResultInfo, err error) {
	return f.DO.Delete(models)
}

func (f *fileDo) withDO(do gen.Dao) *fileDo {
	f.DO = *do.(*gen.DO)
	return f
}
