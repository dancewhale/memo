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

func newLocation(db *gorm.DB, opts ...gen.DOOption) location {
	_location := location{}

	_location.locationDo.UseDB(db, opts...)
	_location.locationDo.UseModel(&storage.Location{})

	tableName := _location.locationDo.TableName()
	_location.ALL = field.NewAsterisk(tableName)
	_location.ID = field.NewUint(tableName, "id")
	_location.Protocol = field.NewString(tableName, "protocol")
	_location.Link = field.NewString(tableName, "link")
	_location.ExLink = field.NewString(tableName, "ex_link")
	_location.Type = field.NewString(tableName, "type")

	_location.fillFieldMap()

	return _location
}

type location struct {
	locationDo

	ALL      field.Asterisk
	ID       field.Uint
	Protocol field.String
	Link     field.String
	ExLink   field.String
	Type     field.String

	fieldMap map[string]field.Expr
}

func (l location) Table(newTableName string) *location {
	l.locationDo.UseTable(newTableName)
	return l.updateTableName(newTableName)
}

func (l location) As(alias string) *location {
	l.locationDo.DO = *(l.locationDo.As(alias).(*gen.DO))
	return l.updateTableName(alias)
}

func (l *location) updateTableName(table string) *location {
	l.ALL = field.NewAsterisk(table)
	l.ID = field.NewUint(table, "id")
	l.Protocol = field.NewString(table, "protocol")
	l.Link = field.NewString(table, "link")
	l.ExLink = field.NewString(table, "ex_link")
	l.Type = field.NewString(table, "type")

	l.fillFieldMap()

	return l
}

func (l *location) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := l.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (l *location) fillFieldMap() {
	l.fieldMap = make(map[string]field.Expr, 5)
	l.fieldMap["id"] = l.ID
	l.fieldMap["protocol"] = l.Protocol
	l.fieldMap["link"] = l.Link
	l.fieldMap["ex_link"] = l.ExLink
	l.fieldMap["type"] = l.Type
}

func (l location) clone(db *gorm.DB) location {
	l.locationDo.ReplaceConnPool(db.Statement.ConnPool)
	return l
}

func (l location) replaceDB(db *gorm.DB) location {
	l.locationDo.ReplaceDB(db)
	return l
}

type locationDo struct{ gen.DO }

func (l locationDo) Debug() *locationDo {
	return l.withDO(l.DO.Debug())
}

func (l locationDo) WithContext(ctx context.Context) *locationDo {
	return l.withDO(l.DO.WithContext(ctx))
}

func (l locationDo) ReadDB() *locationDo {
	return l.Clauses(dbresolver.Read)
}

func (l locationDo) WriteDB() *locationDo {
	return l.Clauses(dbresolver.Write)
}

func (l locationDo) Session(config *gorm.Session) *locationDo {
	return l.withDO(l.DO.Session(config))
}

func (l locationDo) Clauses(conds ...clause.Expression) *locationDo {
	return l.withDO(l.DO.Clauses(conds...))
}

func (l locationDo) Returning(value interface{}, columns ...string) *locationDo {
	return l.withDO(l.DO.Returning(value, columns...))
}

func (l locationDo) Not(conds ...gen.Condition) *locationDo {
	return l.withDO(l.DO.Not(conds...))
}

func (l locationDo) Or(conds ...gen.Condition) *locationDo {
	return l.withDO(l.DO.Or(conds...))
}

func (l locationDo) Select(conds ...field.Expr) *locationDo {
	return l.withDO(l.DO.Select(conds...))
}

func (l locationDo) Where(conds ...gen.Condition) *locationDo {
	return l.withDO(l.DO.Where(conds...))
}

func (l locationDo) Order(conds ...field.Expr) *locationDo {
	return l.withDO(l.DO.Order(conds...))
}

func (l locationDo) Distinct(cols ...field.Expr) *locationDo {
	return l.withDO(l.DO.Distinct(cols...))
}

func (l locationDo) Omit(cols ...field.Expr) *locationDo {
	return l.withDO(l.DO.Omit(cols...))
}

func (l locationDo) Join(table schema.Tabler, on ...field.Expr) *locationDo {
	return l.withDO(l.DO.Join(table, on...))
}

func (l locationDo) LeftJoin(table schema.Tabler, on ...field.Expr) *locationDo {
	return l.withDO(l.DO.LeftJoin(table, on...))
}

func (l locationDo) RightJoin(table schema.Tabler, on ...field.Expr) *locationDo {
	return l.withDO(l.DO.RightJoin(table, on...))
}

func (l locationDo) Group(cols ...field.Expr) *locationDo {
	return l.withDO(l.DO.Group(cols...))
}

func (l locationDo) Having(conds ...gen.Condition) *locationDo {
	return l.withDO(l.DO.Having(conds...))
}

func (l locationDo) Limit(limit int) *locationDo {
	return l.withDO(l.DO.Limit(limit))
}

func (l locationDo) Offset(offset int) *locationDo {
	return l.withDO(l.DO.Offset(offset))
}

func (l locationDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *locationDo {
	return l.withDO(l.DO.Scopes(funcs...))
}

func (l locationDo) Unscoped() *locationDo {
	return l.withDO(l.DO.Unscoped())
}

func (l locationDo) Create(values ...*storage.Location) error {
	if len(values) == 0 {
		return nil
	}
	return l.DO.Create(values)
}

func (l locationDo) CreateInBatches(values []*storage.Location, batchSize int) error {
	return l.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (l locationDo) Save(values ...*storage.Location) error {
	if len(values) == 0 {
		return nil
	}
	return l.DO.Save(values)
}

func (l locationDo) First() (*storage.Location, error) {
	if result, err := l.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Location), nil
	}
}

func (l locationDo) Take() (*storage.Location, error) {
	if result, err := l.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Location), nil
	}
}

func (l locationDo) Last() (*storage.Location, error) {
	if result, err := l.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Location), nil
	}
}

func (l locationDo) Find() ([]*storage.Location, error) {
	result, err := l.DO.Find()
	return result.([]*storage.Location), err
}

func (l locationDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Location, err error) {
	buf := make([]*storage.Location, 0, batchSize)
	err = l.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (l locationDo) FindInBatches(result *[]*storage.Location, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return l.DO.FindInBatches(result, batchSize, fc)
}

func (l locationDo) Attrs(attrs ...field.AssignExpr) *locationDo {
	return l.withDO(l.DO.Attrs(attrs...))
}

func (l locationDo) Assign(attrs ...field.AssignExpr) *locationDo {
	return l.withDO(l.DO.Assign(attrs...))
}

func (l locationDo) Joins(fields ...field.RelationField) *locationDo {
	for _, _f := range fields {
		l = *l.withDO(l.DO.Joins(_f))
	}
	return &l
}

func (l locationDo) Preload(fields ...field.RelationField) *locationDo {
	for _, _f := range fields {
		l = *l.withDO(l.DO.Preload(_f))
	}
	return &l
}

func (l locationDo) FirstOrInit() (*storage.Location, error) {
	if result, err := l.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Location), nil
	}
}

func (l locationDo) FirstOrCreate() (*storage.Location, error) {
	if result, err := l.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Location), nil
	}
}

func (l locationDo) FindByPage(offset int, limit int) (result []*storage.Location, count int64, err error) {
	result, err = l.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = l.Offset(-1).Limit(-1).Count()
	return
}

func (l locationDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = l.Count()
	if err != nil {
		return
	}

	err = l.Offset(offset).Limit(limit).Scan(result)
	return
}

func (l locationDo) Scan(result interface{}) (err error) {
	return l.DO.Scan(result)
}

func (l locationDo) Delete(models ...*storage.Location) (result gen.ResultInfo, err error) {
	return l.DO.Delete(models)
}

func (l *locationDo) withDO(do gen.Dao) *locationDo {
	l.DO = *do.(*gen.DO)
	return l
}
