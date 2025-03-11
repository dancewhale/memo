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

func newProperty(db *gorm.DB, opts ...gen.DOOption) property {
	_property := property{}

	_property.propertyDo.UseDB(db, opts...)
	_property.propertyDo.UseModel(&storage.Property{})

	tableName := _property.propertyDo.TableName()
	_property.ALL = field.NewAsterisk(tableName)
	_property.HeadlineID = field.NewString(tableName, "headline_id")
	_property.Key = field.NewString(tableName, "key")
	_property.Value = field.NewString(tableName, "value")
	_property.Headline = propertyBelongsToHeadline{
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
		}{
			RelationField: field.NewRelation("Headline.Tags", "storage.Tag"),
		},
	}

	_property.fillFieldMap()

	return _property
}

type property struct {
	propertyDo

	ALL        field.Asterisk
	HeadlineID field.String
	Key        field.String
	Value      field.String
	Headline   propertyBelongsToHeadline

	fieldMap map[string]field.Expr
}

func (p property) Table(newTableName string) *property {
	p.propertyDo.UseTable(newTableName)
	return p.updateTableName(newTableName)
}

func (p property) As(alias string) *property {
	p.propertyDo.DO = *(p.propertyDo.As(alias).(*gen.DO))
	return p.updateTableName(alias)
}

func (p *property) updateTableName(table string) *property {
	p.ALL = field.NewAsterisk(table)
	p.HeadlineID = field.NewString(table, "headline_id")
	p.Key = field.NewString(table, "key")
	p.Value = field.NewString(table, "value")

	p.fillFieldMap()

	return p
}

func (p *property) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := p.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (p *property) fillFieldMap() {
	p.fieldMap = make(map[string]field.Expr, 4)
	p.fieldMap["headline_id"] = p.HeadlineID
	p.fieldMap["key"] = p.Key
	p.fieldMap["value"] = p.Value

}

func (p property) clone(db *gorm.DB) property {
	p.propertyDo.ReplaceConnPool(db.Statement.ConnPool)
	return p
}

func (p property) replaceDB(db *gorm.DB) property {
	p.propertyDo.ReplaceDB(db)
	return p
}

type propertyBelongsToHeadline struct {
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
	}
}

func (a propertyBelongsToHeadline) Where(conds ...field.Expr) *propertyBelongsToHeadline {
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

func (a propertyBelongsToHeadline) WithContext(ctx context.Context) *propertyBelongsToHeadline {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a propertyBelongsToHeadline) Session(session *gorm.Session) *propertyBelongsToHeadline {
	a.db = a.db.Session(session)
	return &a
}

func (a propertyBelongsToHeadline) Model(m *storage.Property) *propertyBelongsToHeadlineTx {
	return &propertyBelongsToHeadlineTx{a.db.Model(m).Association(a.Name())}
}

type propertyBelongsToHeadlineTx struct{ tx *gorm.Association }

func (a propertyBelongsToHeadlineTx) Find() (result *storage.Headline, err error) {
	return result, a.tx.Find(&result)
}

func (a propertyBelongsToHeadlineTx) Append(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a propertyBelongsToHeadlineTx) Replace(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a propertyBelongsToHeadlineTx) Delete(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a propertyBelongsToHeadlineTx) Clear() error {
	return a.tx.Clear()
}

func (a propertyBelongsToHeadlineTx) Count() int64 {
	return a.tx.Count()
}

type propertyDo struct{ gen.DO }

func (p propertyDo) Debug() *propertyDo {
	return p.withDO(p.DO.Debug())
}

func (p propertyDo) WithContext(ctx context.Context) *propertyDo {
	return p.withDO(p.DO.WithContext(ctx))
}

func (p propertyDo) ReadDB() *propertyDo {
	return p.Clauses(dbresolver.Read)
}

func (p propertyDo) WriteDB() *propertyDo {
	return p.Clauses(dbresolver.Write)
}

func (p propertyDo) Session(config *gorm.Session) *propertyDo {
	return p.withDO(p.DO.Session(config))
}

func (p propertyDo) Clauses(conds ...clause.Expression) *propertyDo {
	return p.withDO(p.DO.Clauses(conds...))
}

func (p propertyDo) Returning(value interface{}, columns ...string) *propertyDo {
	return p.withDO(p.DO.Returning(value, columns...))
}

func (p propertyDo) Not(conds ...gen.Condition) *propertyDo {
	return p.withDO(p.DO.Not(conds...))
}

func (p propertyDo) Or(conds ...gen.Condition) *propertyDo {
	return p.withDO(p.DO.Or(conds...))
}

func (p propertyDo) Select(conds ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Select(conds...))
}

func (p propertyDo) Where(conds ...gen.Condition) *propertyDo {
	return p.withDO(p.DO.Where(conds...))
}

func (p propertyDo) Order(conds ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Order(conds...))
}

func (p propertyDo) Distinct(cols ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Distinct(cols...))
}

func (p propertyDo) Omit(cols ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Omit(cols...))
}

func (p propertyDo) Join(table schema.Tabler, on ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Join(table, on...))
}

func (p propertyDo) LeftJoin(table schema.Tabler, on ...field.Expr) *propertyDo {
	return p.withDO(p.DO.LeftJoin(table, on...))
}

func (p propertyDo) RightJoin(table schema.Tabler, on ...field.Expr) *propertyDo {
	return p.withDO(p.DO.RightJoin(table, on...))
}

func (p propertyDo) Group(cols ...field.Expr) *propertyDo {
	return p.withDO(p.DO.Group(cols...))
}

func (p propertyDo) Having(conds ...gen.Condition) *propertyDo {
	return p.withDO(p.DO.Having(conds...))
}

func (p propertyDo) Limit(limit int) *propertyDo {
	return p.withDO(p.DO.Limit(limit))
}

func (p propertyDo) Offset(offset int) *propertyDo {
	return p.withDO(p.DO.Offset(offset))
}

func (p propertyDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *propertyDo {
	return p.withDO(p.DO.Scopes(funcs...))
}

func (p propertyDo) Unscoped() *propertyDo {
	return p.withDO(p.DO.Unscoped())
}

func (p propertyDo) Create(values ...*storage.Property) error {
	if len(values) == 0 {
		return nil
	}
	return p.DO.Create(values)
}

func (p propertyDo) CreateInBatches(values []*storage.Property, batchSize int) error {
	return p.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (p propertyDo) Save(values ...*storage.Property) error {
	if len(values) == 0 {
		return nil
	}
	return p.DO.Save(values)
}

func (p propertyDo) First() (*storage.Property, error) {
	if result, err := p.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Property), nil
	}
}

func (p propertyDo) Take() (*storage.Property, error) {
	if result, err := p.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Property), nil
	}
}

func (p propertyDo) Last() (*storage.Property, error) {
	if result, err := p.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Property), nil
	}
}

func (p propertyDo) Find() ([]*storage.Property, error) {
	result, err := p.DO.Find()
	return result.([]*storage.Property), err
}

func (p propertyDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Property, err error) {
	buf := make([]*storage.Property, 0, batchSize)
	err = p.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (p propertyDo) FindInBatches(result *[]*storage.Property, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return p.DO.FindInBatches(result, batchSize, fc)
}

func (p propertyDo) Attrs(attrs ...field.AssignExpr) *propertyDo {
	return p.withDO(p.DO.Attrs(attrs...))
}

func (p propertyDo) Assign(attrs ...field.AssignExpr) *propertyDo {
	return p.withDO(p.DO.Assign(attrs...))
}

func (p propertyDo) Joins(fields ...field.RelationField) *propertyDo {
	for _, _f := range fields {
		p = *p.withDO(p.DO.Joins(_f))
	}
	return &p
}

func (p propertyDo) Preload(fields ...field.RelationField) *propertyDo {
	for _, _f := range fields {
		p = *p.withDO(p.DO.Preload(_f))
	}
	return &p
}

func (p propertyDo) FirstOrInit() (*storage.Property, error) {
	if result, err := p.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Property), nil
	}
}

func (p propertyDo) FirstOrCreate() (*storage.Property, error) {
	if result, err := p.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Property), nil
	}
}

func (p propertyDo) FindByPage(offset int, limit int) (result []*storage.Property, count int64, err error) {
	result, err = p.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = p.Offset(-1).Limit(-1).Count()
	return
}

func (p propertyDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = p.Count()
	if err != nil {
		return
	}

	err = p.Offset(offset).Limit(limit).Scan(result)
	return
}

func (p propertyDo) Scan(result interface{}) (err error) {
	return p.DO.Scan(result)
}

func (p propertyDo) Delete(models ...*storage.Property) (result gen.ResultInfo, err error) {
	return p.DO.Delete(models)
}

func (p *propertyDo) withDO(do gen.Dao) *propertyDo {
	p.DO = *do.(*gen.DO)
	return p
}
