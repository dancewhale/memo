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

func newHeadline(db *gorm.DB, opts ...gen.DOOption) headline {
	_headline := headline{}

	_headline.headlineDo.UseDB(db, opts...)
	_headline.headlineDo.UseModel(&storage.Headline{})

	tableName := _headline.headlineDo.TableName()
	_headline.ALL = field.NewAsterisk(tableName)
	_headline.ID = field.NewString(tableName, "id")
	_headline.CreatedAt = field.NewTime(tableName, "created_at")
	_headline.UpdatedAt = field.NewTime(tableName, "updated_at")
	_headline.DeletedAt = field.NewField(tableName, "deleted_at")
	_headline.Weight = field.NewInt64(tableName, "weight")
	_headline.Source = field.NewString(tableName, "source")
	_headline.ScheduledType = field.NewString(tableName, "scheduled_type")
	_headline.Title = field.NewString(tableName, "title")
	_headline.Hash = field.NewString(tableName, "hash")
	_headline.Content = field.NewString(tableName, "content")
	_headline.ParentID = field.NewString(tableName, "parent_id")
	_headline.Level = field.NewInt(tableName, "level")
	_headline.Order_ = field.NewInt(tableName, "order")
	_headline.Status = field.NewString(tableName, "status")
	_headline.Scheduled = field.NewTime(tableName, "scheduled")
	_headline.Deadline = field.NewTime(tableName, "deadline")
	_headline.Closed = field.NewTime(tableName, "closed")
	_headline.Priority = field.NewString(tableName, "priority")
	_headline.FileID = field.NewString(tableName, "file_id")
	_headline.HeadlineID = field.NewString(tableName, "headline_id")
	_headline.AnnotationFileHash = field.NewString(tableName, "annotation_file_hash")
	_headline.Properties = headlineHasManyProperties{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Properties", "storage.Property"),
		Headline: struct {
			field.RelationField
			File struct {
				field.RelationField
				Headlines struct {
					field.RelationField
				}
			}
			Properties struct {
				field.RelationField
			}
			Children struct {
				field.RelationField
			}
			AnnotationChildren struct {
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
		}{
			RelationField: field.NewRelation("Properties.Headline", "storage.Headline"),
			File: struct {
				field.RelationField
				Headlines struct {
					field.RelationField
				}
			}{
				RelationField: field.NewRelation("Properties.Headline.File", "storage.File"),
				Headlines: struct {
					field.RelationField
				}{
					RelationField: field.NewRelation("Properties.Headline.File.Headlines", "storage.Headline"),
				},
			},
			Properties: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Properties.Headline.Properties", "storage.Property"),
			},
			Children: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Properties.Headline.Children", "storage.Headline"),
			},
			AnnotationChildren: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Properties.Headline.AnnotationChildren", "storage.Headline"),
			},
			LogBook: struct {
				field.RelationField
				Headline struct {
					field.RelationField
				}
			}{
				RelationField: field.NewRelation("Properties.Headline.LogBook", "storage.Clock"),
				Headline: struct {
					field.RelationField
				}{
					RelationField: field.NewRelation("Properties.Headline.LogBook.Headline", "storage.Headline"),
				},
			},
			Tags: struct {
				field.RelationField
				Headline struct {
					field.RelationField
				}
			}{
				RelationField: field.NewRelation("Properties.Headline.Tags", "storage.Tag"),
				Headline: struct {
					field.RelationField
				}{
					RelationField: field.NewRelation("Properties.Headline.Tags.Headline", "storage.Headline"),
				},
			},
		},
	}

	_headline.Children = headlineHasManyChildren{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Children", "storage.Headline"),
	}

	_headline.AnnotationChildren = headlineHasManyAnnotationChildren{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("AnnotationChildren", "storage.Headline"),
	}

	_headline.LogBook = headlineHasManyLogBook{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("LogBook", "storage.Clock"),
	}

	_headline.Tags = headlineHasManyTags{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Tags", "storage.Tag"),
	}

	_headline.File = headlineBelongsToFile{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("File", "storage.File"),
	}

	_headline.fillFieldMap()

	return _headline
}

type headline struct {
	headlineDo

	ALL                field.Asterisk
	ID                 field.String
	CreatedAt          field.Time
	UpdatedAt          field.Time
	DeletedAt          field.Field
	Weight             field.Int64
	Source             field.String
	ScheduledType      field.String
	Title              field.String
	Hash               field.String
	Content            field.String
	ParentID           field.String
	Level              field.Int
	Order_             field.Int
	Status             field.String
	Scheduled          field.Time
	Deadline           field.Time
	Closed             field.Time
	Priority           field.String
	FileID             field.String
	HeadlineID         field.String
	AnnotationFileHash field.String
	Properties         headlineHasManyProperties

	Children headlineHasManyChildren

	AnnotationChildren headlineHasManyAnnotationChildren

	LogBook headlineHasManyLogBook

	Tags headlineHasManyTags

	File headlineBelongsToFile

	fieldMap map[string]field.Expr
}

func (h headline) Table(newTableName string) *headline {
	h.headlineDo.UseTable(newTableName)
	return h.updateTableName(newTableName)
}

func (h headline) As(alias string) *headline {
	h.headlineDo.DO = *(h.headlineDo.As(alias).(*gen.DO))
	return h.updateTableName(alias)
}

func (h *headline) updateTableName(table string) *headline {
	h.ALL = field.NewAsterisk(table)
	h.ID = field.NewString(table, "id")
	h.CreatedAt = field.NewTime(table, "created_at")
	h.UpdatedAt = field.NewTime(table, "updated_at")
	h.DeletedAt = field.NewField(table, "deleted_at")
	h.Weight = field.NewInt64(table, "weight")
	h.Source = field.NewString(table, "source")
	h.ScheduledType = field.NewString(table, "scheduled_type")
	h.Title = field.NewString(table, "title")
	h.Hash = field.NewString(table, "hash")
	h.Content = field.NewString(table, "content")
	h.ParentID = field.NewString(table, "parent_id")
	h.Level = field.NewInt(table, "level")
	h.Order_ = field.NewInt(table, "order")
	h.Status = field.NewString(table, "status")
	h.Scheduled = field.NewTime(table, "scheduled")
	h.Deadline = field.NewTime(table, "deadline")
	h.Closed = field.NewTime(table, "closed")
	h.Priority = field.NewString(table, "priority")
	h.FileID = field.NewString(table, "file_id")
	h.HeadlineID = field.NewString(table, "headline_id")
	h.AnnotationFileHash = field.NewString(table, "annotation_file_hash")

	h.fillFieldMap()

	return h
}

func (h *headline) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := h.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (h *headline) fillFieldMap() {
	h.fieldMap = make(map[string]field.Expr, 27)
	h.fieldMap["id"] = h.ID
	h.fieldMap["created_at"] = h.CreatedAt
	h.fieldMap["updated_at"] = h.UpdatedAt
	h.fieldMap["deleted_at"] = h.DeletedAt
	h.fieldMap["weight"] = h.Weight
	h.fieldMap["source"] = h.Source
	h.fieldMap["scheduled_type"] = h.ScheduledType
	h.fieldMap["title"] = h.Title
	h.fieldMap["hash"] = h.Hash
	h.fieldMap["content"] = h.Content
	h.fieldMap["parent_id"] = h.ParentID
	h.fieldMap["level"] = h.Level
	h.fieldMap["order"] = h.Order_
	h.fieldMap["status"] = h.Status
	h.fieldMap["scheduled"] = h.Scheduled
	h.fieldMap["deadline"] = h.Deadline
	h.fieldMap["closed"] = h.Closed
	h.fieldMap["priority"] = h.Priority
	h.fieldMap["file_id"] = h.FileID
	h.fieldMap["headline_id"] = h.HeadlineID
	h.fieldMap["annotation_file_hash"] = h.AnnotationFileHash

}

func (h headline) clone(db *gorm.DB) headline {
	h.headlineDo.ReplaceConnPool(db.Statement.ConnPool)
	return h
}

func (h headline) replaceDB(db *gorm.DB) headline {
	h.headlineDo.ReplaceDB(db)
	return h
}

type headlineHasManyProperties struct {
	db *gorm.DB

	field.RelationField

	Headline struct {
		field.RelationField
		File struct {
			field.RelationField
			Headlines struct {
				field.RelationField
			}
		}
		Properties struct {
			field.RelationField
		}
		Children struct {
			field.RelationField
		}
		AnnotationChildren struct {
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
}

func (a headlineHasManyProperties) Where(conds ...field.Expr) *headlineHasManyProperties {
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

func (a headlineHasManyProperties) WithContext(ctx context.Context) *headlineHasManyProperties {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyProperties) Session(session *gorm.Session) *headlineHasManyProperties {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyProperties) Model(m *storage.Headline) *headlineHasManyPropertiesTx {
	return &headlineHasManyPropertiesTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyPropertiesTx struct{ tx *gorm.Association }

func (a headlineHasManyPropertiesTx) Find() (result []*storage.Property, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyPropertiesTx) Append(values ...*storage.Property) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyPropertiesTx) Replace(values ...*storage.Property) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyPropertiesTx) Delete(values ...*storage.Property) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyPropertiesTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyPropertiesTx) Count() int64 {
	return a.tx.Count()
}

type headlineHasManyChildren struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasManyChildren) Where(conds ...field.Expr) *headlineHasManyChildren {
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

func (a headlineHasManyChildren) WithContext(ctx context.Context) *headlineHasManyChildren {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyChildren) Session(session *gorm.Session) *headlineHasManyChildren {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyChildren) Model(m *storage.Headline) *headlineHasManyChildrenTx {
	return &headlineHasManyChildrenTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyChildrenTx struct{ tx *gorm.Association }

func (a headlineHasManyChildrenTx) Find() (result []*storage.Headline, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyChildrenTx) Append(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyChildrenTx) Replace(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyChildrenTx) Delete(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyChildrenTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyChildrenTx) Count() int64 {
	return a.tx.Count()
}

type headlineHasManyAnnotationChildren struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasManyAnnotationChildren) Where(conds ...field.Expr) *headlineHasManyAnnotationChildren {
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

func (a headlineHasManyAnnotationChildren) WithContext(ctx context.Context) *headlineHasManyAnnotationChildren {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyAnnotationChildren) Session(session *gorm.Session) *headlineHasManyAnnotationChildren {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyAnnotationChildren) Model(m *storage.Headline) *headlineHasManyAnnotationChildrenTx {
	return &headlineHasManyAnnotationChildrenTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyAnnotationChildrenTx struct{ tx *gorm.Association }

func (a headlineHasManyAnnotationChildrenTx) Find() (result []*storage.Headline, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyAnnotationChildrenTx) Append(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyAnnotationChildrenTx) Replace(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyAnnotationChildrenTx) Delete(values ...*storage.Headline) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyAnnotationChildrenTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyAnnotationChildrenTx) Count() int64 {
	return a.tx.Count()
}

type headlineHasManyLogBook struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasManyLogBook) Where(conds ...field.Expr) *headlineHasManyLogBook {
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

func (a headlineHasManyLogBook) WithContext(ctx context.Context) *headlineHasManyLogBook {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyLogBook) Session(session *gorm.Session) *headlineHasManyLogBook {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyLogBook) Model(m *storage.Headline) *headlineHasManyLogBookTx {
	return &headlineHasManyLogBookTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyLogBookTx struct{ tx *gorm.Association }

func (a headlineHasManyLogBookTx) Find() (result []*storage.Clock, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyLogBookTx) Append(values ...*storage.Clock) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyLogBookTx) Replace(values ...*storage.Clock) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyLogBookTx) Delete(values ...*storage.Clock) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyLogBookTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyLogBookTx) Count() int64 {
	return a.tx.Count()
}

type headlineHasManyTags struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasManyTags) Where(conds ...field.Expr) *headlineHasManyTags {
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

func (a headlineHasManyTags) WithContext(ctx context.Context) *headlineHasManyTags {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyTags) Session(session *gorm.Session) *headlineHasManyTags {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyTags) Model(m *storage.Headline) *headlineHasManyTagsTx {
	return &headlineHasManyTagsTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyTagsTx struct{ tx *gorm.Association }

func (a headlineHasManyTagsTx) Find() (result []*storage.Tag, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyTagsTx) Append(values ...*storage.Tag) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyTagsTx) Replace(values ...*storage.Tag) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyTagsTx) Delete(values ...*storage.Tag) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyTagsTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyTagsTx) Count() int64 {
	return a.tx.Count()
}

type headlineBelongsToFile struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineBelongsToFile) Where(conds ...field.Expr) *headlineBelongsToFile {
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

func (a headlineBelongsToFile) WithContext(ctx context.Context) *headlineBelongsToFile {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineBelongsToFile) Session(session *gorm.Session) *headlineBelongsToFile {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineBelongsToFile) Model(m *storage.Headline) *headlineBelongsToFileTx {
	return &headlineBelongsToFileTx{a.db.Model(m).Association(a.Name())}
}

type headlineBelongsToFileTx struct{ tx *gorm.Association }

func (a headlineBelongsToFileTx) Find() (result *storage.File, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineBelongsToFileTx) Append(values ...*storage.File) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineBelongsToFileTx) Replace(values ...*storage.File) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineBelongsToFileTx) Delete(values ...*storage.File) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineBelongsToFileTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineBelongsToFileTx) Count() int64 {
	return a.tx.Count()
}

type headlineDo struct{ gen.DO }

type IHeadlineDo interface {
	gen.SubQuery
	Debug() IHeadlineDo
	WithContext(ctx context.Context) IHeadlineDo
	WithResult(fc func(tx gen.Dao)) gen.ResultInfo
	ReplaceDB(db *gorm.DB)
	ReadDB() IHeadlineDo
	WriteDB() IHeadlineDo
	As(alias string) gen.Dao
	Session(config *gorm.Session) IHeadlineDo
	Columns(cols ...field.Expr) gen.Columns
	Clauses(conds ...clause.Expression) IHeadlineDo
	Not(conds ...gen.Condition) IHeadlineDo
	Or(conds ...gen.Condition) IHeadlineDo
	Select(conds ...field.Expr) IHeadlineDo
	Where(conds ...gen.Condition) IHeadlineDo
	Order(conds ...field.Expr) IHeadlineDo
	Distinct(cols ...field.Expr) IHeadlineDo
	Omit(cols ...field.Expr) IHeadlineDo
	Join(table schema.Tabler, on ...field.Expr) IHeadlineDo
	LeftJoin(table schema.Tabler, on ...field.Expr) IHeadlineDo
	RightJoin(table schema.Tabler, on ...field.Expr) IHeadlineDo
	Group(cols ...field.Expr) IHeadlineDo
	Having(conds ...gen.Condition) IHeadlineDo
	Limit(limit int) IHeadlineDo
	Offset(offset int) IHeadlineDo
	Count() (count int64, err error)
	Scopes(funcs ...func(gen.Dao) gen.Dao) IHeadlineDo
	Unscoped() IHeadlineDo
	Create(values ...*storage.Headline) error
	CreateInBatches(values []*storage.Headline, batchSize int) error
	Save(values ...*storage.Headline) error
	First() (*storage.Headline, error)
	Take() (*storage.Headline, error)
	Last() (*storage.Headline, error)
	Find() ([]*storage.Headline, error)
	FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Headline, err error)
	FindInBatches(result *[]*storage.Headline, batchSize int, fc func(tx gen.Dao, batch int) error) error
	Pluck(column field.Expr, dest interface{}) error
	Delete(...*storage.Headline) (info gen.ResultInfo, err error)
	Update(column field.Expr, value interface{}) (info gen.ResultInfo, err error)
	UpdateSimple(columns ...field.AssignExpr) (info gen.ResultInfo, err error)
	Updates(value interface{}) (info gen.ResultInfo, err error)
	UpdateColumn(column field.Expr, value interface{}) (info gen.ResultInfo, err error)
	UpdateColumnSimple(columns ...field.AssignExpr) (info gen.ResultInfo, err error)
	UpdateColumns(value interface{}) (info gen.ResultInfo, err error)
	UpdateFrom(q gen.SubQuery) gen.Dao
	Attrs(attrs ...field.AssignExpr) IHeadlineDo
	Assign(attrs ...field.AssignExpr) IHeadlineDo
	Joins(fields ...field.RelationField) IHeadlineDo
	Preload(fields ...field.RelationField) IHeadlineDo
	FirstOrInit() (*storage.Headline, error)
	FirstOrCreate() (*storage.Headline, error)
	FindByPage(offset int, limit int) (result []*storage.Headline, count int64, err error)
	ScanByPage(result interface{}, offset int, limit int) (count int64, err error)
	Scan(result interface{}) (err error)
	Returning(value interface{}, columns ...string) IHeadlineDo
	UnderlyingDB() *gorm.DB
	schema.Tabler
}

func (h headlineDo) Debug() IHeadlineDo {
	return h.withDO(h.DO.Debug())
}

func (h headlineDo) WithContext(ctx context.Context) IHeadlineDo {
	return h.withDO(h.DO.WithContext(ctx))
}

func (h headlineDo) ReadDB() IHeadlineDo {
	return h.Clauses(dbresolver.Read)
}

func (h headlineDo) WriteDB() IHeadlineDo {
	return h.Clauses(dbresolver.Write)
}

func (h headlineDo) Session(config *gorm.Session) IHeadlineDo {
	return h.withDO(h.DO.Session(config))
}

func (h headlineDo) Clauses(conds ...clause.Expression) IHeadlineDo {
	return h.withDO(h.DO.Clauses(conds...))
}

func (h headlineDo) Returning(value interface{}, columns ...string) IHeadlineDo {
	return h.withDO(h.DO.Returning(value, columns...))
}

func (h headlineDo) Not(conds ...gen.Condition) IHeadlineDo {
	return h.withDO(h.DO.Not(conds...))
}

func (h headlineDo) Or(conds ...gen.Condition) IHeadlineDo {
	return h.withDO(h.DO.Or(conds...))
}

func (h headlineDo) Select(conds ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Select(conds...))
}

func (h headlineDo) Where(conds ...gen.Condition) IHeadlineDo {
	return h.withDO(h.DO.Where(conds...))
}

func (h headlineDo) Order(conds ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Order(conds...))
}

func (h headlineDo) Distinct(cols ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Distinct(cols...))
}

func (h headlineDo) Omit(cols ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Omit(cols...))
}

func (h headlineDo) Join(table schema.Tabler, on ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Join(table, on...))
}

func (h headlineDo) LeftJoin(table schema.Tabler, on ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.LeftJoin(table, on...))
}

func (h headlineDo) RightJoin(table schema.Tabler, on ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.RightJoin(table, on...))
}

func (h headlineDo) Group(cols ...field.Expr) IHeadlineDo {
	return h.withDO(h.DO.Group(cols...))
}

func (h headlineDo) Having(conds ...gen.Condition) IHeadlineDo {
	return h.withDO(h.DO.Having(conds...))
}

func (h headlineDo) Limit(limit int) IHeadlineDo {
	return h.withDO(h.DO.Limit(limit))
}

func (h headlineDo) Offset(offset int) IHeadlineDo {
	return h.withDO(h.DO.Offset(offset))
}

func (h headlineDo) Scopes(funcs ...func(gen.Dao) gen.Dao) IHeadlineDo {
	return h.withDO(h.DO.Scopes(funcs...))
}

func (h headlineDo) Unscoped() IHeadlineDo {
	return h.withDO(h.DO.Unscoped())
}

func (h headlineDo) Create(values ...*storage.Headline) error {
	if len(values) == 0 {
		return nil
	}
	return h.DO.Create(values)
}

func (h headlineDo) CreateInBatches(values []*storage.Headline, batchSize int) error {
	return h.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (h headlineDo) Save(values ...*storage.Headline) error {
	if len(values) == 0 {
		return nil
	}
	return h.DO.Save(values)
}

func (h headlineDo) First() (*storage.Headline, error) {
	if result, err := h.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Headline), nil
	}
}

func (h headlineDo) Take() (*storage.Headline, error) {
	if result, err := h.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Headline), nil
	}
}

func (h headlineDo) Last() (*storage.Headline, error) {
	if result, err := h.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Headline), nil
	}
}

func (h headlineDo) Find() ([]*storage.Headline, error) {
	result, err := h.DO.Find()
	return result.([]*storage.Headline), err
}

func (h headlineDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Headline, err error) {
	buf := make([]*storage.Headline, 0, batchSize)
	err = h.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (h headlineDo) FindInBatches(result *[]*storage.Headline, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return h.DO.FindInBatches(result, batchSize, fc)
}

func (h headlineDo) Attrs(attrs ...field.AssignExpr) IHeadlineDo {
	return h.withDO(h.DO.Attrs(attrs...))
}

func (h headlineDo) Assign(attrs ...field.AssignExpr) IHeadlineDo {
	return h.withDO(h.DO.Assign(attrs...))
}

func (h headlineDo) Joins(fields ...field.RelationField) IHeadlineDo {
	for _, _f := range fields {
		h = *h.withDO(h.DO.Joins(_f))
	}
	return &h
}

func (h headlineDo) Preload(fields ...field.RelationField) IHeadlineDo {
	for _, _f := range fields {
		h = *h.withDO(h.DO.Preload(_f))
	}
	return &h
}

func (h headlineDo) FirstOrInit() (*storage.Headline, error) {
	if result, err := h.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Headline), nil
	}
}

func (h headlineDo) FirstOrCreate() (*storage.Headline, error) {
	if result, err := h.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Headline), nil
	}
}

func (h headlineDo) FindByPage(offset int, limit int) (result []*storage.Headline, count int64, err error) {
	result, err = h.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = h.Offset(-1).Limit(-1).Count()
	return
}

func (h headlineDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = h.Count()
	if err != nil {
		return
	}

	err = h.Offset(offset).Limit(limit).Scan(result)
	return
}

func (h headlineDo) Scan(result interface{}) (err error) {
	return h.DO.Scan(result)
}

func (h headlineDo) Delete(models ...*storage.Headline) (result gen.ResultInfo, err error) {
	return h.DO.Delete(models)
}

func (h *headlineDo) withDO(do gen.Dao) *headlineDo {
	h.DO = *do.(*gen.DO)
	return h
}
