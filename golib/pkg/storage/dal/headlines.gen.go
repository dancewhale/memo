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
	_headline.Title = field.NewString(tableName, "title")
	_headline.Hash = field.NewString(tableName, "hash")
	_headline.Content = field.NewString(tableName, "content")
	_headline.Weight = field.NewInt64(tableName, "weight")
	_headline.Source = field.NewString(tableName, "source")
	_headline.ScheduledType = field.NewString(tableName, "scheduled_type")
	_headline.ParentID = field.NewString(tableName, "parent_id")
	_headline.Level = field.NewInt(tableName, "level")
	_headline.Order_ = field.NewInt(tableName, "order")
	_headline.Status = field.NewString(tableName, "status")
	_headline.Scheduled = field.NewTime(tableName, "scheduled")
	_headline.Deadline = field.NewTime(tableName, "deadline")
	_headline.Closed = field.NewTime(tableName, "closed")
	_headline.Priority = field.NewString(tableName, "priority")
	_headline.FileID = field.NewString(tableName, "file_id")
	_headline.Fsrs = headlineHasOneFsrs{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Fsrs", "storage.FsrsInfo"),
	}

	_headline.Children = headlineHasManyChildren{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Children", "storage.Headline"),
		File: struct {
			field.RelationField
			Headlines struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Children.File", "storage.File"),
			Headlines: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Children.File.Headlines", "storage.Headline"),
			},
		},
		Fsrs: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Children.Fsrs", "storage.FsrsInfo"),
		},
		Children: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Children.Children", "storage.Headline"),
		},
		ReviewLogs: struct {
			field.RelationField
		}{
			RelationField: field.NewRelation("Children.ReviewLogs", "storage.ReviewLog"),
		},
		LogBook: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Children.LogBook", "storage.Clock"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Children.LogBook.Headline", "storage.Headline"),
			},
		},
		Locations: struct {
			field.RelationField
			Headline struct {
				field.RelationField
			}
		}{
			RelationField: field.NewRelation("Children.Locations", "storage.Location"),
			Headline: struct {
				field.RelationField
			}{
				RelationField: field.NewRelation("Children.Locations.Headline", "storage.Headline"),
			},
		},
	}

	_headline.ReviewLogs = headlineHasManyReviewLogs{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("ReviewLogs", "storage.ReviewLog"),
	}

	_headline.LogBook = headlineHasManyLogBook{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("LogBook", "storage.Clock"),
	}

	_headline.File = headlineBelongsToFile{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("File", "storage.File"),
	}

	_headline.Locations = headlineManyToManyLocations{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Locations", "storage.Location"),
	}

	_headline.fillFieldMap()

	return _headline
}

type headline struct {
	headlineDo

	ALL           field.Asterisk
	ID            field.String
	CreatedAt     field.Time
	UpdatedAt     field.Time
	DeletedAt     field.Field
	Title         field.String
	Hash          field.String
	Content       field.String
	Weight        field.Int64
	Source        field.String
	ScheduledType field.String
	ParentID      field.String
	Level         field.Int
	Order_        field.Int
	Status        field.String
	Scheduled     field.Time
	Deadline      field.Time
	Closed        field.Time
	Priority      field.String
	FileID        field.String
	Fsrs          headlineHasOneFsrs

	Children headlineHasManyChildren

	ReviewLogs headlineHasManyReviewLogs

	LogBook headlineHasManyLogBook

	File headlineBelongsToFile

	Locations headlineManyToManyLocations

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
	h.Title = field.NewString(table, "title")
	h.Hash = field.NewString(table, "hash")
	h.Content = field.NewString(table, "content")
	h.Weight = field.NewInt64(table, "weight")
	h.Source = field.NewString(table, "source")
	h.ScheduledType = field.NewString(table, "scheduled_type")
	h.ParentID = field.NewString(table, "parent_id")
	h.Level = field.NewInt(table, "level")
	h.Order_ = field.NewInt(table, "order")
	h.Status = field.NewString(table, "status")
	h.Scheduled = field.NewTime(table, "scheduled")
	h.Deadline = field.NewTime(table, "deadline")
	h.Closed = field.NewTime(table, "closed")
	h.Priority = field.NewString(table, "priority")
	h.FileID = field.NewString(table, "file_id")

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
	h.fieldMap = make(map[string]field.Expr, 25)
	h.fieldMap["id"] = h.ID
	h.fieldMap["created_at"] = h.CreatedAt
	h.fieldMap["updated_at"] = h.UpdatedAt
	h.fieldMap["deleted_at"] = h.DeletedAt
	h.fieldMap["title"] = h.Title
	h.fieldMap["hash"] = h.Hash
	h.fieldMap["content"] = h.Content
	h.fieldMap["weight"] = h.Weight
	h.fieldMap["source"] = h.Source
	h.fieldMap["scheduled_type"] = h.ScheduledType
	h.fieldMap["parent_id"] = h.ParentID
	h.fieldMap["level"] = h.Level
	h.fieldMap["order"] = h.Order_
	h.fieldMap["status"] = h.Status
	h.fieldMap["scheduled"] = h.Scheduled
	h.fieldMap["deadline"] = h.Deadline
	h.fieldMap["closed"] = h.Closed
	h.fieldMap["priority"] = h.Priority
	h.fieldMap["file_id"] = h.FileID

}

func (h headline) clone(db *gorm.DB) headline {
	h.headlineDo.ReplaceConnPool(db.Statement.ConnPool)
	return h
}

func (h headline) replaceDB(db *gorm.DB) headline {
	h.headlineDo.ReplaceDB(db)
	return h
}

type headlineHasOneFsrs struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasOneFsrs) Where(conds ...field.Expr) *headlineHasOneFsrs {
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

func (a headlineHasOneFsrs) WithContext(ctx context.Context) *headlineHasOneFsrs {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasOneFsrs) Session(session *gorm.Session) *headlineHasOneFsrs {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasOneFsrs) Model(m *storage.Headline) *headlineHasOneFsrsTx {
	return &headlineHasOneFsrsTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasOneFsrsTx struct{ tx *gorm.Association }

func (a headlineHasOneFsrsTx) Find() (result *storage.FsrsInfo, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasOneFsrsTx) Append(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasOneFsrsTx) Replace(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasOneFsrsTx) Delete(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasOneFsrsTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasOneFsrsTx) Count() int64 {
	return a.tx.Count()
}

type headlineHasManyChildren struct {
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

type headlineHasManyReviewLogs struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineHasManyReviewLogs) Where(conds ...field.Expr) *headlineHasManyReviewLogs {
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

func (a headlineHasManyReviewLogs) WithContext(ctx context.Context) *headlineHasManyReviewLogs {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineHasManyReviewLogs) Session(session *gorm.Session) *headlineHasManyReviewLogs {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineHasManyReviewLogs) Model(m *storage.Headline) *headlineHasManyReviewLogsTx {
	return &headlineHasManyReviewLogsTx{a.db.Model(m).Association(a.Name())}
}

type headlineHasManyReviewLogsTx struct{ tx *gorm.Association }

func (a headlineHasManyReviewLogsTx) Find() (result []*storage.ReviewLog, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineHasManyReviewLogsTx) Append(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineHasManyReviewLogsTx) Replace(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineHasManyReviewLogsTx) Delete(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineHasManyReviewLogsTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineHasManyReviewLogsTx) Count() int64 {
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

type headlineManyToManyLocations struct {
	db *gorm.DB

	field.RelationField
}

func (a headlineManyToManyLocations) Where(conds ...field.Expr) *headlineManyToManyLocations {
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

func (a headlineManyToManyLocations) WithContext(ctx context.Context) *headlineManyToManyLocations {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a headlineManyToManyLocations) Session(session *gorm.Session) *headlineManyToManyLocations {
	a.db = a.db.Session(session)
	return &a
}

func (a headlineManyToManyLocations) Model(m *storage.Headline) *headlineManyToManyLocationsTx {
	return &headlineManyToManyLocationsTx{a.db.Model(m).Association(a.Name())}
}

type headlineManyToManyLocationsTx struct{ tx *gorm.Association }

func (a headlineManyToManyLocationsTx) Find() (result []*storage.Location, err error) {
	return result, a.tx.Find(&result)
}

func (a headlineManyToManyLocationsTx) Append(values ...*storage.Location) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a headlineManyToManyLocationsTx) Replace(values ...*storage.Location) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a headlineManyToManyLocationsTx) Delete(values ...*storage.Location) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a headlineManyToManyLocationsTx) Clear() error {
	return a.tx.Clear()
}

func (a headlineManyToManyLocationsTx) Count() int64 {
	return a.tx.Count()
}

type headlineDo struct{ gen.DO }

func (h headlineDo) Debug() *headlineDo {
	return h.withDO(h.DO.Debug())
}

func (h headlineDo) WithContext(ctx context.Context) *headlineDo {
	return h.withDO(h.DO.WithContext(ctx))
}

func (h headlineDo) ReadDB() *headlineDo {
	return h.Clauses(dbresolver.Read)
}

func (h headlineDo) WriteDB() *headlineDo {
	return h.Clauses(dbresolver.Write)
}

func (h headlineDo) Session(config *gorm.Session) *headlineDo {
	return h.withDO(h.DO.Session(config))
}

func (h headlineDo) Clauses(conds ...clause.Expression) *headlineDo {
	return h.withDO(h.DO.Clauses(conds...))
}

func (h headlineDo) Returning(value interface{}, columns ...string) *headlineDo {
	return h.withDO(h.DO.Returning(value, columns...))
}

func (h headlineDo) Not(conds ...gen.Condition) *headlineDo {
	return h.withDO(h.DO.Not(conds...))
}

func (h headlineDo) Or(conds ...gen.Condition) *headlineDo {
	return h.withDO(h.DO.Or(conds...))
}

func (h headlineDo) Select(conds ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Select(conds...))
}

func (h headlineDo) Where(conds ...gen.Condition) *headlineDo {
	return h.withDO(h.DO.Where(conds...))
}

func (h headlineDo) Order(conds ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Order(conds...))
}

func (h headlineDo) Distinct(cols ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Distinct(cols...))
}

func (h headlineDo) Omit(cols ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Omit(cols...))
}

func (h headlineDo) Join(table schema.Tabler, on ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Join(table, on...))
}

func (h headlineDo) LeftJoin(table schema.Tabler, on ...field.Expr) *headlineDo {
	return h.withDO(h.DO.LeftJoin(table, on...))
}

func (h headlineDo) RightJoin(table schema.Tabler, on ...field.Expr) *headlineDo {
	return h.withDO(h.DO.RightJoin(table, on...))
}

func (h headlineDo) Group(cols ...field.Expr) *headlineDo {
	return h.withDO(h.DO.Group(cols...))
}

func (h headlineDo) Having(conds ...gen.Condition) *headlineDo {
	return h.withDO(h.DO.Having(conds...))
}

func (h headlineDo) Limit(limit int) *headlineDo {
	return h.withDO(h.DO.Limit(limit))
}

func (h headlineDo) Offset(offset int) *headlineDo {
	return h.withDO(h.DO.Offset(offset))
}

func (h headlineDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *headlineDo {
	return h.withDO(h.DO.Scopes(funcs...))
}

func (h headlineDo) Unscoped() *headlineDo {
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

func (h headlineDo) Attrs(attrs ...field.AssignExpr) *headlineDo {
	return h.withDO(h.DO.Attrs(attrs...))
}

func (h headlineDo) Assign(attrs ...field.AssignExpr) *headlineDo {
	return h.withDO(h.DO.Assign(attrs...))
}

func (h headlineDo) Joins(fields ...field.RelationField) *headlineDo {
	for _, _f := range fields {
		h = *h.withDO(h.DO.Joins(_f))
	}
	return &h
}

func (h headlineDo) Preload(fields ...field.RelationField) *headlineDo {
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
