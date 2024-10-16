// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.

package dal

import (
	"context"
	"strings"

	"gorm.io/gorm"
	"gorm.io/gorm/clause"
	"gorm.io/gorm/schema"

	"gorm.io/gen"
	"gorm.io/gen/field"

	"gorm.io/plugin/dbresolver"

	"memo/pkg/storage"
)

func newNote(db *gorm.DB, opts ...gen.DOOption) note {
	_note := note{}

	_note.noteDo.UseDB(db, opts...)
	_note.noteDo.UseModel(&storage.Note{})

	tableName := _note.noteDo.TableName()
	_note.ALL = field.NewAsterisk(tableName)
	_note.Orgid = field.NewString(tableName, "orgid")
	_note.CreatedAt = field.NewTime(tableName, "created_at")
	_note.UpdatedAt = field.NewTime(tableName, "updated_at")
	_note.Content = field.NewString(tableName, "content")
	_note.Type = field.NewString(tableName, "type")
	_note.Hash = field.NewString(tableName, "hash")
	_note.Fsrs = noteHasOneFsrs{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("Fsrs", "storage.FsrsInfo"),
	}

	_note.ReviewLogs = noteHasManyReviewLogs{
		db: db.Session(&gorm.Session{}),

		RelationField: field.NewRelation("ReviewLogs", "storage.ReviewLog"),
	}

	_note.fillFieldMap()

	return _note
}

type note struct {
	noteDo

	ALL       field.Asterisk
	Orgid     field.String
	CreatedAt field.Time
	UpdatedAt field.Time
	Content   field.String
	Type      field.String
	Hash      field.String
	Fsrs      noteHasOneFsrs

	ReviewLogs noteHasManyReviewLogs

	fieldMap map[string]field.Expr
}

func (n note) Table(newTableName string) *note {
	n.noteDo.UseTable(newTableName)
	return n.updateTableName(newTableName)
}

func (n note) As(alias string) *note {
	n.noteDo.DO = *(n.noteDo.As(alias).(*gen.DO))
	return n.updateTableName(alias)
}

func (n *note) updateTableName(table string) *note {
	n.ALL = field.NewAsterisk(table)
	n.Orgid = field.NewString(table, "orgid")
	n.CreatedAt = field.NewTime(table, "created_at")
	n.UpdatedAt = field.NewTime(table, "updated_at")
	n.Content = field.NewString(table, "content")
	n.Type = field.NewString(table, "type")
	n.Hash = field.NewString(table, "hash")

	n.fillFieldMap()

	return n
}

func (n *note) GetFieldByName(fieldName string) (field.OrderExpr, bool) {
	_f, ok := n.fieldMap[fieldName]
	if !ok || _f == nil {
		return nil, false
	}
	_oe, ok := _f.(field.OrderExpr)
	return _oe, ok
}

func (n *note) fillFieldMap() {
	n.fieldMap = make(map[string]field.Expr, 8)
	n.fieldMap["orgid"] = n.Orgid
	n.fieldMap["created_at"] = n.CreatedAt
	n.fieldMap["updated_at"] = n.UpdatedAt
	n.fieldMap["content"] = n.Content
	n.fieldMap["type"] = n.Type
	n.fieldMap["hash"] = n.Hash

}

func (n note) clone(db *gorm.DB) note {
	n.noteDo.ReplaceConnPool(db.Statement.ConnPool)
	return n
}

func (n note) replaceDB(db *gorm.DB) note {
	n.noteDo.ReplaceDB(db)
	return n
}

type noteHasOneFsrs struct {
	db *gorm.DB

	field.RelationField
}

func (a noteHasOneFsrs) Where(conds ...field.Expr) *noteHasOneFsrs {
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

func (a noteHasOneFsrs) WithContext(ctx context.Context) *noteHasOneFsrs {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a noteHasOneFsrs) Session(session *gorm.Session) *noteHasOneFsrs {
	a.db = a.db.Session(session)
	return &a
}

func (a noteHasOneFsrs) Model(m *storage.Note) *noteHasOneFsrsTx {
	return &noteHasOneFsrsTx{a.db.Model(m).Association(a.Name())}
}

type noteHasOneFsrsTx struct{ tx *gorm.Association }

func (a noteHasOneFsrsTx) Find() (result *storage.FsrsInfo, err error) {
	return result, a.tx.Find(&result)
}

func (a noteHasOneFsrsTx) Append(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a noteHasOneFsrsTx) Replace(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a noteHasOneFsrsTx) Delete(values ...*storage.FsrsInfo) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a noteHasOneFsrsTx) Clear() error {
	return a.tx.Clear()
}

func (a noteHasOneFsrsTx) Count() int64 {
	return a.tx.Count()
}

type noteHasManyReviewLogs struct {
	db *gorm.DB

	field.RelationField
}

func (a noteHasManyReviewLogs) Where(conds ...field.Expr) *noteHasManyReviewLogs {
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

func (a noteHasManyReviewLogs) WithContext(ctx context.Context) *noteHasManyReviewLogs {
	a.db = a.db.WithContext(ctx)
	return &a
}

func (a noteHasManyReviewLogs) Session(session *gorm.Session) *noteHasManyReviewLogs {
	a.db = a.db.Session(session)
	return &a
}

func (a noteHasManyReviewLogs) Model(m *storage.Note) *noteHasManyReviewLogsTx {
	return &noteHasManyReviewLogsTx{a.db.Model(m).Association(a.Name())}
}

type noteHasManyReviewLogsTx struct{ tx *gorm.Association }

func (a noteHasManyReviewLogsTx) Find() (result []*storage.ReviewLog, err error) {
	return result, a.tx.Find(&result)
}

func (a noteHasManyReviewLogsTx) Append(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Append(targetValues...)
}

func (a noteHasManyReviewLogsTx) Replace(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Replace(targetValues...)
}

func (a noteHasManyReviewLogsTx) Delete(values ...*storage.ReviewLog) (err error) {
	targetValues := make([]interface{}, len(values))
	for i, v := range values {
		targetValues[i] = v
	}
	return a.tx.Delete(targetValues...)
}

func (a noteHasManyReviewLogsTx) Clear() error {
	return a.tx.Clear()
}

func (a noteHasManyReviewLogsTx) Count() int64 {
	return a.tx.Count()
}

type noteDo struct{ gen.DO }

// sql(select * from notes inner join fsrs_infos on notes.orgid=fsrs_infos.note_orgid where fsrs_infos.due<@today
// order by fsrs_infos.due)
func (n noteDo) GetNoteOrderByDueTime(today string) (result []*storage.Note, err error) {
	var params []interface{}

	var generateSQL strings.Builder
	params = append(params, today)
	generateSQL.WriteString("select * from notes inner join fsrs_infos on notes.orgid=fsrs_infos.note_orgid where fsrs_infos.due<? order by fsrs_infos.due ")

	var executeSQL *gorm.DB
	executeSQL = n.UnderlyingDB().Raw(generateSQL.String(), params...).Find(&result) // ignore_security_alert
	err = executeSQL.Error

	return
}

// sql(select * from notes inner join fsrs_infos on notes.orgid=fsrs_infos.note_orgid where fsrs_infos.due<@today)
func (n noteDo) FindDueCard(today string) (result []*storage.Note, err error) {
	var params []interface{}

	var generateSQL strings.Builder
	params = append(params, today)
	generateSQL.WriteString("select * from notes inner join fsrs_infos on notes.orgid=fsrs_infos.note_orgid where fsrs_infos.due<? ")

	var executeSQL *gorm.DB
	executeSQL = n.UnderlyingDB().Raw(generateSQL.String(), params...).Find(&result) // ignore_security_alert
	err = executeSQL.Error

	return
}

func (n noteDo) Debug() *noteDo {
	return n.withDO(n.DO.Debug())
}

func (n noteDo) WithContext(ctx context.Context) *noteDo {
	return n.withDO(n.DO.WithContext(ctx))
}

func (n noteDo) ReadDB() *noteDo {
	return n.Clauses(dbresolver.Read)
}

func (n noteDo) WriteDB() *noteDo {
	return n.Clauses(dbresolver.Write)
}

func (n noteDo) Session(config *gorm.Session) *noteDo {
	return n.withDO(n.DO.Session(config))
}

func (n noteDo) Clauses(conds ...clause.Expression) *noteDo {
	return n.withDO(n.DO.Clauses(conds...))
}

func (n noteDo) Returning(value interface{}, columns ...string) *noteDo {
	return n.withDO(n.DO.Returning(value, columns...))
}

func (n noteDo) Not(conds ...gen.Condition) *noteDo {
	return n.withDO(n.DO.Not(conds...))
}

func (n noteDo) Or(conds ...gen.Condition) *noteDo {
	return n.withDO(n.DO.Or(conds...))
}

func (n noteDo) Select(conds ...field.Expr) *noteDo {
	return n.withDO(n.DO.Select(conds...))
}

func (n noteDo) Where(conds ...gen.Condition) *noteDo {
	return n.withDO(n.DO.Where(conds...))
}

func (n noteDo) Order(conds ...field.Expr) *noteDo {
	return n.withDO(n.DO.Order(conds...))
}

func (n noteDo) Distinct(cols ...field.Expr) *noteDo {
	return n.withDO(n.DO.Distinct(cols...))
}

func (n noteDo) Omit(cols ...field.Expr) *noteDo {
	return n.withDO(n.DO.Omit(cols...))
}

func (n noteDo) Join(table schema.Tabler, on ...field.Expr) *noteDo {
	return n.withDO(n.DO.Join(table, on...))
}

func (n noteDo) LeftJoin(table schema.Tabler, on ...field.Expr) *noteDo {
	return n.withDO(n.DO.LeftJoin(table, on...))
}

func (n noteDo) RightJoin(table schema.Tabler, on ...field.Expr) *noteDo {
	return n.withDO(n.DO.RightJoin(table, on...))
}

func (n noteDo) Group(cols ...field.Expr) *noteDo {
	return n.withDO(n.DO.Group(cols...))
}

func (n noteDo) Having(conds ...gen.Condition) *noteDo {
	return n.withDO(n.DO.Having(conds...))
}

func (n noteDo) Limit(limit int) *noteDo {
	return n.withDO(n.DO.Limit(limit))
}

func (n noteDo) Offset(offset int) *noteDo {
	return n.withDO(n.DO.Offset(offset))
}

func (n noteDo) Scopes(funcs ...func(gen.Dao) gen.Dao) *noteDo {
	return n.withDO(n.DO.Scopes(funcs...))
}

func (n noteDo) Unscoped() *noteDo {
	return n.withDO(n.DO.Unscoped())
}

func (n noteDo) Create(values ...*storage.Note) error {
	if len(values) == 0 {
		return nil
	}
	return n.DO.Create(values)
}

func (n noteDo) CreateInBatches(values []*storage.Note, batchSize int) error {
	return n.DO.CreateInBatches(values, batchSize)
}

// Save : !!! underlying implementation is different with GORM
// The method is equivalent to executing the statement: db.Clauses(clause.OnConflict{UpdateAll: true}).Create(values)
func (n noteDo) Save(values ...*storage.Note) error {
	if len(values) == 0 {
		return nil
	}
	return n.DO.Save(values)
}

func (n noteDo) First() (*storage.Note, error) {
	if result, err := n.DO.First(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Note), nil
	}
}

func (n noteDo) Take() (*storage.Note, error) {
	if result, err := n.DO.Take(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Note), nil
	}
}

func (n noteDo) Last() (*storage.Note, error) {
	if result, err := n.DO.Last(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Note), nil
	}
}

func (n noteDo) Find() ([]*storage.Note, error) {
	result, err := n.DO.Find()
	return result.([]*storage.Note), err
}

func (n noteDo) FindInBatch(batchSize int, fc func(tx gen.Dao, batch int) error) (results []*storage.Note, err error) {
	buf := make([]*storage.Note, 0, batchSize)
	err = n.DO.FindInBatches(&buf, batchSize, func(tx gen.Dao, batch int) error {
		defer func() { results = append(results, buf...) }()
		return fc(tx, batch)
	})
	return results, err
}

func (n noteDo) FindInBatches(result *[]*storage.Note, batchSize int, fc func(tx gen.Dao, batch int) error) error {
	return n.DO.FindInBatches(result, batchSize, fc)
}

func (n noteDo) Attrs(attrs ...field.AssignExpr) *noteDo {
	return n.withDO(n.DO.Attrs(attrs...))
}

func (n noteDo) Assign(attrs ...field.AssignExpr) *noteDo {
	return n.withDO(n.DO.Assign(attrs...))
}

func (n noteDo) Joins(fields ...field.RelationField) *noteDo {
	for _, _f := range fields {
		n = *n.withDO(n.DO.Joins(_f))
	}
	return &n
}

func (n noteDo) Preload(fields ...field.RelationField) *noteDo {
	for _, _f := range fields {
		n = *n.withDO(n.DO.Preload(_f))
	}
	return &n
}

func (n noteDo) FirstOrInit() (*storage.Note, error) {
	if result, err := n.DO.FirstOrInit(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Note), nil
	}
}

func (n noteDo) FirstOrCreate() (*storage.Note, error) {
	if result, err := n.DO.FirstOrCreate(); err != nil {
		return nil, err
	} else {
		return result.(*storage.Note), nil
	}
}

func (n noteDo) FindByPage(offset int, limit int) (result []*storage.Note, count int64, err error) {
	result, err = n.Offset(offset).Limit(limit).Find()
	if err != nil {
		return
	}

	if size := len(result); 0 < limit && 0 < size && size < limit {
		count = int64(size + offset)
		return
	}

	count, err = n.Offset(-1).Limit(-1).Count()
	return
}

func (n noteDo) ScanByPage(result interface{}, offset int, limit int) (count int64, err error) {
	count, err = n.Count()
	if err != nil {
		return
	}

	err = n.Offset(offset).Limit(limit).Scan(result)
	return
}

func (n noteDo) Scan(result interface{}) (err error) {
	return n.DO.Scan(result)
}

func (n noteDo) Delete(models ...*storage.Note) (result gen.ResultInfo, err error) {
	return n.DO.Delete(models)
}

func (n *noteDo) withDO(do gen.Dao) *noteDo {
	n.DO = *do.(*gen.DO)
	return n
}
