// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.
// Code generated by gorm.io/gen. DO NOT EDIT.

package dal

import (
	"context"
	"database/sql"

	"gorm.io/gorm"

	"gorm.io/gen"

	"gorm.io/plugin/dbresolver"
)

var (
	Q          = new(Query)
	Annotation *annotation
	Clock      *clock
	File       *file
	FsrsInfo   *fsrsInfo
	Headline   *headline
	Property   *property
	ReviewLog  *reviewLog
	Tag        *tag
)

func SetDefault(db *gorm.DB, opts ...gen.DOOption) {
	*Q = *Use(db, opts...)
	Annotation = &Q.Annotation
	Clock = &Q.Clock
	File = &Q.File
	FsrsInfo = &Q.FsrsInfo
	Headline = &Q.Headline
	Property = &Q.Property
	ReviewLog = &Q.ReviewLog
	Tag = &Q.Tag
}

func Use(db *gorm.DB, opts ...gen.DOOption) *Query {
	return &Query{
		db:         db,
		Annotation: newAnnotation(db, opts...),
		Clock:      newClock(db, opts...),
		File:       newFile(db, opts...),
		FsrsInfo:   newFsrsInfo(db, opts...),
		Headline:   newHeadline(db, opts...),
		Property:   newProperty(db, opts...),
		ReviewLog:  newReviewLog(db, opts...),
		Tag:        newTag(db, opts...),
	}
}

type Query struct {
	db *gorm.DB

	Annotation annotation
	Clock      clock
	File       file
	FsrsInfo   fsrsInfo
	Headline   headline
	Property   property
	ReviewLog  reviewLog
	Tag        tag
}

func (q *Query) Available() bool { return q.db != nil }

func (q *Query) clone(db *gorm.DB) *Query {
	return &Query{
		db:         db,
		Annotation: q.Annotation.clone(db),
		Clock:      q.Clock.clone(db),
		File:       q.File.clone(db),
		FsrsInfo:   q.FsrsInfo.clone(db),
		Headline:   q.Headline.clone(db),
		Property:   q.Property.clone(db),
		ReviewLog:  q.ReviewLog.clone(db),
		Tag:        q.Tag.clone(db),
	}
}

func (q *Query) ReadDB() *Query {
	return q.ReplaceDB(q.db.Clauses(dbresolver.Read))
}

func (q *Query) WriteDB() *Query {
	return q.ReplaceDB(q.db.Clauses(dbresolver.Write))
}

func (q *Query) ReplaceDB(db *gorm.DB) *Query {
	return &Query{
		db:         db,
		Annotation: q.Annotation.replaceDB(db),
		Clock:      q.Clock.replaceDB(db),
		File:       q.File.replaceDB(db),
		FsrsInfo:   q.FsrsInfo.replaceDB(db),
		Headline:   q.Headline.replaceDB(db),
		Property:   q.Property.replaceDB(db),
		ReviewLog:  q.ReviewLog.replaceDB(db),
		Tag:        q.Tag.replaceDB(db),
	}
}

type queryCtx struct {
	Annotation IAnnotationDo
	Clock      IClockDo
	File       IFileDo
	FsrsInfo   IFsrsInfoDo
	Headline   IHeadlineDo
	Property   IPropertyDo
	ReviewLog  IReviewLogDo
	Tag        ITagDo
}

func (q *Query) WithContext(ctx context.Context) *queryCtx {
	return &queryCtx{
		Annotation: q.Annotation.WithContext(ctx),
		Clock:      q.Clock.WithContext(ctx),
		File:       q.File.WithContext(ctx),
		FsrsInfo:   q.FsrsInfo.WithContext(ctx),
		Headline:   q.Headline.WithContext(ctx),
		Property:   q.Property.WithContext(ctx),
		ReviewLog:  q.ReviewLog.WithContext(ctx),
		Tag:        q.Tag.WithContext(ctx),
	}
}

func (q *Query) Transaction(fc func(tx *Query) error, opts ...*sql.TxOptions) error {
	return q.db.Transaction(func(tx *gorm.DB) error { return fc(q.clone(tx)) }, opts...)
}

func (q *Query) Begin(opts ...*sql.TxOptions) *QueryTx {
	tx := q.db.Begin(opts...)
	return &QueryTx{Query: q.clone(tx), Error: tx.Error}
}

type QueryTx struct {
	*Query
	Error error
}

func (q *QueryTx) Commit() error {
	return q.db.Commit().Error
}

func (q *QueryTx) Rollback() error {
	return q.db.Rollback().Error
}

func (q *QueryTx) SavePoint(name string) error {
	return q.db.SavePoint(name).Error
}

func (q *QueryTx) RollbackTo(name string) error {
	return q.db.RollbackTo(name).Error
}
