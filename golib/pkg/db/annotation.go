package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
)

func NewAnnotationDB() (*AnnotationDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	if dal.Annotation != nil {
		dal.SetDefault(db)
	}
	return &AnnotationDB{db: db}, nil
}

type AnnotationDB struct {
	db *gorm.DB
}

func (adb *AnnotationDB) CreateAnnotation(annotation *storage.Annotation) (*storage.Annotation, error) {
	err := adb.db.Create(annotation).Error
	if err != nil {
		return nil, err
	}
	return annotation, nil
}

func (adb *AnnotationDB) GetAnnotationByID(annotationID uint) (*storage.Annotation, error) {
	anno := dal.Annotation
	annotation, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).First()
	if err != nil {
		return nil, logger.Errorf("Failed to get annotation %d: %v", annotationID, err)
	}
	return annotation, nil
}

func (adb *AnnotationDB) GetAnnotationsByHeadlineID(headlineID string) ([]*storage.Annotation, error) {
	var annotations []*storage.Annotation
	anno := dal.Annotation
	annotations, err := anno.WithContext(context.Background()).Where(anno.HeadlineID.Eq(headlineID)).Find()
	if err != nil {
		return nil, logger.Errorf("Failed to get annotations for headline %s: %v", headlineID, err)
	}
	return annotations, err
}

func (adb *AnnotationDB) DeleteAnnotationByID(annotationID uint) error {
	anno := dal.Annotation
	_, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).Delete()
	if err != nil {
		return logger.Errorf("Failed to delete annotation %d: %v", annotationID, err)
	}
	return nil
}

func (adb *AnnotationDB) UpdateAnnotation(annotation *storage.Annotation) error {
	anno := dal.Annotation
	_, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotation.ID)).
		UpdateSimple(
			anno.Start.Value(annotation.Start),
			anno.End.Value(annotation.End),
			anno.CommentText.Value(annotation.CommentText),
			anno.AnnoText.Value(annotation.AnnoText),
		)
	if err != nil {
		return logger.Errorf("Failed to update annotation %d: %v", annotation.ID, err)
	}
	return nil
}
