package db

import (
	"context"
	"strings"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/google/uuid"
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
	head := dal.Headline
	anno := dal.Annotation
	_, err := head.WithContext(context.Background()).Where(head.ID.Eq(annotation.ParentHeadlineID)).First()
	if err != nil {
		return nil, logger.Errorf("Failed to get headline %s: %v", annotation.ParentHeadlineID, err)
	}

	err = anno.WithContext(context.Background()).Create(annotation)
	if err != nil {
		logger.Errorf("Create annotation failed: %v", err)
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

func (adb *AnnotationDB) GetAnnotationOriginalText(annotationID uint) (string, error) {
	anno := dal.Annotation
	headline := dal.Headline
	annotation, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).First()
	if err != nil {
		return "", logger.Errorf("Failed to get annotation %d: %v", annotationID, err)
	}
	if annotation == nil {
		return "", logger.Errorf("Annotation %d not found", annotationID)
	}
	head, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(annotation.ParentHeadlineID)).First()
	if err != nil {
		return "", logger.Errorf("Failed to get headline %s: %v", annotation.ParentHeadlineID, err)
	}
	if head == nil {
		return "", logger.Errorf("Headline %s not found", annotation.ParentHeadlineID)
	}
	content := truncateUnicodeString(head.Content, int(annotation.Start), int(annotation.End))
	return content, nil
}

func (adb *AnnotationDB) GetAnnotationCommentText(annotationID uint) (string, error) {
	anno := dal.Annotation
	annotation, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).First()
	if err != nil {
		return "", logger.Errorf("Failed to get annotation %d: %v", annotationID, err)
	}
	return annotation.CommentText, nil
}

func (adb *AnnotationDB) UpdateAnnotationCommentText(annotationID uint, commentText string) error {
	anno := dal.Annotation
	_, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).Updates(anno.CommentText.Value(commentText))
	if err != nil {
		return logger.Errorf("Failed to update annotation %d comment text: %v", annotationID, err)
	}
	return nil
}

func (adb *AnnotationDB) GetAnnotationsByHeadlineID(headlineID string) ([]*storage.Annotation, error) {
	var annotations []*storage.Annotation
	anno := dal.Annotation
	annotations, err := anno.WithContext(context.Background()).Where(anno.ParentHeadlineID.Eq(headlineID)).Find()
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
		Updates(annotation)
	if err != nil {
		return logger.Errorf("Failed to update region of annotation %d: %v", annotation.ID, err)
	}
	return nil
}

func (adb *AnnotationDB) UpdateAnnotationRegion(annotation *storage.Annotation) error {
	anno := dal.Annotation
	_, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotation.ID)).
		UpdateSimple(
			anno.Start.Value(annotation.Start),
			anno.End.Value(annotation.End),
		)
	if err != nil {
		return logger.Errorf("Failed to update region of annotation %d: %v", annotation.ID, err)
	}
	return nil
}

func (adb *AnnotationDB) UpdateAnnotationTypeFace(annotation *storage.Annotation) error {
	anno := dal.Annotation
	_, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotation.ID)).
		UpdateSimple(
			anno.Face.Value(annotation.Face),
			anno.Type.Value(annotation.Type),
		)
	if err != nil {
		return logger.Errorf("Failed to update face and type of annotation %d: %v", annotation.ID, err)
	}
	return nil
}

func (adb *AnnotationDB) CreateHeadlineFromAnnotation(annotationID uint, commentText string) error {
	anno := dal.Annotation
	annotation, err := anno.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).First()
	if err != nil {
		return logger.Errorf("Failed to update annotation %d comment text: %v", annotationID, err)
	}

	if annotation.ChildHeadlineID == "" {
		db := dal.Q.Begin()

		id := uuid.New().String()
		ID := strings.ToUpper(id)
		head := &storage.Headline{
			ID:         ID,
			HeadlineID: &annotation.ParentHeadlineID,
			Title:      truncateUnicodeString(commentText, 0, 32),
			Content:    commentText,
		}
		err = db.Headline.Create(head)
		if err != nil {
			logger.Errorf("Create headline for annotation %d failed: %v", annotation.ID, err)
			_ = db.Rollback()
			return err
		}
		_, err = db.Annotation.WithContext(context.Background()).Where(anno.ID.Eq(annotationID)).
			Updates(anno.ChildHeadlineID.Value(ID))
		if err != nil {
			logger.Errorf("Create headline for annotation %d failed: %v", annotation.ID, err)
			_ = db.Rollback()
			return err
		}
		err = db.Commit()
		if err != nil {
			logger.Errorf("Update annotation %d  comment failed: %v", annotation.ID, err)
			_ = db.Rollback()
			return err
		}
	} else {
		headline := dal.Headline
		_, err = headline.WithContext(context.Background()).Where(headline.ID.Eq(annotation.ChildHeadlineID)).
			Updates(headline.Content.Value(commentText))
		return err
	}

	return nil
}
