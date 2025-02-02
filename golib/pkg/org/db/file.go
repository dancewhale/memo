package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

func GetFileByID(id string) (*storage.File, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	f := dal.Use(db).File

	file, err := f.WithContext(context.Background()).Where(f.ID.Eq(id)).First()
	if err != nil {
		return nil, logger.Errorf("Get file record in sql db error: %v", err)
	}
	return file, nil
}

func FileDBUpdate(id, filePath, hash string) error {
	db, err := storage.InitDBEngine()
	if err != nil {
		return logger.Errorf("Init db engine error: %v", err)
	}
	f := dal.Use(db).File

	file, err := f.WithContext(context.Background()).Where(f.ID.Eq(id)).FirstOrCreate()
	if err != nil {
		return logger.Errorf("Check file record in db error: %v", err)
	}
	if file.FilePath != filePath || file.Hash != hash {
		_, err := f.WithContext(context.Background()).Where(f.ID.Eq(id)).
			UpdateSimple(f.FilePath.Value(filePath), f.Hash.Value(hash))

		if err != nil {
			return logger.Errorf("Check file record in db error: %v", err)
		}
	}
	return nil
}

func IfFileDBNeedUpdate(id, hash string) (bool, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return false, logger.Errorf("Init db engine error: %v", err)
	}
	f := dal.Use(db).File

	file, err := f.WithContext(context.Background()).Where(f.ID.Eq(id)).Find()
	if err != nil {
		return false, logger.Errorf("Check file record in db error: %v", err)
	}
	if len(file) == 0 {
		return true, nil
	} else if file[0].Hash == hash {
		return false, nil
	}
	return true, nil
}
