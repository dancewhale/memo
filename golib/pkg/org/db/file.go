package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

func NewOrgFileDB() (*OrgFileDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	return &OrgFileDB{query: dal.Use(db)}, nil
}

type OrgFileDB struct {
	query *dal.Query
}

func (f *OrgFileDB) GetID() string {
	return "set"
}

func (f *OrgFileDB) GetFileByID(id string) (*storage.File, error) {
	file := f.query.File
	return file.WithContext(context.Background()).Where(file.ID.Eq(id)).First()
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
