package db

import (
	"memo/pkg/logger"
	"memo/pkg/storage"
)

func CheckFileDBRecord(id, filePath, hash string) error {
	db, err := storage.InitDBEngine()
	if err != nil {
		return logger.Errorf("Init db engine error: %v", err)
	}
	file := storage.File{ID: id, Hash: hash, FilePath: filePath}
	err = db.FirstOrCreate(file, "id = ?", id).Error
	if err != nil {
		return logger.Errorf("Check file record in db error: %v", err)
	}
	return nil
}
