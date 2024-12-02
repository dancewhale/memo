package db

import (
	"context"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
)

// 该文件中的函数用于数据库中对File表的操作，包括文件的创建，更新，删除等操作。

func LoadFileFromDB(id string) (*storage.File, error) {
	r := dal.Use(storage.Engine).File
	file := storage.File{ID: id}
	files, err := r.WithContext(context.Background()).Where(r.ID.Eq(id)).Find()
	if err != nil {
		return nil, logger.Errorf("File search error for %s: %v", id, err)
	}
	if len(files) == 0 {
		err := r.WithContext(context.Background()).Create(&file)
		if err != nil {
			return nil, logger.Errorf("Create file record %s failed: %s", id, err.Error())
		}
		return &file, nil
	} else if len(files) == 1 {
		return files[0], nil
	} else {
		return nil, logger.Errorf("File search error for %s: more than one record found", id)
	}
}

func Save(file storage.File) {
	storage.Engine.Session(&gorm.Session{FullSaveAssociations: true}).Updates(file)
}
