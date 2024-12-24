package db

import (
	"context"
	"github.com/emirpasic/gods/maps/linkedhashmap"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

// load file and headline from db.
// file data prepare for hash check.
func LoadFileFromDB(id string) (*File, error) {
	f := dal.Use(storage.Engine).File
	file, err := f.WithContext(context.Background()).Where(f.ID.Eq(id)).Find()
	var filedb File
	if err != nil {
		return nil, logger.Errorf("Load id %s file from db error for %v", id, err)
	}
	if len(file) == 0 {
		filedb = File{}
	} else if len(file) > 1 {
		return nil, logger.Errorf("Find more than one file with the same id %s", id)
	} else {
		filedb = File{ID: id, Hash: file[0].Hash, FilePath: file[0].FilePath}
	}
	if err := filedb.loadHeadCache(); err != nil {
		return nil, err
	}
	return &filedb, nil
}

// 该文件中的函数用于数据库中对File表的操作，包括文件的创建，更新，删除等操作。
type File struct {
	ID       string
	Hash     string
	FilePath string
	hcache   *HeadsCache
}

func (f *File) loadHeadCache() error {
	h, err := LoadHeadsByFileIDFromDB(f.ID)
	if err != nil {
		return err
	}
	f.hcache = h
	return nil
}

func (f *File) Update(id, hash, filePath string, headFromFileCache *linkedhashmap.Map, force bool) error {
	fd := dal.Use(storage.Engine).File
	file := storage.File{ID: id, Hash: hash, FilePath: filePath}
	_, err := fd.WithContext(context.Background()).Updates(&file)
	if err != nil {
		return logger.Errorf("Create file in db error: %v", err)
	}
	err = f.hcache.UpdateHeadlineToDB(headFromFileCache, force)
	if err != nil {
		return err
	}
	return nil
}

// 文件id 不存在创建记录并更新headline
func (f *File) Create(id, hash, filePath string, headFromFileCache *linkedhashmap.Map) error {
	fd := dal.Use(storage.Engine).File
	file := storage.File{ID: id, Hash: hash, FilePath: filePath}
	err := fd.WithContext(context.Background()).Create(&file)
	if err != nil {
		return logger.Errorf("Create file in db error: %v", err)
	}
	err = f.hcache.UpdateHeadlineToDB(headFromFileCache, false)
	if err != nil {
		return err
	}
	return nil
}
