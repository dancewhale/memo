package db

import (
	"context"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
)

func NewOrgFileDB() (*OrgFileDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	return &OrgFileDB{query: dal.Use(db), db: db}, nil
}

type OrgFileDB struct {
	query *dal.Query
	db    *gorm.DB
}

func preload(d *gorm.DB) *gorm.DB {
	return d.Order("`order` ASC").Preload("Children", "type = 1", preload).Preload("Properties")
}
func (f *OrgFileDB) GetHeadTree(fileID string) ([]storage.Headline, error) {
	var heads []storage.Headline
	err := f.db.Model(&storage.Headline{}).Order("`order` ASC").
		Where("file_id = ? AND level = ?", fileID, 1).
		Preload("Properties").Preload("Children", "type = 1", preload).Find(&heads).Error
	if err != nil {
		return nil, logger.Errorf("Get headline of file %s error: %v", fileID, err)
	}
	return heads, nil
}

func (f *OrgFileDB) GetFileByHash(hash string) (*storage.File, error) {
	file := f.query.File
	files, err := file.WithContext(context.Background()).Where(file.Hash.Eq(hash)).Find()
	if err != nil {
		return nil, logger.Errorf("Get file by hash %s error: %v", hash, err)
	}
	if len(files) == 0 {
		return nil, nil
	}
	return files[0], nil
}

func (f *OrgFileDB) UpdateFileHash(fileid, hash string) error {
	file := f.query.File
	_, err := file.WithContext(context.Background()).Where(file.ID.Eq(fileid)).UpdateSimple(file.Hash.Value(hash))
	if err != nil {
		return logger.Errorf("Update file hash error: %v", err)
	}
	return nil
}

func (f *OrgFileDB) GetFileByID(id string) (*storage.File, error) {
	ctx := context.Background()
	file := f.query.File
	files, err := file.WithContext(ctx).Where(file.ID.Eq(id)).Find()
	if err != nil {
		return nil, logger.Errorf("Get file by id %s error: %v", id, err)
	}
	if len(files) == 0 {
		return nil, nil
	}
	heads, err := f.GetHeadTree(id)
	if err != nil {
		return nil, err
	}
	files[0].Headlines = heads
	return files[0], nil
}

func FileDBUpdate(fd *storage.File, force bool) error {
	db, err := storage.InitDBEngine()
	if err != nil {
		return logger.Errorf("Init db engine error: %v", err)
	}
	f := dal.Use(db).File

	file, err := f.WithContext(context.Background()).Where(f.ID.Eq(fd.ID)).FirstOrCreate()
	if err != nil {
		return logger.Errorf("Check file record in db error: %v", err)
	}
	if file.FilePath != fd.FilePath || file.Hash != fd.Hash || force {
		_, err := f.WithContext(context.Background()).Where(f.ID.Eq(fd.ID)).
			UpdateSimple(f.FilePath.Value(fd.FilePath), f.Hash.Value(fd.Hash), f.MetaContent.Value(fd.MetaContent))

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
