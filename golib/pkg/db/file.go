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
	if dal.File == nil {
		dal.SetDefault(db)
	}
	return &OrgFileDB{db: db}, nil
}

type OrgFileDB struct {
	db *gorm.DB
}

func preload(d *gorm.DB) *gorm.DB {
	return d.Order("`order` ASC").Preload("Children", "type = 1", preload).
		Preload("Properties").Preload("Tags").Preload("LogBook")
}
func (f *OrgFileDB) GetHeadTree(fileID string, filetype int) ([]storage.Headline, error) {
	var heads []storage.Headline
	var err error
	if filetype == storage.NormalFile {
		err = f.db.Model(&storage.Headline{}).Order("`order` ASC").
			Where("file_id = ? AND level = ?", fileID, 1).
			Preload("Properties").Preload("LogBook").Preload("Tags").
			Preload("Children", preload).Find(&heads).Error
	} else if filetype == storage.VirtualFile {
		err = f.db.Model(&storage.Headline{}).Order("`order` ASC").
			Where("headline_id = ? AND level = ?", fileID, 1).
			Preload("Properties").Preload("LogBook").Preload("Tags").
			Preload("Children", preload).Find(&heads).Error
	}
	if err != nil {
		return nil, logger.Errorf("Get headline of file %s error: %v", fileID, err)
	}
	return heads, nil
}

func (f *OrgFileDB) GetFileByHash(hash string) (*storage.File, error) {
	file := dal.File
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
	file := dal.File
	_, err := file.WithContext(context.Background()).Where(file.ID.Eq(fileid)).UpdateSimple(file.Hash.Value(hash))
	if err != nil {
		return logger.Errorf("Update file hash error: %v", err)
	}

	return nil
}

// if filetype is virt build virt file, otherwise build normal file
func (f *OrgFileDB) GetFileByID(id string, filetype int) (*storage.File, error) {
	ctx := context.Background()
	if filetype == storage.NormalFile {
		file := dal.File
		files, err := file.WithContext(ctx).Where(file.ID.Eq(id)).Find()
		if err != nil {
			return nil, logger.Errorf("Get file by id %s error: %v", id, err)
		}
		if len(files) == 0 {
			return nil, nil
		}
		heads, err := f.GetHeadTree(id, filetype)
		if err != nil {
			return nil, err
		}
		files[0].Headlines = heads
		return files[0], nil
	} else if filetype == storage.VirtualFile {
		file := &storage.File{ID: id}
		head := dal.Headline
		heads, err := head.WithContext(ctx).Where(head.ID.Eq(id)).Find()
		if err != nil {
			return nil, logger.Errorf("Get virt file by head id %s error: %v", id, err)
		}
		if len(heads) == 0 {
			return nil, nil
		} else if len(heads) > 1 {
			return nil, logger.Errorf("Found duplicate head id %s", id)
		} else if heads[0].VirtFileHash != nil {
			file.Hash = *heads[0].VirtFileHash
		}
		headlist, err := f.GetHeadTree(id, filetype)
		if err != nil {
			return nil, err
		}
		file.Headlines = headlist
		return file, nil
	} else {
		return nil, nil
	}
}

func FileDBUpdate(fd *storage.File, force bool, filetype int) error {
	f := dal.Q.File
	h := dal.Q.Headline

	if filetype == storage.NormalFile {
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
			// 使相关缓存失效
			if cacheManager := GetCacheManager(); cacheManager != nil {
				cacheManager.InvalidateCache(fd.ID)
			}
		}
	} else if filetype == storage.VirtualFile {
		head, err := h.WithContext(context.Background()).Where(h.ID.Eq(fd.ID)).First()
		if err != nil {
			return logger.Errorf("Check virt file record in db by head %s error: %v", fd.ID, err)
		}
		if head != nil {
			if head.VirtFileHash == nil || *head.VirtFileHash != fd.Hash || force {
				_, err := h.WithContext(context.Background()).Where(h.ID.Eq(fd.ID)).
					UpdateSimple(h.VirtFileHash.Value(fd.Hash))
				if err != nil {
					return logger.Errorf("Update virt file record in db error: %v", err)
				}
				// 使相关缓存失效
				if cacheManager := GetCacheManager(); cacheManager != nil {
					cacheManager.InvalidateCache(fd.ID)
				}
			}
		}
	}

	return nil
}

func IfFileDBNeedUpdate(id, hash string, filetype int) (bool, error) {
	f := dal.Q.File
	h := dal.Q.Headline
	var file []*storage.File
	var head *storage.Headline
	var err error

	if filetype == storage.NormalFile {
		file, err = f.WithContext(context.Background()).Where(f.ID.Eq(id)).Find()
		if err != nil {
			return false, logger.Errorf("Check normal file record in db error: %v", err)
		}
	} else if filetype == storage.VirtualFile {
		head, err = h.WithContext(context.Background()).Where(h.ID.Eq(id)).First()
		if err != nil {
			return false, logger.Errorf("Check virt file record in db error: %v", err)
		}
		if head != nil {
			if head.VirtFileHash != nil && *head.VirtFileHash == hash {
				return false, nil
			} else {
				return true, nil
			}
		}
	}
	if len(file) == 0 {
		return true, nil
	} else if file[0].Hash == hash {
		return false, nil
	}
	return true, nil
}
