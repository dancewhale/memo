package org

import (
	"context"
	"errors"
	"github.com/karrick/godirwalk"
	"gorm.io/gorm"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"strings"
)

type OrgApi struct {
	db   *gorm.DB
	hash string
}

func NewOrgApi() (*OrgApi, error) {
	DB, err := storage.InitDBEngine()

	return &OrgApi{db: DB, hash: ""}, err
}

// 文件是否更新，更新则删除file的hash记录和相关headline，直到创建headline后变更hash。
// force mean alway update file even hash is same.
func (o *OrgApi) UploadFile(filePath string, force bool) error {
	f, err := NewFileFromPath(filePath)
	if err != nil {
		return err
	} else if f == nil {
		return nil
	}
	if err := f.Load(); err != nil {
		return err
	}
	if f.Data.Hash != f.Hash || force {
		return f.UpdateFile(force)
	} else {
		return nil
	}
}

func (e *OrgApi) UploadFilesUnderDir(dirPath string, needForce bool) error {
	err := godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				err := e.UploadFile(osPathname, needForce)
				if errors.Is(err, MissFileID) {
					return nil
				} else if errors.Is(err, FoundDupID) {
					return nil
				} else if err != nil {
					return logger.Errorf("Upload file %s failed: %v", osPathname, err)
				}
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	return err
}

// GetNote 获取一张卡片,首先判断org是否有fsrs学习记录，没有
func (o *OrgApi) GetHeadlineByOrgID(orgid string) (*storage.Headline, error) {
	h := dal.Use(o.db).Headline

	headlines, err := h.WithContext(context.Background()).Preload(h.File).Order(h.UpdatedAt.Desc()).Where(h.ID.Eq(orgid)).Find()
	if err != nil {
		return nil, logger.Errorf("Get headline by orgid failed: %v", err)
	} else if len(headlines) == 0 {
		logger.Warnf("orgid %s has no headline attach to it.", orgid)
		return nil, nil
	} else if len(headlines) == 1 {
		return headlines[0], nil
	} else {
		return nil, logger.Errorf("The orgid %s has more than one headline attach to it.", orgid)
	}
}
