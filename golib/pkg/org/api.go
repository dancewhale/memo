package org

import (
	"context"
	"errors"
	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/org/parser"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"strings"

	"github.com/karrick/godirwalk"
	"gorm.io/gorm"
)

type OrgApi struct {
	db *gorm.DB
}

func NewOrgApi() (*OrgApi, error) {
	DB, err := storage.InitDBEngine()

	return &OrgApi{db: DB}, err
}

// UploadFile first load hash and filePath from file in disk，
// Then get nodes and fileId, first try to get from kv cache, second to parse file.
// force mean alway update file even hash is same.
func (o *OrgApi) UploadFile(filePath string, force bool) error {
	f, err := NewFileFromPath(filePath)
	if err != nil {
		return err
	} else if f == nil {
		return nil
	}
	err = f.LoadFromFile(force)
	if err != nil && !errors.Is(err, parser.MissFileID) {
		return err
	} else if err != nil && errors.Is(err, parser.MissFileID) {
		logger.Warnf("Miss file id in file %s.", filePath)
		return nil
	}
	return f.SaveDB(force)
}

func (e *OrgApi) UploadFilesUnderDir(dirPath string, needForce bool) error {

	err := godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				err := e.UploadFile(osPathname, needForce)
				if errors.Is(err, parser.MissFileID) {
					return nil
				} else if errors.Is(err, parser.FoundDupID) {
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

func (o *OrgApi) ExportOrgFileToDisk(fileid string, filePath string) error {
	f, err := GetFileFromFileID(fileid)
	if err != nil {
		return err
	}
	err = f.SaveToDiskFile(filePath)
	if err != nil {
		return err
	}
	return nil
}

func (o *OrgApi) UpdateOrgHeadContent(orgid, bodyContent string) error {
	file, err := GetFileFromHeadID(orgid)
	if err != nil {
		return err
	}
	err = file.LoadFromFile(false)
	if err != nil {
		return err
	}
	list, index := file.GetHeadlineByID(orgid)
	if list == nil {
		return logger.Errorf("Can not find headline by orgid %s in file Nodes.", orgid)
	}

	head, _ := list.Get(index)
	if h, ok := head.(parser.Headline); ok {
		h.BodyContent = bodyContent
		list.Set(index, h)
	} else {
		return logger.Errorf("Get headline by orgid %s failed.", orgid)
	}
	err = file.SaveToDiskFile(file.Path)
	if err != nil {
		return err
	}
	err = file.SaveToKvDB(false)
	if err != nil {
		return err
	}
	err = db.UpdateHeadContentByID(orgid, bodyContent)
	if err != nil {
		return err
	}
	return nil
}

func (o *OrgApi) UpdateOrgHeadProperty(orgid, key, value string) error {
	file, err := GetFileFromHeadID(orgid)
	if err != nil {
		return err
	}
	err = file.LoadFromFile(false)
	if err != nil {
		return err
	}
	list, index := file.GetHeadlineByID(orgid)
	if list == nil {
		return logger.Errorf("Can not find headline by orgid %s in file Nodes.", orgid)
	}

	head, _ := list.Get(index)
	if h, ok := head.(parser.Headline); ok {
		h.Properties.Set(key, value)
		list.Set(index, h)
	} else {
		return logger.Errorf("Get headline by orgid %s failed.", orgid)
	}
	err = file.SaveToDiskFile(file.Path)
	if err != nil {
		return err
	}
	err = file.SaveToKvDB(false)
	if err != nil {
		return err
	}
	if key == options.GetPropertySchedule() {
		err = db.UpdateHeadScheduleTypeByID(orgid, value)
		if err != nil {
			return err
		}
	}
	return nil
}
