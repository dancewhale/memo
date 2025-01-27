package org

import (
	"context"
	"errors"
	"log"
	"memo/pkg/logger"
	"memo/pkg/org/parser"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"os"
	"runtime/pprof"
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

func (o *OrgApi) UploadFileToCache(filePath string) error {
	f, err := NewFileFromPath(filePath)
	if err != nil {
		return err
	}
	if err := f.LoadFromFile(); err != nil {
		return err
	}
	return f.SaveToKvDB()
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
	err = f.LoadFromFile()
	if !errors.Is(err, parser.FileExistInKv) {
		return err
	} else if errors.Is(err, parser.FileExistInKv) && !force {
		return nil
	} else {
		err = f.SaveToKvDB()
		if err != nil {
			return err
		}
		err = f.SaveToSqlDB()
		if err != nil {
			return err
		}
	}
	return nil
}

func (e *OrgApi) UploadFilesUnderDir(dirPath string, needForce bool) error {
	f, err := os.Create("/tmp/cpu.prof")
	if err != nil {
		log.Fatal(err)
	}
	pprof.StartCPUProfile(f)
	defer pprof.StopCPUProfile()

	err = godirwalk.Walk(dirPath, &godirwalk.Options{
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
