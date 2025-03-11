package org

import (
	"context"
	"errors"
	"strings"
	"sync"

	"memo/pkg/card"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/org/parser"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	"github.com/karrick/godirwalk"
	epc "github.com/kiwanami/go-elrpc"
	"gorm.io/gorm"
)

var muteDB = sync.Mutex{}
var muteFile = sync.Mutex{}

type OrgApi struct {
	db *gorm.DB
}

func NewOrgApi() (*OrgApi, error) {
	DB, err := storage.InitDBEngine()

	return &OrgApi{db: DB}, err
}

func (o *OrgApi) RegistryEpcMethod(service *epc.ServerService) *epc.ServerService {
	service.RegisterMethod(epc.MakeMethod("UploadFile", o.UploadFile, "string", "Upload org file to database"))
	service.RegisterMethod(epc.MakeMethod("SyncOrgDir", o.UploadFilesUnderDir, "string", "Upload org files under dir to database"))
	service.RegisterMethod(epc.MakeMethod("UpdateOrgHeadContent", o.UpdateOrgHeadContent, "string", "Update org head content"))
	service.RegisterMethod(epc.MakeMethod("UpdateOrgHeadProperty", o.UpdateOrgHeadProperty, "string", "Update org head property"))
	service.RegisterMethod(epc.MakeMethod("ExportOrgFileToDisk", o.ExportOrgFileToDisk, "string", "Export org file to disk"))
	service.RegisterMethod(epc.MakeMethod("CreateVirtualHead", o.CreateVirtHead, "string", "Create virtual head."))
	service.RegisterMethod(epc.MakeMethod("GetVirtualHeadByParentID", o.GetVirtHeadByParentID, "string", "Get VirtHead By parentID."))
	service.RegisterMethod(epc.MakeMethod("Test", o.Test, "string", "test"))
	return service
}

// UploadFile first load hash and filePath from file in disk，
// Then get nodes and fileId, first try to get from kv cache, second to parse file.
// force is not 0 mean alway update file even hash is same.
// batchMode is 1 mean not scan head for fsrs init after upload.
func (o *OrgApi) UploadFile(filePath string, fo int, batchMode int) util.Result {
	var force bool
	if fo == 0 {
		force = false
	} else {
		force = true
	}

	f, err := NewFileFromPath(filePath)
	if err != nil {
		return util.Result{Data: false, Err: err}
	} else if f == nil {
		return util.Result{Data: false, Err: err}
	}
	err = f.LoadFromFile(force)
	if err != nil && !errors.Is(err, parser.MissFileID) {
		return util.Result{Data: false, Err: err}
	} else if err != nil && errors.Is(err, parser.MissFileID) {
		logger.Warnf("Miss file id in file %s.", filePath)
		return util.Result{Data: false, Err: nil}
	}
	err = f.SaveDB(force)
	if err != nil {
		return util.Result{Data: false, Err: err}
	}

	if batchMode == 0 {
		o.InitFsrs()
	}

	if err != nil {
		return util.Result{Data: false, Err: err}
	} else {
		return util.Result{Data: true, Err: nil}
	}
}

func (o *OrgApi) Test(test string) error {
	return errors.New("test")
}

func (o *OrgApi) UploadFilesUnderDir(dirPath string, needForce int) util.Result {
	err := godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				result := o.UploadFile(osPathname, needForce, 1)
				if errors.Is(result.Err, parser.MissFileID) {
					return nil
				} else if errors.Is(result.Err, parser.FoundDupID) {
					return nil
				} else if result.Err != nil {
					return logger.Errorf("Upload file %s failed: %v", osPathname, result.Err)
				}
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	o.InitFsrs()
	return util.Result{Data: true, Err: nil}
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
	file, err := GetFileFromID(fileid)
	if err != nil {
		return err
	}
	err = file.SaveToDiskFile(file.path)
	if err != nil {
		return err
	}
	return nil
}

func (o *OrgApi) ExportOrgFileToDiskByOrgID(orgid string, filePath string) error {
	headline, err := o.GetHeadlineByOrgID(orgid)
	if err != nil {
		return err
	}
	if headline.FileID != nil {
		err = o.ExportOrgFileToDisk(*headline.FileID, filePath)
		if err != nil {
			return err
		}
	}
	return nil
}

func (o *OrgApi) UpdateOrgHeadContent(orgid, bodyContent string) util.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	bodyContent = strings.ReplaceAll(bodyContent, "\\\\", "\\")
	bodyContent = strings.ReplaceAll(bodyContent, "\\\"", "\"")
	err = headdb.UpdateHeadlineContent(orgid, bodyContent)
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	go func() {
		muteFile.Lock()
		defer muteFile.Unlock()
		o.ExportOrgFileToDiskByOrgID(orgid, "")
	}()
	return util.Result{Data: true, Err: nil}
}

func (o *OrgApi) UpdateOrgHeadProperty(orgid, key, value string) util.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	err = headdb.UpdateProperty(orgid, key, value)
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	go func() {
		muteFile.Lock()
		defer muteFile.Unlock()
		o.ExportOrgFileToDiskByOrgID(orgid, "")
	}()
	return util.Result{Data: true, Err: nil}
}

func (o *OrgApi) CreateVirtHead(parentid, title, content string) util.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	err = headdb.CreateVirtualHead(parentid, title, content)
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	o.InitFsrs()
	return util.Result{Data: true, Err: nil}
}

func (o *OrgApi) GetVirtHeadByParentID(parentid string) util.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	heads, err := headdb.GetVirtualHeadByParentID(parentid)
	if err != nil {
		return util.Result{Data: false, Err: err}
	}
	Heads := util.GetHeadStructs(heads, headdb)
	return util.Result{Data: Heads, Err: nil}
}

func (o *OrgApi) InitFsrs() {
	cardApi, err := card.NewCardApi()
	logger.Warnf("Failed to create CardApi instance: %v", err)
	// 异步执行 scanHeadlineInitFsrs
	go func() {
		muteDB.Lock()
		defer muteDB.Unlock()

		if _, err := cardApi.ScanHeadlineInitFsrs(); err != nil {
			logger.Warnf("Async scanHeadlineInitFsrs failed: %v", err)
		}
	}()
}
