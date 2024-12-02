package org

import (
	"context"
	"fmt"
	"memo/pkg/org/db"
	"os"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	"github.com/niklasfasching/go-org/org"
	"gorm.io/gorm"
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
func (o *OrgApi) initFile2(filePath string, force bool) (file *storage.File, err error) {
	f, err := NewFileFromPath(filePath)
	if err != nil {
		return nil, err
	}
	if err := f.Load(); err != nil {
		return nil, err
	}
	_, _ = db.LoadHeadlinesFromDB(f.Meta.ID)
	return nil, nil
}

// 确保id 的file记录存在，如果文件更新了则，删除file的hash记录和相关headline，直到创建headline后变更hash。
func (o *OrgApi) initFile(fileId string, filePath string, force bool) (file storage.File, err error) {
	fd := dal.Use(o.db).File
	h := dal.Use(o.db).Headline
	// 创建file model包括地址和hash值,如果不存在
	o.hash, err = hash(filePath)
	if err != nil {
		return storage.File{}, logger.Errorf("hash error: %v", err)
	}
	file = storage.File{ID: fileId, FilePath: filePath}
	// 判断是否存在id 的file记录，且是否hash 一致。
	existFileList, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).Find()
	if err != nil {
		return storage.File{}, logger.Errorf("File search error for %s: %v", fileId, err)
	}
	// 不存fileId 创建相应的记录
	if len(existFileList) == 0 {
		err = fd.WithContext(context.Background()).Create(&file)
		if err != nil {
			return storage.File{}, logger.Errorf("Create file record %s failed: %s", fileId, err.Error())
		}
		return file, nil
	} else {
		// 存在fileId 记录，判断hash值。
		existFile := existFileList[0]
		if existFile.Hash != o.hash || force {
			//file 记录存在且文件有更新或者force 为true，删除相关的headline 和file 的hash记录, 后续file相关head记录需要重新创建。
			_, err = h.WithContext(context.Background()).Where(h.FileID.Eq(fileId)).Unscoped().Delete()
			if err != nil {
				return storage.File{}, logger.Errorf("Remove head record for file %s failed: %s", fileId, err.Error())
			}
			_, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).UpdateSimple(fd.FilePath.Value(filePath), fd.Hash.Zero())
			if err != nil {
				return storage.File{}, logger.Errorf("Update file hash to zore failed: %s", err.Error())
			}
			return file, nil
		} else if existFile.FilePath != filePath {
			_, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).UpdateSimple(fd.FilePath.Value(filePath))
			if err != nil {
				return storage.File{}, logger.Errorf("Update file path to %s failed: %s", filePath, err.Error())
			}
			return file, nil
		} else {
			// file 记录存在，且md5相等，直接返回，不做变动。
			file.Hash = o.hash
			return file, nil
		}
	}
}

func (o *OrgApi) UploadFile2(filePath string, force bool) (bool, error) {

	//file = NewFileFromPath(filePath)
	return true, nil
}

func (o *OrgApi) UploadFile(filePath string, force bool) (bool, error) {
	fd := dal.Use(o.db).File

	f, err := os.Open(filePath)
	defer f.Close()
	if err != nil {
		return false, logger.Errorf("Open file %s failed: %v", filePath, err.Error())
	}
	doc := org.New().Parse(f, filePath)
	if doc.Error != nil {
		return false, logger.Errorf("Parse file %s failed: %s", filePath, doc.Error)
	}
	fileID := getFileID(doc)
	if fileID == "" {
		return false, util.NoFileIdFoundError
	}
	// 初始化file记录, 无记录则创建，有记录如果有变动则去掉hash记录。
	file, err := o.initFile(fileID, filePath, force)
	if err != nil {
		return false, logger.Errorf("Upload file %s failed: %s", fileID, err.Error())
	}
	sql := NewSqlWriter()
	sql.fileId = fileID
	_, err = doc.Write(sql)
	if err != nil {
		return false, logger.Errorf("Upload file %s failed for doc.Write err: %v", filePath, err.Error())
	}
	if file.Hash == "" {
		for _, headline := range sql.Headlines {
			// TODO: 此处需要替换，创建headline
			fmt.Printf("headline: %v\n", headline)
		}
		if err != nil {
			return false, logger.Errorf("Upload file %s failed: %s, when create headline.", fileID, err.Error())
		}
		// TODO: 此处需要替换，创建headline
		for _, headline := range sql.Headlines {
			fmt.Printf("headline: %v\n", headline)
		}
		_, err = fd.WithContext(context.Background()).Where(fd.ID.Eq(file.ID)).UpdateSimple(fd.Hash.Value(o.hash))
		if err != nil {
			return false, logger.Errorf("Upload file %s failed: %s, when update file hash in database.", fileID, err.Error())
		}
		return true, nil
	}
	return true, nil
}

// GetNote 获取一张卡片,首先判断org是否有fsrs学习记录，没有
func (o *OrgApi) GetHeadlineByOrgID(orgid string) (*storage.Headline, error) {
	h := dal.Use(o.db).Headline
	c := dal.Use(o.db).Card

	// TODO: card 的type 绑定到headline 上,逻辑变了需要修正。
	notes, err := c.WithContext(context.Background()).Where(c.Orgid.Eq(orgid)).Find()
	if err != nil {
		return nil, logger.Errorf("Get headline by orgid failed: %v", err)
	}
	if len(notes) == 0 {
		return nil, logger.Errorf("The orgid your request has no memo card")
	} else {
		headlines, err := h.WithContext(context.Background()).Preload(h.File).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
		if err != nil {
			return nil, logger.Errorf("Get headline by orgid failed: %v", err)
		} else if len(headlines) == 0 {
			logger.Warnf("orgid %s has no headline attach to it.", orgid)
			return nil, nil
		} else if len(headlines) == 1 {
			return headlines[0], nil
		} else if len(headlines) > 1 {
			// 多个headline引用了一个orgid，尝试修复, 重新上传相关文件，删除多余的headline
			for _, headline := range headlines {
				_, err := o.UploadFile(headline.File.FilePath, true)
				if err != nil {
					return nil, logger.Errorf("GetHeadlineByOrgID %s failed because upload failed: %v", orgid, err.Error())
				}
			}
			headlines, err = h.WithContext(context.Background()).Preload(h.File).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
			if err != nil {
				return nil, logger.Errorf("GetHeadlineByOrgID %s failed for headline search error: %v", orgid, err.Error())
			}
			if len(headlines) == 1 {
				return headlines[0], nil
			} else if len(headlines) > 1 {
				return nil, logger.Errorf("orgid %s has no headline attach to it.", orgid)
			}
		}
	}
	return nil, nil
}

//// Use Upload File to recursion  upload all files under dir.
//func (o *OrgApi) UploadDireFiles(dir string) (bool, error) {
//
//}
