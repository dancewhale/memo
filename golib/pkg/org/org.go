package org

import (
	"context"
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

func NewOrg() *OrgApi {
	return &OrgApi{db: storage.InitDBEngine(), hash: ""}
}

// 确保id 的file记录存在，如果文件更新了则，删除file的hash记录和相关headline，直到创建headline后变更hash。
func (o *OrgApi) initFile(fileId string, filePath string) (file storage.File, err error) {
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
		if existFile.Hash != o.hash {
			//file 记录存在且文件有更新，并删除相关的headline 和file 的hash记录, 后续file相关head记录需要重新创建。
			_, err = h.WithContext(context.Background()).Where(h.FileRefer.Eq(fileId)).Unscoped().Delete()
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

func (o *OrgApi) UploadFile(filePath string) (bool, error) {
	h := dal.Use(o.db).Headline
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
	file, err := o.initFile(fileID, filePath)
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
		err = h.WithContext(context.Background()).Create(sql.Headline...)
		if err != nil {
			return false, logger.Errorf("Upload file %s failed: %s, when create headline.", fileID, err.Error())
		}
		for _, headline := range sql.Headline {
			o.db.Session(&gorm.Session{FullSaveAssociations: true}).Updates(headline)
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
	n := dal.Use(o.db).Note

	notes, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).Where(n.Type.IsNotNull()).Find()
	if err != nil {
		return nil, logger.Errorf("Get headline by orgid failed: %v", err)
	}
	if len(notes) == 0 {
		return nil, nil
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
			// 多个headline引用了一个orgid，尝试修复。
			for _, headline := range headlines {
				_, err := o.UploadFile(headline.File.FilePath)
				if err != nil {
					return nil, logger.Errorf("GetHeadlineByOrgID %s failed because upload failed: %v", orgid, err.Error())
				}
			}
			headlines, err = h.WithContext(context.Background()).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
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
