package org

import (
	"context"
	"errors"
	"os"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

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
		logger.Errorf("hash error: %v", err)
		return storage.File{}, err
	}
	file = storage.File{ID: fileId, FilePath: filePath}
	// 判断是否存在id 的file记录，且是否hash 一致。
	existFileList, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).Find()
	if err != nil {
		logger.Errorf(err.Error())
		return storage.File{}, err
	}
	// 不存fileId 创建相应的记录
	if len(existFileList) == 0 {
		err = fd.WithContext(context.Background()).Create(&file)
		if err != nil {
			logger.Errorf("Create file record %s failed: %s", fileId, err.Error())
			return storage.File{}, err
		}
		return file, nil
	} else {
		// 存在fileId 记录，判断hash值。
		existFile := existFileList[0]
		if existFile.Hash != o.hash {
			//file 记录存在且文件有更新，并删除相关的headline 和file 的hash记录, 后续file相关head记录需要重新创建。
			_, err = h.WithContext(context.Background()).Where(h.FileRefer.Eq(fileId)).Unscoped().Delete()
			if err != nil {
				logger.Errorf("Remove head record for file %s failed: %s", fileId, err.Error())
				return storage.File{}, err
			}
			_, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).UpdateSimple(fd.FilePath.Value(filePath), fd.Hash.Zero())
			if err != nil {
				logger.Errorf("Update file hash to zore failed: %s", err.Error())
				return storage.File{}, err
			}
			return file, nil
		} else if existFile.FilePath != filePath {
			_, err := fd.WithContext(context.Background()).Where(fd.ID.Eq(fileId)).UpdateSimple(fd.FilePath.Value(filePath))
			if err != nil {
				logger.Errorf("Update file path to %s failed: %s", filePath, err.Error())
				return storage.File{}, err
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
		logger.Errorf(err.Error())
		return false, err
	}
	doc := org.New().Parse(f, filePath)
	if doc.Error != nil {
		return false, doc.Error
	}
	fileID := getFileID(doc)
	if fileID == "" {
		return false, errors.New("File id not found.")
	}
	// 初始化file记录, 无记录则创建，有记录如果有变动则去掉hash记录。
	file, err := o.initFile(fileID, filePath)
	if err != nil {
		logger.Errorf("Upload file %s failed: %s", fileID, err.Error())
		return false, err
	}
	sql := NewSqlWriter()
	sql.fileId = fileID
	_, err = doc.Write(sql)
	if err != nil {
		logger.Errorf(err.Error())
		return false, err
	}
	if file.Hash == "" {
		err = h.WithContext(context.Background()).Create(sql.Headline...)
		if err != nil {
			logger.Errorf("Upload file %s failed: %s, when create headline.", fileID, err.Error())
			return false, err
		}
		_, err = fd.WithContext(context.Background()).Where(fd.ID.Eq(file.ID)).UpdateSimple(fd.Hash.Value(o.hash))
		if err != nil {
			logger.Errorf("Upload file %s failed: %s, when update file hash in database.", fileID, err.Error())
			return false, err
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
		logger.Errorf("Get headline by orgid failed: %v", err)
		return nil, err
	}
	if len(notes) == 0 {
		return nil, nil
	} else {
		headlines, err := h.WithContext(context.Background()).Preload(h.File).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
		if err != nil {
			logger.Errorf("Get headline by orgid failed: %v", err)
			return nil, err
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
					logger.Errorf(err.Error())
					return nil, err
				}
			}
			headlines, err = h.WithContext(context.Background()).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
			if err != nil {
				logger.Errorf(err.Error())
				return nil, err
			}
			if len(headlines) == 1 {
				return headlines[0], nil
			} else if len(headlines) > 1 {
				logger.Errorf("orgid %s has no headline attach to it.", orgid)
				return nil, errors.New("orgid has more than one headline attach to it.")
			}
		}
	}
	return nil, nil
}
