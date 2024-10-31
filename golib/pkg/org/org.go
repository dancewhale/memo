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
	db *gorm.DB
}

func NewOrg() *OrgApi {
	return &OrgApi{storage.InitDBEngine()}
}

func (o *OrgApi) UploadFile(filePath string) (bool, error) {
	h := dal.Use(o.db).Headline
	fd := dal.Use(o.db).File

	// 创建file model包括地址和hash值。
	hash, err := hash(filePath)
	if err != nil {
		return false, err
	}
	// 判断是否存在file记录，且是否hash 一致。
	existFileList, err := fd.WithContext(context.Background()).Where(fd.FilePath.Eq(filePath)).Find()
	if err != nil {
		logger.Errorf(err.Error())
		return false, err
	}
	if len(existFileList) != 0 {
		existFile := existFileList[0]
		if existFile.Hash != file.Hash {
			// file 记录存在，更新md5值，并删除相关的headline 记录, 后续file相关head记录全部重新创建。
			_, err = h.WithContext(context.Background()).Where(h.FileRefer.Eq(filePath)).Unscoped().Delete()
			if err != nil {
				logger.Errorf(err.Error())
				return false, err
			}
		} else {
			// file 记录存在，且md5相等，直接返回，不做变动。
			return true, nil
		}
	} else { // file记录不存在，创建新的。
		file := storage.File{FilePath: filePath, Hash: hash}
		err = fd.WithContext(context.Background()).Create(&file)
		if err != nil {
			logger.Errorf(err.Error())
			return false, err
		}
	}
	f, err = os.Open(file.FilePath)
	defer f.Close()
	if err != nil {
		logger.Errorf(err.Error())
		return false, err
	}
	doc := org.New().Parse(f, file.FilePath)
	if doc.Error != nil {
		logger.Errorf(err.Error())
		return false, doc.Error
	}
	sql := NewSqlWriter()
	_, err = doc.Write(sql)
	if err != nil {
		logger.Errorf(err.Error())
		return false, err
	}
	err = h.WithContext(context.Background()).Create(sql.Headline...)
	if err != nil {
		return false, err
	}
	// 更新headline后再更新file的hash
	_, err = fd.WithContext(context.Background()).Where(fd.FilePath.Eq(file.FilePath)).Updates(file)
	if err != nil {
		logger.Errorf(err.Error())
		return false, err
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
		headlines, err := h.WithContext(context.Background()).Order(h.UpdatedAt.Desc()).Where(h.OrgID.Eq(notes[0].Orgid)).Find()
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
				_, err := o.UploadFile(headline.FileRefer)
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
