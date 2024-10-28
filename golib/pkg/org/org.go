package org

import (
	"context"
	"crypto/md5"
	"encoding/hex"
	"io"
	"os"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/niklasfasching/go-org/org"
	"gorm.io/gorm"
)

type Org struct {
	db *gorm.DB
}

func NewOrg() *Org {
	return &Org{storage.InitDBEngine()}
}

var DB = storage.InitDBEngine()

func (o *Org) UploadFile(filePath string) (bool, error) {
	h := dal.Use(o.db).Headline
	fd := dal.Use(o.db).File

	file := storage.File{FilePath: filePath}
	// 创建file model包括地址和hash值。
	f, err := os.Open(file.FilePath)
	if err != nil {
		return false, err
	}

	hash := md5.New()

	_, err = io.Copy(hash, f)
	if err != nil {
		panic(err)
	}
	file.Hash = hex.EncodeToString(hash.Sum(nil))
	f.Close()
	// 判断是否存在file记录，且是否hash 一致。
	existFileList, err := fd.WithContext(context.Background()).Where(fd.FilePath.Eq(file.FilePath)).Find()
	if err != nil {
		return false, err
	}
	if len(existFileList) != 0 {
		existFile := existFileList[0]
		if existFile.Hash != file.Hash {
			// file 记录存在，更新md5值，并删除相关的headline 记录, 后续file相关head记录全部重新创建。
			_, err = h.WithContext(context.Background()).Where(h.FileRefer.Eq(file.FilePath)).Unscoped().Delete()
			if err != nil {
				return false, err
			}
			_, err = fd.WithContext(context.Background()).Where(fd.FilePath.Eq(file.FilePath)).Updates(file)
			if err != nil {
				return false, err
			}
		} else {
			// file 记录存在，且md5相等，直接返回，不做变动。
			return true, nil
		}
	} else { // file记录不存在，创建新的。
		err = fd.WithContext(context.Background()).Create(&file)
		if err != nil {
			return false, err
		}
	}
	f, err = os.Open(file.FilePath)
	defer f.Close()
	if err != nil {
		return false, err
	}
	doc := org.New().Parse(f, file.FilePath)
	if doc.Error != nil {
		return false, doc.Error
	}
	sql := NewSqlWriter()
	_, err = doc.Write(sql)
	if err != nil {
		return false, err
	}

	err = h.WithContext(context.Background()).Create(sql.Headline...)
	if err != nil {
		return false, err
	}
	return true, nil
}
