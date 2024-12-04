package org

import (
	"bytes"
	"crypto/md5"
	"encoding/hex"
	"io"
	"os"
	"path/filepath"

	"github.com/niklasfasching/go-org/org"
	"memo/pkg/logger"
	"memo/pkg/org/db"
)

// 分为两种操作，File 类中数据由文件查询加载， File.Data 由类由数据库查询加载。
// 数据库操作函数放在storage 中做为底层操作函数。
func NewFileFromPath(filePath string) (*File, error) {
	if filepath.Ext(filePath) != ".org" {
		return nil, logger.Errorf("The file %s is not a org file.", filePath)
	}
	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return nil, logger.Errorf("The file %s does not exist.", filePath)
	}
	if err != nil {
		return nil, logger.Errorf("The file %s stat failed: %v", filePath, err)
	}

	// Check if it is a regular file
	if !info.Mode().IsRegular() {
		return nil, logger.Errorf("The file %s is not a regular file.", filePath)
	}

	// Try to open the file for reading
	file, err := os.Open(filePath)
	if err != nil {
		return nil, logger.Errorf("Open file %s failed: %v", filePath, err)
	}
	defer file.Close()

	return &File{
		FilePath: filePath,
	}, nil
}

// store the meta info of the file from org file parse.
type MetaInfo struct {
	ID string
}

type File struct {
	// store the data for file from database.
	Data *db.File
	// store the data parse from file in disk.
	Content   string
	FilePath  string
	Hash      string
	Meta      *MetaInfo
	doc       *org.Document
	HeadCache *HeadlineCacheMap
}

// getHash returns the md5 hash of the file content
func (f *File) getHash() error {
	fileHandler, _ := os.Open(f.FilePath)
	defer fileHandler.Close()

	hash := md5.New()
	_, err := io.Copy(hash, fileHandler)
	if err != nil {
		return logger.Errorf("Copy file content to buffer failed: %v", err.Error())
	}
	f.Hash = hex.EncodeToString(hash.Sum(nil))
	return nil
}

// LoadContent reads the file content and stores it in the Content field
func (f *File) loadContent() error {
	content, err := os.ReadFile(f.FilePath)
	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.FilePath)
	}
	f.Content = string(content)
	return nil
}

// 解析获取Document
func (f *File) parseContent() error {
	if err := f.loadContent(); err != nil {
		return err
	}

	// Parse content, create an io.Reader from the byte slice first.
	c := []byte(f.Content)
	reader := bytes.NewReader(c)
	f.doc = org.New().Parse(reader, f.FilePath)
	if f.doc.Error != nil {
		return logger.Errorf("Parse file %s failed: %s", f.FilePath, f.doc.Error)
	}
	return nil
}

// 解析获取Document 的所有数据.
// TODO:  增加File 类型描述字段。
func (f *File) parseDocument() error {
	if err := f.parseContent(); err != nil {
		return err
	} else {
		f.Meta = getFileMeta(f.doc)
	}
	if f.Meta == nil {
		return logger.Errorf("File %s has no id Property.", f.FilePath)
	}
	sql := NewSqlWriter()
	sql.fileId = f.Meta.ID
	_, err := f.doc.Write(sql)
	if err != nil {
		return logger.Errorf("Parse Document use org writer failed: %v", err)
	}
	f.HeadCache, err = NewHeadlineCache(sql.Headlines, f.Meta.ID)
	if err != nil {
		return err
	}
	return nil
}

func (f *File) Load() error {
	if err := f.getHash(); err != nil {
		return err
	}
	if err := f.parseDocument(); err != nil {
		return err
	}
	filedb, err := db.LoadFileFromDB(f.Meta.ID)
	if err != nil {
		return err
	} else if filedb != nil {
		f.Data = filedb
	}
	return nil
}

// check if file not exist in db or changed after last update to db.
func (f *File) ifNeedUpdate() (bool, error) {
	// Check if the file content has changed
	if f.Data == nil {
		return false, logger.Errorf("File Data init error when load file.")
	}
	if f.Hash != f.Data.Hash {
		// The file content has changed
		f.Data.Hash = f.Hash
		logger.Infof("File %s content has changed, updating hash to %s.", f.FilePath, f.Hash)
		return true, nil
	} else {
		// The file content has not changed
		logger.Infof("File %s content has not changed.", f.FilePath)
		return false, nil
	}
}

func (f *File) UpdateFile(force bool) error {
	// Check if the file content has changed
	needUpdate, err := f.ifNeedUpdate()
	if err != nil {
		return err
	}
	if !needUpdate && !force {
		return nil
	} else {
		// create file record if id not exist.
		if f.Data.ID == "" {
			err = f.Data.Create(f.Meta.ID, f.Hash, f.FilePath, f.HeadCache.HeadlinesFileCache)
		} else {
			err = f.Data.Update(f.Meta.ID, f.Hash, f.FilePath, f.HeadCache.HeadlinesFileCache)
		}
		if err != nil {
			return err
		} else {
			return nil
		}
	}
}
