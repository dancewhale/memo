package org

import (
	"bytes"
	"crypto/md5"
	"encoding/hex"
	"memo/pkg/storage"
	"memo/pkg/util"
	"os"
	"path/filepath"

	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/org/parser"

	"github.com/emirpasic/gods/lists/arraylist"
)

// NewFileFromPath creates a new OrgFile from a file path.
// It checks if the file exists, if it is a regular file, and if it is a .org file.
// If the file is a .org file, it reads the file content, calculates the hash.
func NewFileFromPath(filePath string) (*OrgFile, error) {
	orgfiledb, err := db.NewOrgFileDB()
	if err != nil {
		return nil, logger.Errorf("Init orgfile db operator error: %v", err)
	}
	if filepath.Ext(filePath) != ".org" {
		logger.Infof("The file %s is not a org fileHandler, skip this file.", filePath)
		return nil, nil
	}

	hashSting, err := util.HashFile(filePath)
	if err != nil {
		return nil, err
	}
	return &OrgFile{
		db:   orgfiledb,
		hash: hashSting,
		path: filePath,
	}, nil
}

func GetFileFromID(id string) (*OrgFile, error) {
	orgfiledb, err := db.NewOrgFileDB()
	if err != nil {
		return nil, logger.Errorf("Init orgfile db operator error: %v", err)
	}
	file, err := orgfiledb.GetFileByID(id)
	if err != nil {
		return nil, err
	}
	if file == nil {
		return nil, nil
	}
	return &OrgFile{
		db:   orgfiledb,
		hash: file.Hash,
		path: file.FilePath,
		file: file,
	}, nil
}

// 目标是用来，解析file 文件然后存储在kv 数据库中
// 提供接口用来加载文件，然后存储到kv数据库中
// 提供接口用于修改，然后存储到kv数据库中，同时写入文件
type OrgFile struct {
	db    *db.OrgFileDB
	path  string
	hash  string
	file  *storage.File
	Nodes *arraylist.List
}

func (f *OrgFile) String() string {
	return f.file.String()
}

func (f *OrgFile) parseDocument() error {
	content, err := os.ReadFile(f.path)
	reader := bytes.NewReader(content)

	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.path)
	}

	nodes := parser.New().Parse(reader, f.path)
	f.Nodes = nodes

	s := parser.NewSqlWriter()
	f.file = s.ParseOrgFile(f.Nodes)
	f.file.FilePath = f.path
	f.file.Hash = f.hash
	if f.file.ID == "" {
		return parser.MissFileID
	}
	return nil
}

// LoadFromFile parse file to File object.
// It first check if the file with same hash is already in the db,
// If exist it returns the file and ID from the db.
// If the file with same hash is not in the db, it parses the file content and returns the file.
// if force, it will always parse the file content even file hash not change.
func (f *OrgFile) LoadFromFile(force bool) error {
	file, err := f.db.GetFileByHash(f.hash)
	if err != nil {
		return logger.Errorf("Load file By hash from database %s failed: %v", f.file.Hash, err)
	}
	if file == nil || force {
		return f.parseDocument()
	} else {
		f.file = file
		f.path = file.FilePath
		f.hash = file.Hash
		return nil
	}
}

func (f *OrgFile) SaveToDiskFile(path string) error {
	if path == "" {
		path = f.file.FilePath
	}

	content := f.String()
	hash := md5.New()
	hash.Write([]byte(content))
	HashString := hex.EncodeToString(hash.Sum(nil))

	// 只有当内容发生变化时才写入文件
	if f.hash != HashString {
		// Open the file with write-only, create if not exists, and truncate it if it exists
		file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		if err != nil {
			return logger.Errorf("Failed to open file when save file: %w", err)
		}
		defer file.Close()

		// Write the content to the file
		_, err = file.WriteString(content)
		if err != nil {
			return logger.Errorf("failed to write to file: %w", err)
		}

		// 只有当文件路径相同时才更新数据库中的哈希值
		if f.file.FilePath == path {
			err = f.db.UpdateFileHash(f.file.ID, HashString)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (f *OrgFile) SaveDB(force bool) error {
	needUpdate, err := db.IfFileDBNeedUpdate(f.file.ID, f.file.Hash)
	if err != nil {
		return err
	}

	if needUpdate || force {
		HeadCache, err := NewHeadlineCache(f.file.Headlines, f.file.ID, f.file.FilePath, f.file.Hash)
		if err != nil {
			return err
		}

		err = HeadCache.UpdateHeadlineToDB(force)
		if err != nil {
			return err
		}
	}

	err = db.FileDBUpdate(f.file, force)
	if err != nil {
		return err
	}
	return nil
}
