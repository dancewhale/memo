package org

import (
	"bytes"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
	"memo/pkg/storage"
	"os"
	"path/filepath"

	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/org/parser"
	"memo/pkg/util/gods/lists/arraylist"
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
	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return nil, logger.Errorf("The file %s does not exist.", filePath)
	}
	if err != nil {
		return nil, logger.Errorf("The file %s stat failed: %v", filePath, err)
	}

	// Check if it is a regular file
	if !info.Mode().IsRegular() {
		return nil, logger.Errorf("The fileHandler %s is not a regular file.", filePath)
	}
	// Try to open the file for reading
	fileHandler, err := os.Open(filePath)
	if err != nil {
		return nil, logger.Errorf("Open file %s failed: %v", filePath, err)
	}
	defer fileHandler.Close()

	hash := md5.New()
	_, err = io.Copy(hash, fileHandler)
	if err != nil {
		return nil, logger.Errorf("Copy file content to buffer failed: %v", err.Error())
	}
	hashSting := hex.EncodeToString(hash.Sum(nil))
	file := &storage.File{Hash: hashSting, FilePath: filePath}
	return &OrgFile{
		OrgFileDB: orgfiledb,
		file:      file,
	}, nil
}

// 目标是用来，解析file 文件然后存储在kv 数据库中
// 提供接口用来加载文件，然后存储到kv数据库中
// 提供接口用于修改，然后存储到kv数据库中，同时写入文件
type OrgFile struct {
	*db.OrgFileDB
	file  *storage.File
	Nodes *arraylist.List
}

func (f *OrgFile) getFileID(d *OrgFile) (string, error) {
	var ID string
	if d.Nodes != nil && d.Nodes.Size() != 0 {
		it := d.Nodes.Iterator()
		for it.Next() {
			node := it.Value()
			switch n := node.(type) {
			case parser.Headline:
				break
			case parser.PropertyDrawer:
				id, exist := n.Get(options.EmacsPropertyID)
				if exist && ID != "" {
					return "", parser.FoundDupID
				} else if exist {
					ID = id
				}
			}
		}
	}
	if ID == "" {
		logger.Warnf("No FileID found in file %s.", d.file.FilePath)
		return "", parser.MissFileID
	} else {
		return ID, nil
	}
}

func (f *OrgFile) String() (out string, err error) {
	var er error
	defer func() {
		if recovered := recover(); recovered != nil {
			er = logger.Errorf("Could not export file, Because error happend: %s", recovered)
		}
	}()

	if er != nil {
		return "", er
	} else if f.Nodes == nil {
		return "", fmt.Errorf("File is empty.")
	} else if f.file.FilePath == "" {
		return "", fmt.Errorf("File path is empty.")
	}

	return parser.NodesToString(f.Nodes), nil
}

// LoadFromFile parse file to File object.
// It first check if the file with same hash is already in the cache,
// If exist it returns the Nodes and ID from the cache.
// If the file with same hash is not in the cache, it parses the file content and returns the file.
// if force, it will always parse the file content even file hash not change.
func (f *OrgFile) LoadFromFile(force bool) error {
	con := options.ConfigInit()
	fileCache := KvInit(con.GetDBPath())

	file, err := fileCache.LoadFromHash(f.file.Hash)
	defer fileCache.Close()
	if err != nil {
		return logger.Errorf("Load file By hash from kvcache %s failed: %v", f.file.Hash, err)
	}
	if file == nil || force {
		return f.parseDocument()
	} else {
		f.Nodes = file.Nodes
		f.file.ID = file.file.ID
		return nil
	}
}

// SaveToKvDB save orgfile to kv database.
func (f *OrgFile) SaveToKvDB(force bool) error {
	con := options.ConfigInit()
	fileCache := KvInit(con.GetDBPath())

	err := fileCache.Save(f, force)
	defer fileCache.Close()
	return err
}

func (f *OrgFile) SaveToSqlDB(force bool) error {
	w := parser.NewSqlWriter(f.file.ID)
	headlines := w.ParseHeadlineNodes(f.Nodes)

	HeadCache, err := NewHeadlineCache(headlines, f.file.ID, f.file.FilePath, f.file.Hash)
	if err != nil {
		return err
	}

	return HeadCache.UpdateHeadlineToDB(force)
}

func (f *OrgFile) SaveToDiskFile(path string) error {
	// Open the file with write-only, create if not exists, and truncate it if it exists
	file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("Failed to open file when save file: %w", err)
	}
	defer file.Close()

	content, err := f.String()
	if err != nil {
		return fmt.Errorf("Failed to get file content: %w", err)
	}
	// Write the content to the file
	_, err = file.WriteString(content)
	if err != nil {
		return fmt.Errorf("failed to write to file: %w", err)
	}
	hash := md5.New()
	hash.Write([]byte(content))
	f.file.Hash = hex.EncodeToString(hash.Sum(nil))
	f.file.FilePath = path
	return nil
}

func (f *OrgFile) SaveDB(force bool) error {
	err := f.SaveToKvDB(force)
	if err != nil {
		return err
	}
	needUpdate, err := db.IfFileDBNeedUpdate(f.file.ID, f.file.Hash)
	if err != nil {
		return err
	}

	err = db.FileDBUpdate(f.file.ID, f.file.FilePath, f.file.Hash)
	if err != nil {
		return err
	}

	if needUpdate || force {
		err = f.SaveToSqlDB(force)
		if err != nil {
			return err
		}
	}
	return nil
}

func (f *OrgFile) parseDocument() error {
	content, err := os.ReadFile(f.file.FilePath)
	reader := bytes.NewReader(content)

	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.file.FilePath)
	}

	nodes := parser.New().Parse(reader, f.file.FilePath)
	f.Nodes = nodes

	// change Nodes to headlines.

	return nil
}

func (f *OrgFile) parseString(content string) (*arraylist.List, error) {
	reader := bytes.NewReader([]byte(content))
	nodes := parser.New().Parse(reader, f.file.FilePath)
	return nodes, nil
}

func (f *OrgFile) GetHeadlineByID(id string) (*arraylist.List, int) {
	list, index := parser.FindHeadByID(f.Nodes, id)
	if list == nil {
		return nil, -1
	}
	return list, index
}
