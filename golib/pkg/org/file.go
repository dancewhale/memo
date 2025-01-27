package org

import (
	"bytes"
	"crypto/md5"
	"encoding/gob"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
	"memo/pkg/org/parser"
)

// prepare for struct to be stored in kv database.
// if not registered, kv database will not be able to store the struct.
func init() {
	gob.Register(OrgFile{})
	gob.Register(parser.PropertyDrawer{})
	gob.Register(parser.Headline{})
	gob.Register(parser.Paragraph{})
	gob.Register(parser.Text{})
	gob.Register(parser.RegularLink{})
}

// NewFileFromPath creates a new OrgFile from a file path.
// It checks if the file exists, if it is a regular file, and if it is a .org file.
// If the file is a .org file, it reads the file content, calculates the hash.
func NewFileFromPath(filePath string) (*OrgFile, error) {
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
	return &OrgFile{
		Path: filePath,
		Hash: hashSting,
	}, nil
}

// 目标是用来，解析file 文件然后存储在kv 数据库中
// 提供接口用来加载文件，然后存储到kv数据库中
// 提供接口用于修改，然后存储到kv数据库中，同时写入文件
type OrgFile struct {
	ID    string
	Path  string
	Hash  string
	Nodes []parser.Node
}

func (f *OrgFile) getFileID(d *OrgFile) (string, error) {
	var ID string
	if len(d.Nodes) != 0 {
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case parser.Headline:
				break
			case parser.PropertyDrawer:
				id, exist := n.Get(options.GetPropertyID())
				if exist && ID != "" {
					return "", parser.FoundDupID
				} else if exist {
					ID = id
				}
			}
		}
	}
	if ID == "" {
		logger.Warnf("No FileID found in file %s.", d.Path)
		return "", parser.MissFileID
	} else {
		return ID, nil
	}
}

func (f *OrgFile) String() (out string, err error) {
	var er error
	defer func() {
		if recovered := recover(); recovered != nil {
			er = fmt.Errorf("could not write output: %s", recovered)
		}
	}()

	if er != nil {
		return "", er
	} else if f.Nodes == nil {
		return "", fmt.Errorf("File is empty.")
	} else if f.Path == "" {
		return "", fmt.Errorf("File path is empty.")
	}

	return parser.NodesToString(f.Nodes...), nil
}

// LoadFromFile parse file to File object.
// It first check if the file with same hash is already in the cache,
// If exist it returns the Nodes and ID from the cache.
// If the file with same hash is not in the cache, it parses the file content and returns the file.
func (f *OrgFile) LoadFromFile() error {
	fileCache := KvInit(options.GetCacheDirPath())
	file, err := fileCache.LoadFromHash(f.Hash)
	defer fileCache.Close()
	if err != nil {
		return logger.Errorf("Load file By hash from cache %s failed: %v", f.Hash, err)
	}
	if file == nil {
		return f.parseDocument()
	} else {
		f.Nodes = file.Nodes
		f.ID = file.ID
		return parser.FileExistInKv
	}
}

// SaveToKvDB save orgfile to kv database.
func (f *OrgFile) SaveToKvDB() error {
	fileCache := KvInit(options.GetCacheDirPath())
	err := fileCache.Save(f)
	defer fileCache.Close()
	return err
}

func (f *OrgFile) SaveToSqlDB() error {
	w := parser.NewSqlWriter(f.ID)
	headlines := w.ParseNodes(f.Nodes...)

	HeadCache, err := NewHeadlineCache(headlines, f.ID, f.Path, f.Hash)
	if err != nil {
		return err
	}

	return HeadCache.UpdateHeadlineToDB(false)
}

func (f *OrgFile) SaveToDiskFile() error {
	// Open the file with write-only, create if not exists, and truncate it if it exists
	file, err := os.OpenFile(f.Path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		return fmt.Errorf("Failed to open file when save file: %w", err)
	}
	defer file.Close()

	content, err := f.String()
	if err != nil {
		return fmt.Errorf("Failed to get file content: %w", err)
	}
	// Write the content to the file
	_, err = file.WriteString(content + "\n")
	if err != nil {
		return fmt.Errorf("failed to write to file: %w", err)
	}
	return nil
}

func (f *OrgFile) parseDocument() error {
	content, err := os.ReadFile(f.Path)
	reader := bytes.NewReader(content)

	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.Path)
	}

	f.Nodes = parser.New().Parse(reader, f.Path)
	f.ID, err = f.getFileID(f)
	if err != nil {
		return logger.Errorf("Parse Document and get id failed: %v", err)
	}
	return nil
}
