package orgp

import (
	"bytes"
	"crypto/md5"
	"encoding/gob"
	"encoding/hex"
	"fmt"
	"io"
	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
	"os"
	"path/filepath"
)

// prepare for struct to be stored in kv database.
// if not registered, kv database will not be able to store the struct.
func init() {
	gob.Register(OrgFile{})
	gob.Register(PropertyDrawer{})
	gob.Register(Headline{})
	gob.Register(Paragraph{})
	gob.Register(Text{})
	gob.Register(RegularLink{})
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
	Nodes []Node
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

	return NodesToString(f.Nodes...), nil
}

// Load check if the file with same hash is already in the cache, it returns the file from the cache.
// If the file with same hash is not in the cache, it parses the file content and returns the file.
func (f *OrgFile) Load() error {
	fileCache := kvInit(options.GetCacheDirPath())
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
		return nil
	}
}

// Save save orgfile to kv database.
func (f *OrgFile) Save() error {
	fileCache := kvInit(options.GetCacheDirPath())
	err := fileCache.Save(f)
	defer fileCache.Close()
	return err
}

func (f *OrgFile) parseDocument() error {
	content, err := os.ReadFile(f.Path)
	reader := bytes.NewReader(content)

	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.Path)
	}

	f.Nodes = New().Parse(reader, f.Path)
	f.ID, err = getFileID(f)
	if err != nil {
		return logger.Errorf("Parse Document and get id failed: %v", err)
	}
	return nil
}
