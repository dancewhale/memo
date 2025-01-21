package orgp

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
	"memo/pkg/logger"
	"os"
	"path/filepath"
)

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

// 从文件中加载数据，之后检查kv 数据库中是否存在相同hash 的数据，如果存直接从kv 中进行加载
// 如果不存在，解析文件为Nodes
func Load() error {
	return nil
}

// 检查OrgFile 生成的文件的hash 是否有变化，有变化首先保存到kv 中
// 之后解析为数据库格式数据，保存到数据库中
func Save() error {
	return nil
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
