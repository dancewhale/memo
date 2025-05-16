package _import

import (
	"memo/cmd/options"
	"memo/pkg/client"
	"memo/pkg/org/parser"
	"os"
	"path/filepath"

	"memo/pkg/logger"
)

const MediaDir = "media"
const BookDir = "book"
const ArticleDir = "article"
const InboxDir = "inbox"

type File struct {
	ID            string
	srcFilePath   string
	dstFilePath   string
	rootDirectory string
}

func NewFile(srcFilePath string) *File {
	id := parser.GenerateID()
	return &File{
		ID:          id,
		srcFilePath: srcFilePath,
	}
}

// checkFileValid checks if a File exists and is not empty.
func (f *File) checkFileValid() error {
	fileInfo, err := os.Stat(f.srcFilePath)
	if os.IsNotExist(err) {
		return logger.Errorf("File %s does not exist", f.srcFilePath)
	}
	if err != nil {
		return logger.Errorf("error checking File %s: %w", f.srcFilePath, err)
	}

	if fileInfo.IsDir() {
		return logger.Errorf("%s is a directory, not a File", f.srcFilePath)
	}

	if fileInfo.Size() == 0 {
		return logger.Errorf("File %s is empty", f.srcFilePath)
	}

	return nil
}

func (f *File) ensureRootDirExist() error {
	con := options.ConfigInit()
	client.NewEmacsEpcClient(int(con.EmacsPort))
	rootDir, err := client.EClient.GetEmacsVar("memo-org-directory")
	if err != nil {
		logger.Errorf("Failed to get Memo org directory var: %v", err)
	}
	dirPaths := rootDir.([]interface{})
	dirPath := dirPaths[0].(string)
	// Check if the main directory exists, create if not.
	if _, err := os.Stat(dirPath); os.IsNotExist(err) {
		if err := os.MkdirAll(dirPath, 0755); err != nil {
			return logger.Errorf("failed to create directory %s: %w", dirPath, err)
		}
		logger.Infof("Directory %s created.", dirPath)
	} else if err != nil {
		return logger.Errorf("error checking directory %s: %w", dirPath, err)
	}

	subDirs := []string{MediaDir, BookDir, ArticleDir, InboxDir}
	for _, subDir := range subDirs {
		subDirPath := filepath.Join(dirPath, subDir)
		if _, err := os.Stat(subDirPath); os.IsNotExist(err) {
			if err := os.MkdirAll(subDirPath, 0755); err != nil {
				return logger.Errorf("failed to create subdirectory %s: %w", subDirPath, err)
			}
			logger.Infof("Subdirectory %s created.", subDirPath)
		} else if err != nil {
			return logger.Errorf("error checking subdirectory %s: %w", subDirPath, err)
		}
	}
	f.rootDirectory = dirPath
	return nil
}

func (f *File) getExportPath() string {
	f.dstFilePath = filepath.Join(f.rootDirectory, InboxDir)
	filename := filepath.Base(f.srcFilePath)
	//change ext of filename to .org
	ext := filepath.Ext(filename)
	filename = filename[0 : len(filename)-len(ext)]
	filename = filename + ".org"
	f.dstFilePath = filepath.Join(f.dstFilePath, filename)
	return f.dstFilePath
}

func Import(filePath string) (string, error) {
	file := NewFile(filePath)

	if err := file.checkFileValid(); err != nil {
		return "", logger.Errorf("Invalid File: %s, %v", file.srcFilePath, err)
	}

	if err := file.ensureRootDirExist(); err != nil {
		return "", logger.Errorf("Ensure memo org directory exist error: %v", err)
	}
	file.getExportPath()
	mediaOutputDir := filepath.Join(file.rootDirectory, MediaDir)
	exportFilePath, err := ConvertFileToOrgMode(file.ID, file.srcFilePath, file.dstFilePath, mediaOutputDir)
	if err != nil {
		return "", logger.Errorf("Convert file to org mode error: %v", err)
	}
	err = processOrgFile(exportFilePath, file.ID)
	if err != nil {
		return "", logger.Errorf("Convert file to org mode error: %v", err)
	}

	return file.dstFilePath, nil
}
