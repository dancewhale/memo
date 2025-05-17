package _import

import (
	"memo/pkg/logger"
	"memo/pkg/org/parser"
	"path/filepath"
	"strings"
	"time"
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
	fileExt       string
	tempFilePath  string
	tempDirectory string
}

func NewFile(srcFilePath string) (*File, error) {
	if err := checkFileValid(srcFilePath); err != nil {
		return nil, logger.Errorf("Invalid File: %s, %v", srcFilePath, err)
	}

	rootDir, err := ensureRootDirExist()
	if err != nil {
		return nil, logger.Errorf("Ensure memo org directory exist error: %v", err)
	}
	tempDir, err := ensureTempDirExist()
	if err != nil {
		return nil, logger.Errorf("Ensure rootDirectory directory exist error: %v", err)
	}

	id := parser.GenerateID()
	file := &File{
		ID:            id,
		srcFilePath:   srcFilePath,
		rootDirectory: rootDir,
		tempDirectory: tempDir,
		fileExt:       strings.ToLower(filepath.Ext(srcFilePath)),
	}
	file.getExportPath()
	file.getTempExportPath()
	return file, nil
}

func (f *File) ifBook() bool {
	ext := filepath.Ext(f.srcFilePath)
	if ext == ".epub" || ext == ".pdf" || ext == ".mobi" {
		return true
	}
	return false
}

func (f *File) getTempMediaDir() string {
	mediaOutputDir := filepath.Join(f.tempDirectory, MediaDir)
	return mediaOutputDir
}

func (f *File) getMediaDir() string {
	mediaOutputDir := filepath.Join(f.rootDirectory, MediaDir)
	return mediaOutputDir
}

func (f *File) getExportPath() string {
	if f.dstFilePath == "" {
		f.dstFilePath = filepath.Join(f.rootDirectory, InboxDir)
		filename := filepath.Base(f.srcFilePath)
		//change ext of filename to .org
		ext := filepath.Ext(filename)
		if f.ifBook() {
			filename = filename[0 : len(filename)-len(ext)]
			filename = filename + ".org"
			f.dstFilePath = filepath.Join(f.dstFilePath, filename)
		} else if !f.ifBook() {
			// filename = today + "_" + filename
			todayString := time.Now().Format("2006-01-02")
			filename = todayString + "_" + "article" + ".org"
			f.dstFilePath = filepath.Join(f.dstFilePath, filename)
		}
	}
	return f.dstFilePath
}

func (f *File) getTempExportPath() string {
	filename := filepath.Base(f.srcFilePath)
	//change ext of filename to .org
	ext := filepath.Ext(filename)
	filename = filename[0 : len(filename)-len(ext)]
	filename = filename + ".org"
	return filepath.Join(f.tempDirectory, filename)
}

func Import(filePath string) (string, error) {
	file, err := NewFile(filePath)
	if err != nil {
		return "", logger.Errorf("New file error: %v", err)
	}

	exportFilePath, err := file.ConvertFileToOrgMode()
	if err != nil {
		return "", logger.Errorf("Convert file to org mode error: %v", err)
	}
	err = processOrgFile(exportFilePath, file.ID)
	if err != nil {
		return "", logger.Errorf("Convert file to org mode error: %v", err)
	}

	return file.dstFilePath, nil
}
