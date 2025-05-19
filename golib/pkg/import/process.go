package _import

import (
	"bytes"
	"fmt"
	"memo/pkg/org/parser"
	"os"
	"path/filepath"
	"strings" // New

	"memo/pkg/logger"
	"memo/pkg/storage"
)

// GenerateFileUUID reads the file at f.srcFilePath, calculates its MD5 hash,
// and returns a UUID string derived from the hash.
// The UUID format is uppercase with hyphens (e.g., XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX).
func (f *File) GenerateFileUUID() (string, error) {
	if f.srcFilePath == "" {
		return "", logger.Errorf("srcFilePath is not set for the file object")
	}

	content, err := os.ReadFile(f.srcFilePath)
	if err != nil {
		return "", logger.Errorf("failed to read file %s: %v", f.srcFilePath, err)
	}

	return generateUUIDFromContent(content)
}

// VerifyFileUUID checks if the given uuidToCheck matches the UUID generated
// from the content of the file at f.srcFilePath.
func (f *File) VerifyFileUUID(uuidToCheck string) (bool, error) {
	generatedUUID, err := f.GenerateFileUUID()
	if err != nil {
		return false, err
	}
	return strings.ToUpper(uuidToCheck) == generatedUUID, nil
}

func (f *File) getFileIDString() string {
	if f.ID != "" {
		idString := ":PROPERTIES:\n" + ":ID:       " + f.ID + "\n" + ":END:\n"
		return idString
	}
	return ""
}

func (f *File) ensureFileID() error {
	if f.ID == "" {
		uuid, err := f.GenerateFileUUID()
		if err != nil {
			return logger.Errorf("Generate file ID failed: %v", err)
		}
		f.ID = uuid
		return nil
	}
	return nil
}

// parserImportBookOrgFile parses the org file
// Change the head start from level 2 and under the new level 1
func (f *File) parserImportArticleOrgFile() (*storage.File, error) {
	content, err := os.ReadFile(f.tempFilePath)
	reader := bytes.NewReader(content)

	if err != nil {
		return nil, logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return nil, logger.Errorf("File %s is empty.", f.tempFilePath)
	}

	nodes := parser.New().Parse(reader, f.tempFilePath)

	s := parser.NewSqlWriter(storage.NormalFile)
	file := s.ParseOrgFile(nodes)

	if len(file.MetaContent) == 0 {
		// Append the id to the start of file
		err = insertToFileStart(f.tempFilePath, f.getFileIDString())
		if err != nil {
			return nil, logger.Errorf("Insert ID string to file failed: %v", err)
		}
	} else {
		headString := fmt.Sprintf("* %s\n", GetLeadingChars(file.MetaContent, 20))
		err = insertToFileStart(f.tempFilePath, f.getFileIDString(), headString)
		if err != nil {
			return nil, logger.Errorf("Insert ID string and new head title to file failed: %v", err)
		}
	}

	// read the file content again, after insert id and head
	content, err = os.ReadFile(f.tempFilePath)
	reader = bytes.NewReader(content)
	if err != nil {
		return nil, logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return nil, logger.Errorf("File %s is empty.", f.tempFilePath)
	}

	nodes = parser.New().Parse(reader, f.tempFilePath)

	s = parser.NewSqlWriter(storage.NormalFile)
	file = s.ParseOrgFile(nodes)

	file.ID = f.ID
	file.EnsureOnlyOneRootHead()
	return file, nil
}

// 需要处理一些什么？
// 临时文件，从中获取对应的hash对应的uuid
// 如果file 是ifBook()是true，则移动到对应的rootDirectory
// 如果file 是ifBook()是false，则首先获取对应的exportPath，检查是否存在文件
// 如果存在文件，
func (f *File) processTempOrgFile() error {
	if f.ifBook() {
		return f.processTempBookOrgFile()
	} else {
		return f.processTempArticleOrgFile()
	}

}

// Append the ID string to the head of the file
// and move the file to the destination directory.
func (f *File) processTempBookOrgFile() error {
	mediaTempDir := f.getTempMediaRootDir()
	err := f.ensureFileID()
	if err != nil {
		return err
	}
	// Append the id to the start of file
	err = insertToFileStart(f.tempFilePath, f.getFileIDString())
	if err != nil {
		return logger.Errorf("Insert ID string to file failed: %v", err)
	}

	// Copy the file to the destination directory
	err = os.Rename(f.tempFilePath, f.dstFilePath)
	if err != nil {
		return logger.Errorf("Failed to move file %s to %s: %v", f.tempFilePath, f.dstFilePath, err)
	}
	// Move the media directory to the destination directory
	mediaDir := filepath.Join(f.rootDirectory, MediaDir, f.ID)
	err = os.Rename(mediaTempDir, mediaDir)
	if err != nil {
		return logger.Errorf("Failed to move file %s to %s: %v", mediaTempDir, mediaDir, err)
	}
	return nil
}

func (f *File) processTempArticleOrgFile() error {
	err := f.ensureFileID()
	if err != nil {
		return err
	}

	orgFile, err := f.parserImportArticleOrgFile()
	if err != nil {
		return logger.Errorf("Failed to parse org file: %s, %v", f.tempFilePath, err)
	}
	if orgFile.Headlines == nil {
		return logger.Errorf("The converted temp org file %s has no headlines", f.tempFilePath)
	}
	// check if f.dstFilePath exist
	if _, err := os.Stat(f.dstFilePath); err == nil {
		err = f.mergeOrgHeadline(f.dstFilePath, orgFile.Headlines[0])
		if err != nil {
			return logger.Errorf("Failed to merge org file: %s, %v", f.dstFilePath, err)
		}
		return nil
	}
	// if not exist, create the dstFilePath
	err = os.WriteFile(f.dstFilePath, []byte(orgFile.String()), 0644)
	if err != nil {
		return logger.Errorf("Failed to write org file: %s, %v", f.dstFilePath, err)
	}

	return nil
}

func (f *File) mergeOrgHeadline(dstFilePath string, orgHeadlien storage.Headline) error {
	if orgHeadlien.Level != 1 {
		return logger.Errorf("Org Headline in file %s is not root headline", f.tempFilePath)
	}
	content, err := os.ReadFile(dstFilePath)

	if err != nil {
		return logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return logger.Errorf("File %s is empty.", f.tempFilePath)
	}
	err = insertToFileEnd(dstFilePath, orgHeadlien.String())
	if err != nil {
		return logger.Errorf("Merge new import file content failed: %v", err)
	}

	return nil
}
