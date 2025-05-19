package _import

import (
	"crypto/md5"
	"fmt"
	"memo/cmd/options"
	"memo/pkg/client"
	"os"
	"path/filepath"
	"strings"

	"memo/pkg/logger"
)

// GetLeadingChars processes a string to remove leading unwanted characters
// and returns the first 'num' characters of the result.
// Unwanted leading characters are: space, tab, '-', '/'.
func GetLeadingChars(s string, num int) string {
	if num <= 0 {
		return ""
	}

	startIndex := 0
	runes := []rune(s) // Convert to runes for correct character indexing

	for i, r := range runes {
		if r == ' ' || r == '\t' || r == '-' || r == '/' {
			// Continue skipping if it's one of the specified characters
			// This loop structure was slightly off in thought, correcting here:
			// We need to find the *first* character that is NOT one of these.
		} else {
			startIndex = i
			break // Found the first valid character
		}
		// If loop finishes, all characters were skippable
		if i == len(runes)-1 && (r == ' ' || r == '\t' || r == '-' || r == '/') {
			startIndex = len(runes)
		}
	}

	// If all characters were trimmed or string was empty initially
	if startIndex >= len(runes) {
		return ""
	}

	trimmedRunes := runes[startIndex:]

	if num >= len(trimmedRunes) {
		return string(trimmedRunes)
	}

	return string(trimmedRunes[:num])
}

// removeDirIfEmpty checks if a directory is empty and removes it if it is.
// It returns an error for unexpected issues during check or removal.
func removeDirIfEmpty(dirPath string) error {
	entries, err := os.ReadDir(dirPath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil // Not an error if dir doesn't exist
		}
		return logger.Errorf("failed to read directory %s for cleanup: %v", dirPath, err)
	}

	if len(entries) == 0 {
		err = os.RemoveAll(dirPath)
		if err != nil {
			return logger.Errorf("failed to remove empty directory %s: %v", dirPath, err)
		}
	}
	return nil
}

// checkFileValid checks if a File exists and is not empty.
func checkFileValid(srcFilePath string) error {
	fileInfo, err := os.Stat(srcFilePath)
	if os.IsNotExist(err) {
		return logger.Errorf("File %s does not exist", srcFilePath)
	}
	if err != nil {
		return logger.Errorf("error checking File %s: %w", srcFilePath, err)
	}

	if fileInfo.IsDir() {
		return logger.Errorf("%s is a directory, not a File", srcFilePath)
	}

	if fileInfo.Size() == 0 {
		return logger.Errorf("File %s is empty", srcFilePath)
	}

	return nil
}

func ensureRootDirExist() (string, error) {
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
			return "", logger.Errorf("failed to create directory %s: %w", dirPath, err)
		}
		logger.Infof("Directory %s created.", dirPath)
	} else if err != nil {
		return "", logger.Errorf("error checking directory %s: %w", dirPath, err)
	}

	subDirs := []string{MediaDir, BookDir, ArticleDir}
	for _, subDir := range subDirs {
		subDirPath := filepath.Join(dirPath, subDir)
		if _, err := os.Stat(subDirPath); os.IsNotExist(err) {
			if err := os.MkdirAll(subDirPath, 0755); err != nil {
				return "", logger.Errorf("failed to create subdirectory %s: %w", subDirPath, err)
			}
			logger.Infof("Subdirectory %s created.", subDirPath)
		} else if err != nil {
			return "", logger.Errorf("error checking subdirectory %s: %w", subDirPath, err)
		}
	}
	return dirPath, nil
}

func ensureTempDirExist() (string, error) {
	tmpDirPath := filepath.Join("/tmp/", "memo")
	if _, err := os.Stat(tmpDirPath); os.IsNotExist(err) {
		if err := os.MkdirAll(tmpDirPath, 0755); err != nil {
			return "", logger.Errorf("failed to create directory %s: %v", tmpDirPath, err)
		}
		logger.Infof("Directory %s created.", tmpDirPath)
	} else if err != nil {
		return "", logger.Errorf("error checking directory %s: %v", tmpDirPath, err)
	}
	return tmpDirPath, nil
}

// generateUUIDFromContent calculates MD5 of content and formats it as a UUID string.
// The UUID format is uppercase with hyphens (e.g., XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX).
func generateUUIDFromContent(content []byte) (string, error) {
	hasher := md5.New()
	if _, err := hasher.Write(content); err != nil {
		return "", fmt.Errorf("failed to write content to hasher: %w", err)
	}
	hashBytes := hasher.Sum(nil)

	uuidStr := fmt.Sprintf("%X-%X-%X-%X-%X",
		hashBytes[0:4],
		hashBytes[4:6],
		hashBytes[6:8],
		hashBytes[8:10],
		hashBytes[10:16])

	return strings.ToUpper(uuidStr), nil
}

func insertToFileStart(filePath string, contents ...string) error {
	// 读取原文件内容
	oldData, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}

	// 打开原文件用于写入（会清空原内容）
	file, err := os.OpenFile(filePath, os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	// 先写入新内容
	for _, content := range contents {
		if _, err := file.WriteString(content); err != nil {
			return err
		}
	}
	// 再写入原内容
	if _, err := file.Write(oldData); err != nil {
		return err
	}
	return nil
}

func insertToFileEnd(filePath, content string) error {
	// 读取原文件内容
	oldData, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}

	// 打开原文件用于写入（会清空原内容）
	file, err := os.OpenFile(filePath, os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	// 先写入原内容
	if _, err := file.Write(oldData); err != nil {
		return err
	}
	// 再写入新内容
	if _, err := file.WriteString(content); err != nil {
		return err
	}
	return nil
}
