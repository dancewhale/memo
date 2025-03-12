package util

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/storage"
	"net"
	"os"
)

type Result struct {
	Data interface{}
	Err  error
}

func QueryFreePort() (int64, error) {
	s, err := net.Listen("tcp", ":0")
	if err != nil {
		return -1, fmt.Errorf("could not listen TCP port 0: %v", err)
	}
	defer s.Close()
	tcpa, _ := s.Addr().(*net.TCPAddr)
	return int64(tcpa.Port), nil
}

type Headline struct {
	ID            string  `json:"id"`
	Weight        int64   `json:"weight"`
	Source        string  `json:"source"`
	ScheduledType string  `json:"scheduled_type"`
	Type          int     `json:"type"`
	Title         string  `json:"title"`
	Hash          string  `json:"hash" hash:"ignore"`
	Content       string  `json:"content"`
	ParentID      *string `json:"parent_id"`
	Level         int     `json:"level"`
	Order         int     `json:"order"`
	Status        string  `json:"status"`
	Priority      string  `json:"priority"`
	FileID        *string `json:"fileID"`
	FilePath      string  `json:"file_path"`
	Expandable    int     `json:"expandable"`
}

// change
func GetHeadStruct(headline *storage.Headline, db *db.OrgHeadlineDB) Headline {
	if headline == nil {
		return Headline{}
	}
	ifExpand, err := db.IfVirtualHeadExpandable(headline.ID)
	if err != nil {
		return Headline{}
	}
	return Headline{
		ID: headline.ID, Weight: headline.Weight, Source: headline.Source, ScheduledType: headline.ScheduledType,
		Type: headline.Type, Title: headline.Title, Hash: headline.Hash, Content: headline.Content,
		ParentID: headline.ParentID, Level: headline.Level, Order: headline.Order, Status: headline.Status,
		Priority: headline.Priority, FileID: headline.FileID, FilePath: headline.File.FilePath, Expandable: ifExpand,
	}

}

func GetHeadStructs(headline []*storage.Headline, db *db.OrgHeadlineDB) []Headline {
	if headline == nil {
		return nil
	}
	var heads []Headline
	for _, head := range headline {
		if head == nil {
			continue
		}
		heads = append(heads, GetHeadStruct(head, db))
	}
	return heads
}

func HashFile(filePath string) (string, error) {
	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		logger.Errorf("The file %s does not exist.", filePath)
	}
	if err != nil {
		return "", logger.Errorf("The file %s stat failed: %v", filePath, err)
	}

	// Check if it is a regular file
	if !info.Mode().IsRegular() {
		return "", logger.Errorf("The fileHandler %s is not a regular file.", filePath)
	}
	// Try to open the file for reading
	fileHandler, err := os.Open(filePath)
	if err != nil {
		return "", logger.Errorf("Open file %s failed: %v", filePath, err)
	}
	defer fileHandler.Close()

	hash := md5.New()
	_, err = io.Copy(hash, fileHandler)
	if err != nil {
		return "", logger.Errorf("Copy file content to buffer failed: %v", err.Error())
	}
	hashSting := hex.EncodeToString(hash.Sum(nil))
	return hashSting, nil
}

func HashContent(content string) string {
	hash := md5.New()
	hash.Write([]byte(content))
	return hex.EncodeToString(hash.Sum(nil))
}
