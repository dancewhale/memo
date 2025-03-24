package util

import (
	"memo/pkg/db"
	"memo/pkg/storage"
)

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
	var filePath string
	if headline == nil {
		return Headline{}
	}
	ifExpand, err := db.IfVirtualHeadExpandable(headline.ID)
	if err != nil {
		return Headline{}
	}
	file, err := db.GetFileByHeadlineID(headline.ID)
	if err != nil {
		return Headline{}
	}
	if file == nil {
		filePath = ""
	} else {
		filePath = file.FilePath
	}
	return Headline{
		ID: headline.ID, Weight: headline.Weight, Source: headline.Source, ScheduledType: headline.ScheduledType,
		Type: headline.Type, Title: headline.Title, Hash: headline.Hash, Content: headline.Content,
		ParentID: headline.ParentID, Level: headline.Level, Order: headline.Order, Status: headline.Status,
		Priority: headline.Priority, FileID: headline.FileID, FilePath: filePath, Expandable: ifExpand,
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
