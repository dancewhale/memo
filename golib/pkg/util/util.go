package util

import (
	"fmt"
	"github.com/maniartech/gotime"
	"memo/pkg/org/db"
	"memo/pkg/storage"
	"net"
	"time"
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

// get the start/end time after n day
func GetDayTime(n int64) (dayStart, dayEnd time.Time) {
	china, _ := time.LoadLocation("Asia/Shanghai")
	dueDay := time.Now().In(china).AddDate(0, 0, int(n))
	dayStart = gotime.SoD(dueDay)
	dayEnd = gotime.EoD(dueDay)
	return dayStart, dayEnd
}
