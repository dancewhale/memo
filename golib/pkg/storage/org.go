package storage

import (
	"fmt"
	"os"
	"strings"
	"time"

	"memo/cmd/options"
	"memo/pkg/logger"

	"gorm.io/gorm"
)

const DefaultWeight = 50

const SUSPEND string = "suspend"
const NORMAL string = "normal"
const DELTED string = "deleted"

const NormalFile int = 1
const VirtualFile int = 2

type File struct {
	ID          string `gorm:"primaryKey;index;not null"`
	CreatedAt   time.Time
	UpdatedAt   time.Time
	DeletedAt   gorm.DeletedAt
	FilePath    string `gorm:"primaryKey;not null"`
	Hash        string
	MetaContent string
	Headlines   []Headline
}

type Headline struct {
	ID        string         `gorm:"primarykey;index;not null"`
	CreatedAt time.Time      `hash:"ignore"`
	UpdatedAt time.Time      `hash:"ignore"`
	DeletedAt gorm.DeletedAt `gorm:"index" hash:"ignore"`
	Weight    int64          `json:"weight"`
	Source    string         `json:"source"`
	// Scheduled Type
	// Suspend: hang up card, not review until set to normal.
	// Normal: normal schedule.
	ScheduledType string     `json:"scheduled_type"`
	Title         string     `json:"title"`
	Hash          string     `json:"hash" hash:"ignore"`
	Content       string     `json:"content"`
	ParentID      *string    `json:"parent_id"`
	Level         int        `json:"level"`
	Order         int        `json:"order"`
	Status        string     `json:"status"`
	Scheduled     *time.Time `json:"scheduled"`
	Deadline      *time.Time `json:"deadline"`
	Closed        *time.Time `json:"closed"`
	Priority      string     `json:"priority"`
	Properties    []Property `gorm:"foreignKey:HeadlineID;references:ID" json:"properties"`
	Children      []Headline `gorm:"foreignKey:ParentID" json:"children" hash:"ignore"`
	FileID        *string    `gorm:"primaryKey"`
	HeadlineID    *string    `json:"headline_id"` // which file headline that virtual head attach to
	VirtChildren  []Headline `gorm:"foreignKey:HeadlineID" json:"virt_children" hash:"ignore"`
	VirtFileHash  *string    `json:"virt_file_hash" hash:"ignore"`
	File          File       `gorm:"foreignKey:FileID;references:ID" json:"file" hash:"ignore"`
	LogBook       []*Clock   `gorm:"foreignKey:HeadlineID;references:ID" json:"logbook"`
	Tags          []Tag      `gorm:"foreignKey:HeadlineID;references:ID" json:"tags"`
}

type Property struct {
	HeadlineID string   `gorm:"primarykey;index;not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Key        string   `gorm:"primarykey;not null"`
	Value      string   `gorm:"not null"`
}

type Clock struct {
	ID         uint     `gorm:"primarykey;index;not null" hash:"ignore"`
	HeadlineID string   `gorm:"not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Start      *time.Time
	End        *time.Time
}

type Tag struct {
	HeadlineID string   `gorm:"primarykey;index;not null"`
	Headline   Headline `gorm:"foreignKey:HeadlineID;references:ID" json:"headline"`
	Name       string   `gorm:"primarykey;not null"`
}

type Annotation struct {
	ID               uint   `gorm:"primarykey;index;not null" hash:"ignore"`
	Start            uint   `gorm:"not null"`
	End              uint   `gorm:"not null"`
	ParentHeadlineID string `gorm:"not null"`
	ChildHeadlineID  string
	CommentText      string
	Face             string
	Type             uint
}

//--------------------------------------------------
// output for sql struct.

func (c Clock) String() string {
	start := "nil"
	end := "nil"
	if c.Start != nil {
		start = c.Start.Format("2006-01-02 15:04:05")
	}
	if c.End != nil {
		end = c.End.Format("2006-01-02 15:04:05")
	}
	return fmt.Sprintf("Clock{Start: %s, End: %s}", start, end)
}

type Content struct {
	strings.Builder
}

func (f *File) String() string {
	file := &Content{}
	file.WriteString(f.MetaContent)

	for _, h := range f.Headlines {
		h.Write(file)
	}
	return file.String()
}

func (f *File) Write() error {
	file, err := os.OpenFile(f.FilePath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		return logger.Errorf("Failed to open file when save file: %w", err)
	}
	defer file.Close()

	// Write the content to the file
	_, err = file.WriteString(f.String())
	if err != nil {
		return logger.Errorf("failed to write to file %s: %v", f.FilePath, err)
	}
	return nil
}

func (f *File) EnsureOnlyOneRootHead() {
	if len(f.Headlines) == 0 {
		return
	} else if len(f.Headlines) == 1 {
		f.Headlines[0].AdjustLevel(1)
		return
	} else {
		rootHead := Headline{Title: f.Headlines[0].Title, Level: 1}
		Headlines := []Headline{rootHead}
		for _, h := range f.Headlines {
			h.AdjustLevel(2)
			Headlines = append(Headlines, h)
		}
		f.Headlines = Headlines
		return
	}
}

func (h *Headline) taskTimeString() string {
	var content string
	if h.Closed != nil {
		closedTime := "CLOSED: [" + h.Closed.Format("2006-01-02 Mon 15:04") + "]"
		content += closedTime
	}
	if h.Deadline != nil {
		if content != "" {
			content += " "
		}
		deadTime := "DEADLINE: [" + h.Deadline.Format("2006-01-02 Mon") + "]"
		content += deadTime
	}
	if h.Scheduled != nil {
		if content != "" {
			content += " "
		}
		schTime := "SCHEDULED: [" + h.Scheduled.Format("2006-01-02 Mon") + "]"
		content += schTime
	}
	if content != "" {
		content += "\n"
	}
	return content
}

func (h *Headline) propertiesString() string {
	var content string
	content = ":PROPERTIES:\n"
	content += fmt.Sprintf("%-10s %s", ":"+options.EmacsPropertyID+":", h.ID) + "\n"
	if h.Source != "" {
		content += fmt.Sprintf("%-10s %s", ":"+options.EmacsPropertySource+":", h.Source) + "\n"
	}
	content += fmt.Sprintf("%-10s %d", ":"+options.EmacsPropertyWeight+":", h.Weight) + "\n"
	content += fmt.Sprintf("%-10s %s", ":"+options.EmacsPropertySchedule+":", h.ScheduledType) + "\n"
	if h.Properties != nil {
		for _, kv := range h.Properties {
			key := ":" + kv.Key + ":"
			value := kv.Value
			formatString := fmt.Sprintf("%-10s %s", key, value)
			content += formatString + "\n"
		}
	}
	content += ":END:\n"
	return content
}

func (h *Headline) logbookString() string {
	var content string
	if len(h.LogBook) == 0 {
		return content
	}
	content = ":LOGBOOK:\n"
	for _, c := range h.LogBook {
		end := "nil"
		if c.Start != nil {
			content += "CLOCK: [" + c.Start.Format("2006-01-02 15:04:05") + "]"
		}
		if c.End != nil {
			end = c.End.Format("2006-01-02 15:04:05")
			duration := c.End.Sub(*c.Start)
			hours := int(duration.Hours())
			minutes := int(duration.Minutes()) % 60
			content += fmt.Sprintf(" -- [%s] =>  %d:%d", end, hours, minutes)
		}
		content += "\n"
	}
	content += ":END:\n"
	return content
}

func (h *Headline) String() string {
	content := strings.Repeat("*", h.Level) + " " + h.Title + "\n"
	content += h.taskTimeString()
	content += h.propertiesString()
	content += h.logbookString()
	content += h.Content
	return content
}

func (h *Headline) Write(content *Content) {
	content.WriteString(h.String())

	for _, c := range h.Children {
		c.Write(content)
	}
}

func (h *Headline) AdjustLevel(newLevel int) {
	if newLevel < 1 {
		// Or handle error appropriately, e.g., return an error
		// For now, let's assume newLevel is always valid as per requirement (>=1)
		// If not, we might default to 1 or log an error.
		logger.Warnf("AdjustLevel called with invalid level %d, defaulting to 1", newLevel)
		newLevel = 1
	}
	h.Level = newLevel
	for i := range h.Children {
		h.Children[i].AdjustLevel(newLevel + 1)
	}
}
