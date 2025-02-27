package parser

import (
	"fmt"
	"memo/cmd/options"
	"memo/pkg/storage"
	"strings"
)

type OrgFileContent struct {
	strings.Builder
}

func FileToString(f storage.File) string {
	file := &OrgFileContent{}
	file.WriteString(f.MetaContent)

	for _, h := range f.Headlines {
		HeadWriteString(h, file)
	}
	return file.String()
}

func taskTimeString(h storage.Headline) string {
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

func propertiesString(h storage.Headline) string {
	var content string
	content = ":PROPERTIES:\n"
	content += fmt.Sprintf("%-10s %s", ":"+options.EmacsPropertyID+":", h.ID) + "\n"
	content += fmt.Sprintf("%-10s %s", ":"+options.EmacsPropertySource+":", h.ID) + "\n"
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

func logbookString(h storage.Headline) string {
	var content string
	if h.LogBook == nil {
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

func headString(h storage.Headline) string {
	content := strings.Repeat("*", h.Level) + " " + h.Title + "\n"
	content += taskTimeString(h)
	content += propertiesString(h)
	content += logbookString(h)
	content += h.Content
	return content
}

func HeadWriteString(h storage.Headline, f *OrgFileContent) {
	f.WriteString(headString(h))

	for _, c := range h.Children {
		HeadWriteString(c, f)
	}
}
