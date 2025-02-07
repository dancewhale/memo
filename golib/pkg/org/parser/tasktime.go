package parser

import (
	"regexp"
	"time"
)

type TaskTime struct {
	Scheduled string
	Deadline  string
	Closed    string
	Content   string
}

var taskTimeRegexp = regexp.MustCompile(`(?i)^\s*((SCHEDULED|DEADLINE):\s*<\d{4}-\d{2}-\d{2}\s+\w+>\s*)+$`)

var closedRegexp = regexp.MustCompile(`(?i)^\s*CLOSED:\s*\[(\d{4}-\d{2}-\d{2}\s+\w+\s+{\d}2:{\d}2)\]\s*`)
var scheduledRegexp = regexp.MustCompile(`(?i)^\s*SCHEDULED:\s*<(\d{4}-\d{2}-\d{2}\s+\w+)>\s*`)
var deadlineRegexp = regexp.MustCompile(`(?i)^\s*DEADLINE:\s*<(\d{4}-\d{2}-\d{2}\s+\w+)>\s*`)

func lexTaskTime(line string) (token, bool) {
	if m := taskTimeRegexp.FindStringSubmatch(line); m != nil {
		return token{"taskTime", m[0], m}, true
	} else if m := closedRegexp.FindStringSubmatch(line); m != nil {
		return token{"taskTime", m[0], m}, true
	}
	return nilToken, false
}

func (d *Document) parseTaskTime(i int, parentStop stopFn) (int, Node) {
	timeContent := d.tokens[i].content
	i++
	taskTime := TaskTime{}
	taskTime.Content = timeContent

	if m := scheduledRegexp.FindStringSubmatch(timeContent); m != nil {
		taskTime.Scheduled = m[1]
	}
	if m := deadlineRegexp.FindStringSubmatch(timeContent); m != nil {
		taskTime.Deadline = m[1]
	}
	if m := closedRegexp.FindStringSubmatch(timeContent); m != nil {
		taskTime.Closed = m[1]
	}
	return 1, taskTime

}

func (n TaskTime) GetScheduled() *time.Time {
	if n.Scheduled == "" {
		return nil
	}
	t, err := time.Parse("2006-01-02 Mon", n.Scheduled)
	if err != nil {
		return nil
	} else {
		return &t
	}
}

func (n TaskTime) GetDeadline() *time.Time {
	if n.Deadline == "" {
		return nil
	}
	t, err := time.Parse("2006-01-02 Mon", n.Deadline)
	if err != nil {
		return nil
	} else {
		return &t
	}
}

func (n TaskTime) GetClosed() *time.Time {
	if n.Closed == "" {
		return nil
	}
	t, err := time.Parse("2006-01-02 Mon 12:03", n.Closed)
	if err != nil {
		return nil
	} else {
		return &t
	}
}

func (n TaskTime) String() string {
	return n.Content
}
