package parser

import (
	"fmt"
	"github.com/emirpasic/gods/lists/arraylist"
	"regexp"
	"strings"
	"time"
)

type Drawer struct {
	Name     string
	Children *arraylist.List
	Content  string
}

type PropertyDrawer struct {
	Properties [][]string
}

type LogBookDrawer struct {
	Clocks  []Clock
	Content string
}

type Clock struct {
	Start string
	End   string
}

var beginDrawerRegexp = regexp.MustCompile(`^(\s*):(\S+):\s*$`)
var endDrawerRegexp = regexp.MustCompile(`(?i)^(\s*):END:\s*$`)
var propertyRegexp = regexp.MustCompile(`^(\s*):(\S+):(\s+(.*)$|$)`)

var logBookClockRegexp = regexp.MustCompile(`^\s*CLOCK:\s+\[(\d{4}-\d{2}-\d{2}\s+\w{3}\s+\d{2}:\d{2})\]-+\[(\d{4}-\d{2}-\d{2}\s+\w{3}\s+\d{2}:\d{2})\]`)

func lexDrawer(line string) (token, bool) {
	if m := endDrawerRegexp.FindStringSubmatch(line); m != nil {
		return token{"endDrawer", "", m}, true
	} else if m := beginDrawerRegexp.FindStringSubmatch(line); m != nil {
		return token{"beginDrawer", strings.ToUpper(m[2]), m}, true
	}
	return nilToken, false
}

func (d *Document) parseDrawer(i int, parentStop stopFn) (int, Node) {
	name := strings.ToUpper(d.tokens[i].content)
	if name == "PROPERTIES" {
		return d.parsePropertyDrawer(i, parentStop)
	} else if name == "LOGBOOK" {
		return d.parseLogBookClock(i, parentStop)
	}
	drawer, start := Drawer{Name: name, Children: arraylist.New()}, i
	i++
	stop := func(d *Document, i int) bool {
		if parentStop(d, i) {
			return true
		}
		kind := d.tokens[i].kind
		return kind == "beginDrawer" || kind == "endDrawer" || kind == "headline"
	}
	for {
		consumed, nodes := d.parseMany(i, stop)
		i += consumed
		drawer.Children.Add(nodes)
		if i < len(d.tokens) && d.tokens[i].kind == "beginDrawer" {
			p := Paragraph{Children: []Node{Text{":" + d.tokens[i].content + ":", false}}}
			drawer.Children.Add(p)
			i++
		} else {
			break
		}
	}
	if i < len(d.tokens) && d.tokens[i].kind == "endDrawer" {
		i++
	}
	return i - start, drawer
}

func (d *Document) parsePropertyDrawer(i int, parentStop stopFn) (int, Node) {
	drawer, start := PropertyDrawer{}, i
	i++
	stop := func(d *Document, i int) bool {
		return parentStop(d, i) || (d.tokens[i].kind != "text" && d.tokens[i].kind != "beginDrawer")
	}
	for ; !stop(d, i); i++ {
		m := propertyRegexp.FindStringSubmatch(d.tokens[i].matches[0])
		if m == nil {
			return 0, nil
		}
		k, v := strings.ToUpper(m[2]), strings.TrimSpace(m[4])
		drawer.Properties = append(drawer.Properties, []string{k, v})
	}
	if i < len(d.tokens) && d.tokens[i].kind == "endDrawer" {
		i++
	} else {
		return 0, nil
	}
	return i - start, drawer
}

func (d *Document) parseLogBookClock(i int, parentStop stopFn) (int, Node) {
	drawer, start := LogBookDrawer{}, i
	i++
	stop := func(d *Document, i int) bool {
		return parentStop(d, i) || (d.tokens[i].kind != "text" && d.tokens[i].kind != "beginDrawer")
	}
	for ; !stop(d, i); i++ {
		m := logBookClockRegexp.FindStringSubmatch(d.tokens[i].matches[0])
		if m == nil {
			return 0, nil
		}
		clock := Clock{Start: m[1], End: m[2]}
		drawer.Clocks = append(drawer.Clocks, clock)
	}
	if i < len(d.tokens) && d.tokens[i].kind == "endDrawer" {
		i++
	} else {
		return 0, nil
	}

	for j := start; j < i; j++ {
		content := d.tokens[j].matches[0]
		drawer.Content += content + "\n"
	}
	return i - start, drawer
}

func (d *PropertyDrawer) Get(key string) (string, bool) {
	if d == nil {
		return "", false
	}
	for _, kvPair := range d.Properties {
		if kvPair[0] == key {
			return kvPair[1], true
		}
	}
	return "", false
}

func (d *PropertyDrawer) Set(key, value string) {
	property := []string{key, value}
	if d != nil {
		for _, kvPair := range d.Properties {
			if kvPair[0] == key {
				kvPair[1] = value
				return
			}
		}
		d.Properties = append(d.Properties, property)
		return
	} else {
		d = &PropertyDrawer{Properties: [][]string{property}}
		return
	}
}

func (n Clock) GetStart() *time.Time {
	if n.Start == "" {
		return nil
	}
	t, err := time.Parse("2006-01-02 Mon 15:04", n.Start)
	if err != nil {
		return nil
	}
	return &t
}

func (n Clock) GetEnd() *time.Time {
	if n.End == "" {
		return nil
	}
	t, err := time.Parse("2006-01-02 Mon 15:04", n.End)
	if err != nil {
		return nil
	}
	return &t
}

func (p PropertyDrawer) String() string {
	property := ":PROPERTIES:\n"
	for _, kv := range p.Properties {
		key := ":" + kv[0] + ":"
		value := kv[1]
		formatString := fmt.Sprintf("%-10s %s", key, value)
		property += formatString + "\n"
	}
	property += ":END:\n"
	return property
}

func (l LogBookDrawer) String() string {
	return l.Content
}

func (d Drawer) String() string {
	return d.Content
}
