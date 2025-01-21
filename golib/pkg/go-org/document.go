package orgp

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
)

type Configuration struct {
	DefaultSettings map[string]string // Default values for settings that are overriden by setting the same key in BufferSettings.
	Log             *log.Logger       // Log is used to print warnings during parsing.
}

// Document contains the parsing results and a pointer to the Configuration.
type Document struct {
	*Configuration
	tokens         []token
	Path           string
	Outline        Outline           // Outline is a Table Of Contents for the document and contains all sections (headline + content).
	BufferSettings map[string]string // Settings contains all settings that were parsed from keywords.
	Error          error
}

// Node represents a parsed node of the document.
type Node interface {
	String() string
}

type lexFn = func(line string) (t token, ok bool)
type stopFn = func(*Document, int) bool

type token struct {
	kind    string
	content string
	matches []string
}

var lexFns = []lexFn{
	lexHeadline,
	lexTaskTime,
	lexDrawer,
	lexKeywordOrComment,
	lexText,
}

var nilToken = token{"nil", "", nil}

// New returns a new Configuration with (hopefully) sane defaults.
func New() *Configuration {
	return &Configuration{
		DefaultSettings: map[string]string{
			"TODO": "TODO | DONE",
		},
		Log: log.New(os.Stderr, "go-org: ", 0),
	}
}

// Parse parses the input into an AST (and some other helpful fields like Outline).
// To allow method chaining, errors are stored in document.Error rather than being returned.
func (c *Configuration) Parse(input io.Reader, path string) []Node {
	outlineSection := &Section{}
	d := &Document{
		Configuration:  c,
		Outline:        Outline{outlineSection, outlineSection, 0},
		BufferSettings: map[string]string{},
		Path:           path,
	}
	defer func() {
		if recovered := recover(); recovered != nil {
			d.Error = fmt.Errorf("could not parse input: %v", recovered)
		}
	}()
	if d.tokens != nil {
		d.Error = fmt.Errorf("parse was called multiple times")
	}
	d.tokenize(input)
	_, nodes := d.parseMany(0, func(d *Document, i int) bool { return i >= len(d.tokens) })
	return nodes
}

func (c *Configuration) ParseString(input io.Reader) []Node {
	return nil
}

// Silent disables all logging of warnings during parsing.
func (c *Configuration) Silent() *Configuration {
	c.Log = log.New(ioutil.Discard, "", 0)
	return c
}

func (d *Document) tokenize(input io.Reader) {
	d.tokens = []token{}
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		d.tokens = append(d.tokens, tokenize(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		d.Error = fmt.Errorf("could not tokenize input: %s", err)
	}
}

// Get returns the value for key in BufferSettings or DefaultSettings if key does not exist in the former
func (d *Document) Get(key string) string {
	if v, ok := d.BufferSettings[key]; ok {
		return v
	}
	if v, ok := d.DefaultSettings[key]; ok {
		return v
	}
	return ""
}

func (d *Document) parseOne(i int, stop stopFn) (consumed int, node Node) {
	switch d.tokens[i].kind {
	case "beginDrawer":
		consumed, node = d.parseDrawer(i, stop)
	case "taskTime":
		consumed, node = d.parseTaskTime(i, stop)
	case "text":
		consumed, node = d.parseParagraph(i, stop)
	case "headline":
		consumed, node = d.parseHeadline(i, stop)
	}

	if consumed != 0 {
		return consumed, node
	}
	d.Log.Printf("Could not parse token %#v in file %s: Falling back to treating it as plain text.", d.tokens[i], d.Path)
	m := plainTextRegexp.FindStringSubmatch(d.tokens[i].matches[0])
	d.tokens[i] = token{"text", m[1], m}
	return d.parseOne(i, stop)
}

func (d *Document) parseMany(i int, stop stopFn) (int, []Node) {
	start, nodes := i, []Node{}
	for i < len(d.tokens) && !stop(d, i) {
		consumed, node := d.parseOne(i, stop)
		i += consumed
		nodes = append(nodes, node)
	}
	return i - start, nodes
}

func (d *Document) addHeadline(headline *Headline) int {
	current := &Section{Headline: headline}
	d.Outline.last.add(current)
	d.Outline.count++
	d.Outline.last = current
	return d.Outline.count
}

func tokenize(line string) token {
	for _, lexFn := range lexFns {
		if token, ok := lexFn(line); ok {
			return token
		}
	}
	panic(fmt.Sprintf("could not lex line: %s", line))
}
