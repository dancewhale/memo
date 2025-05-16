package _import

import (
	"bytes"
	"memo/pkg/storage"
	"os"

	"memo/pkg/logger"
	"memo/pkg/org/parser"
)

func parseDocument(filePath string) (*storage.File, error) {
	content, err := os.ReadFile(filePath)
	reader := bytes.NewReader(content)

	if err != nil {
		return nil, logger.Errorf("Read file content failed: %v", err)
	} else if len(content) == 0 {
		return nil, logger.Errorf("File %s is empty.", filePath)
	}

	nodes := parser.New().Parse(reader, filePath)

	s := parser.NewSqlWriter(storage.NormalFile)
	file := s.ParseOrgFile(nodes)
	file.FilePath = filePath
	return file, nil
}

func ensureFileID(file *storage.File, id string) {
	file.ID = id
	idString := ":PROPERTIES:\n" + ":ID:       " + file.ID + "\n" + ":END:\n"
	if len(file.Headlines) == 0 {
		title := "* " + GetLeadingChars(file.MetaContent, 13) + "\n"
		idString = idString + title
	}
	file.MetaContent = idString + file.MetaContent
}

func processOrgFile(filePath, id string) error {
	file, err := parseDocument(filePath)
	if err != nil {
		return err
	}

	ensureFileID(file, id)
	err = file.Write()
	return err
}
