package org

import (
	"github.com/niklasfasching/go-org/org"
	"memo/pkg/storage"
)

var datestampFormat = "2006-01-02 Mon"
var timestampFormat = "2006-01-02 Mon 15:04"

func isRawTextBlock(name string) bool {
	return name == "SRC" || name == "EXAMPLE" || name == "EXPORT"
}

func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := []T{}
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func getIdType(pd *org.PropertyDrawer) storage.Note {
	if pd != nil {
		note := storage.Note{}
		for _, kvPair := range pd.Properties {
			k, v := kvPair[0], kvPair[1]
			if k == "ID" && v != "" {
				note.Orgid = v
			}
			if k == "MEMO_TYPE" && v != "" {
				note.Type = &v
			}
		}
		if note.Orgid != "" {
			return note
		}
	}
	return storage.Note{}
}
