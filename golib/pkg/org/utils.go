package org

import "github.com/niklasfasching/go-org/org"

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

func getID(pd *org.PropertyDrawer) *string {
	if pd != nil {
		for _, kvPair := range pd.Properties {
			k, v := kvPair[0], kvPair[1]
			if k == "ID" && v != "" {
				return &v
			}
		}
	}
	return nil
}

func getType(pd *org.PropertyDrawer) string {
	if pd != nil {
		for _, kvPair := range pd.Properties {
			k, v := kvPair[0], kvPair[1]
			if k == "MEMO_TYPE" && v != "" {
				return v
			}
		}
	}
	return ""
}
