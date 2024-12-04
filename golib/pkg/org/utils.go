package org

import (
	"crypto/md5"
	"encoding/hex"
	"io"
	"os"

	"github.com/niklasfasching/go-org/org"

	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
)

var emacsVar = options.EmacsEnvInit()

func isRawTextBlock(name string) bool {
	return name == "SRC" || name == "EXAMPLE" || name == "EXPORT"
}

// 用于对[]org.Nodes 过滤
func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := []T{}
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func getHeadlineIdType(pd *org.PropertyDrawer) (string, *string) {
	var id string
	var headType *string
	if pd != nil {
		for _, kvPair := range pd.Properties {
			k, v := kvPair[0], kvPair[1]
			if k == emacsVar.MemoIdProverty && v != "" {
				id = v
			}
			if k == emacsVar.MemoTypeProverty && v != "" {
				headType = &v
			}
		}
	}
	return id, headType
}

func getFileID(d *org.Document) string {
	if len(d.Nodes) != 0 {
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case org.Headline:
				break
			case org.PropertyDrawer:
				for _, kvPair := range n.Properties {
					k, v := kvPair[0], kvPair[1]
					if k == emacsVar.MemoIdProverty && v != "" {
						return v
					}
				}
			}
		}
	}
	return ""
}

func hash(filePath string) (string, error) {
	f, err := os.Open(filePath)
	defer f.Close()

	if err != nil {
		logger.Errorf(err.Error())
		return "", err
	}

	hash := md5.New()

	_, err = io.Copy(hash, f)
	if err != nil {
		logger.Errorf(err.Error())
		return "", err
	}
	return hex.EncodeToString(hash.Sum(nil)), nil
}

func getFileMeta(d *org.Document) *MetaInfo {
	if len(d.Nodes) != 0 {
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case org.Headline:
				break
			case org.PropertyDrawer:
				for _, kvPair := range n.Properties {
					k, v := kvPair[0], kvPair[1]
					if k == emacsVar.MemoIdProverty && v != "" {
						meta := MetaInfo{}
						meta.ID = v
						return &meta
					}
				}
			}
		}
	}
	return nil
}
