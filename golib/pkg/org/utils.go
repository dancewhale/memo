package org

import (
	"crypto/md5"
	"encoding/hex"
	"io"
	"memo/pkg/org/db"
	"os"

	"github.com/emirpasic/gods/stacks/arraystack"

	"github.com/dancewhale/go-org/org"

	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
)

var emacsVar = options.EmacsEnvInit()

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

func getHeadlineIdType(pd *org.PropertyDrawer) (string, string) {
	var id string
	var headType string
	if pd != nil {
		for _, kvPair := range pd.Properties {
			k, v := kvPair[0], kvPair[1]
			if k == emacsVar.MemoIdProverty && v != "" {
				id = v
			}
			if k == emacsVar.MemoTypeProverty && v != "" {
				headType = v
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

func getFileMeta(d *org.Document) (*MetaInfo, error) {
	if len(d.Nodes) != 0 {
		meta := MetaInfo{}
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case org.Headline:
				break
			case org.PropertyDrawer:
				for _, kvPair := range n.Properties {
					k, v := kvPair[0], kvPair[1]
					if k == emacsVar.MemoIdProverty && v != "" {
						if meta.ID == "" {
							meta.ID = v
						} else {
							logger.Warnf("Found duplicate id: %s in file %s.", v, d.Path)
							return nil, FoundDupID
						}
						return &meta, nil
					}
				}
			}
		}
	}
	logger.Warnf("No content in file %s.", d.Path)
	return nil, MissFileID
}

func getHeadOrder(stack *arraystack.Stack, currentHead db.Headline) int {
	order := 1
	if stack.Size() == 0 {
		return order
	}
	it := stack.Iterator()
	for it.End(); it.Prev(); {
		v := it.Value()
		if v.(db.Headline).Data.Level == currentHead.Data.Level {
			order++
		} else {
			break
		}
	}
	return order

}
