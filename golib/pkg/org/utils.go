package org

import (
	"github.com/dancewhale/go-org/org"
	"github.com/emirpasic/gods/stacks/arraystack"
	"memo/pkg/org/db"
	"strconv"

	"memo/cmd/libmemo/options"
	"memo/pkg/logger"
)

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

func FilterContentForHeadline(node org.Node) bool {
	_, d := node.(org.Drawer)
	_, p := node.(org.PropertyDrawer)
	_, h := node.(org.Headline)
	if !d && !p && !h {
		return true
	} else {
		return false
	}
}

func getWeightFromPropertyDrawer(pd *org.PropertyDrawer) int64 {
	weight, exist := pd.Get(options.GetPropertyWeight())
	if !exist {
		return 50
	} else {
		w, err := strconv.Atoi(weight)
		if err != nil {
			return 50
		} else {
			return int64(w)
		}
	}
}

func updateHeadlineProperty(headline *db.Headline, pd *org.PropertyDrawer) {
	if pd != nil {
		headline.Data.ID, _ = pd.Get(options.GetPropertyID())
		headline.Data.Weight = getWeightFromPropertyDrawer(pd)
	}
}

func getFileMeta(d *org.Document) (*MetaInfo, error) {
	meta := MetaInfo{}
	if len(d.Nodes) != 0 {
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case org.Headline:
				break
			case org.PropertyDrawer:
				id, exist := n.Get(options.GetPropertyID())
				if exist && meta.ID != "" {
					return nil, FoundDupID
				} else if exist {
					meta.ID = id
				}
			}
		}
	}
	if meta.ID == "" {
		logger.Warnf("No content in file %s.", d.Path)
		return nil, MissFileID
	} else {
		return &meta, nil
	}
}

// Get the order of headline.
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
