package db

import (
	"context"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"strconv"

	"github.com/samber/lo"
)

func getHeadIDByAncestorID(ancestorID string) []string {
	var headIDs []string

	h := dal.Headline
	heads, _ := h.WithContext(context.Background()).Where(h.ParentID.Eq(ancestorID)).Find()
	if len(heads) != 0 {
		headIDs = lo.Map(heads, func(head *storage.Headline, index int) string {
			return head.ID
		})
		for _, head := range heads {
			headIDs = append(headIDs, getHeadIDByAncestorID(head.ID)...)
		}
	}

	return headIDs
}

func getHeadIDByAncestorIDs(ancestorID []string) []string {
	var headIDs []string
	for _, ancID := range ancestorID {
		headIDs = append(headIDs, getHeadIDByAncestorID(ancID)...)
	}
	// distinct headIDs
	headIDs = lo.Uniq(headIDs)
	return headIDs
}

func ParseIntForList(values []string) []int64 {
	return lo.Map(values, func(value string, index int) int64 {
		num, _ := strconv.ParseInt(value, 10, 64)
		return num
	})
}

func ParseStateList(values []string) []int8 {
	return lo.Map(values, func(value string, index int) int8 {
		switch value {
		case "new":
			return 0
		case "learning":
			return 1
		case "review":
			return 2
		case "relearning":
			return 3
		}
		return 0
	})
}
