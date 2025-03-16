package db

import (
	"context"
	"github.com/samber/lo"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
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
