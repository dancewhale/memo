package card

import (
	cardDB "memo/pkg/db/query"
	"memo/pkg/storage"
)

func GetReviewCardByWeightDueTime() *storage.Headline {
	queryNormal := []string{"filter:dueBefore:1", "order:due:desc", "filter:type:normal"}
	queryPostpone := []string{"filter:dueBefore:1", "order:due:desc", "filter:type:postpone"}

	querys := [][]string{queryNormal, queryPostpone}
	for _, query := range querys {
		q, err := cardDB.BuildQueryFromSyntax(query)
		if err != nil {
			return nil
		}
		cards, err := q.ExecuteList()
		if len(cards) == 0 {
			continue
		} else {
			return cards[0]
		}
	}
	return nil
}
