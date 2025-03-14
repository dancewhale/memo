package db

import (
	"memo/pkg/storage"
)

func GetReviewCardByWeightDueTime() *storage.Headline {
	var cards []*storage.Headline

	cardDB, err := NewCardDB()
	if err != nil {
		return nil
	}
	for _, stype := range []string{storage.NORMAL, storage.POSTPONE} {
		cards, err = cardDB.JoinFsrs().DueAtDay(0).TypeFilter(stype).Find()
		if len(cards) == 0 {
			cards, err = cardDB.JoinFsrs().DueBeforeDay(0).TypeFilter(stype).Find()
			if len(cards) == 0 {
				continue
			} else {
				return cards[0]
			}
		} else {
			return cards[0]
		}
	}
	return nil
}
