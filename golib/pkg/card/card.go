package card

import (
	"memo/pkg/card/db"
	"memo/pkg/storage"
)

func GetReviewCardByWeightDueTime() *storage.Headline {
	var cards []*storage.Headline

	cardDB, err := db.NewCardDB()
	if err != nil {
		return nil
	}
	day := []int64{0}
	types := [][]string{{storage.NORMAL}, {storage.POSTPONE}}
	for _, stype := range types {
		cards, err = cardDB.JoinFsrs().DueAtDaysFilter("+", day).TypeFilter("+", stype).Find()
		if len(cards) == 0 {
			cards, err = cardDB.JoinFsrs().DueBeforeDayFilter("+", 0).TypeFilter("+", stype).Find()
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
