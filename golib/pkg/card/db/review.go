package db

import (
	"context"
	"github.com/maniartech/gotime"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"time"
)

func GetReviewCardByWeightDueTime() *storage.Headline {
	fsrsDB, err := NewFsrsDB()
	if err != nil {
		return nil
	}
	for _, stype := range []string{storage.NORMAL, storage.POSTPONE} {
		cards := fsrsDB.dueCardsInToday(stype)
		if len(cards) == 0 {
			cards = fsrsDB.dueCardsBeforeToday(stype)
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

// 传入参数orgid,当对应的Card 卡片已经到期则返回true,否则返回false
func (f *FsrsDB) IfCardIsDue(orgid string) (bool, error) {
	fsrs := dal.FsrsInfo

	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayEnd := gotime.EoD(today)

	heads, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(orgid)).
		Where(fsrs.Due.Lte(todayEnd)).Find()

	if err != nil {
		return false, logger.Errorf("Check orgid %s if is due Card failed: %v", orgid, err)
	} else if len(heads) == 0 {
		return false, nil
	} else {
		return true, nil
	}
}

// 获取已经到期且已经延期的卡片
func (f *FsrsDB) dueCardsBeforeToday(stype string) []*storage.Headline {
	h := dal.Headline
	fsrs := dal.FsrsInfo

	var err error
	var heads []*storage.Headline

	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayStart := gotime.SoD(today)

	headDo := h.WithContext(context.Background()).
		Preload(h.File).
		Join(fsrs, h.ID.EqCol(fsrs.HeadlineID)).
		Order(h.Weight.Desc()).Order(fsrs.Due.Asc()).
		Where(fsrs.Due.Lte(todayStart))

	switch stype {
	case storage.NORMAL:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.NORMAL)).Find()
	case storage.POSTPONE:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.POSTPONE)).Find()
	default:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.NORMAL)).Find()
	}
	if err != nil {
		logger.Errorf("Get Headline due before today order by weight,duetime failed: %v", err)
		return nil
	}
	return heads
}

// 获取在之后第day天当天到期，当天应该复习的所有闪卡，+1表示明天，-1表示昨天
func (f *FsrsDB) DueCardsInDay(day int64) []*storage.Headline {
	h := dal.Headline
	fsrs := dal.FsrsInfo

	china, _ := time.LoadLocation("Asia/Shanghai")
	dueDay := time.Now().In(china).AddDate(0, 0, int(day))
	dueDayStart := gotime.SoD(dueDay)
	dueDayEnd := gotime.EoD(dueDay)

	heads, err := h.WithContext(context.Background()).
		Preload(h.File).
		Join(fsrs, h.ID.EqCol(fsrs.HeadlineID)).
		Order(h.Weight.Desc()).Order(fsrs.Due.Asc()).Where(fsrs.Due.Gte(dueDayStart)).
		Where(fsrs.Due.Lte(dueDayEnd)).Find()

	if err != nil {
		_ = logger.Errorf("Get due heads in %d day failed: %v", day, err)
		return nil
	}
	return heads
}

// get card which due time is today
func (f *FsrsDB) dueCardsInToday(stype string) []*storage.Headline {
	h := dal.Headline
	fsrs := dal.FsrsInfo

	var err error
	var heads []*storage.Headline

	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayStart := gotime.SoD(today)
	todayEnd := gotime.EoD(today)

	headDo := h.WithContext(context.Background()).
		Join(fsrs, h.ID.EqCol(fsrs.HeadlineID)).
		Order(h.Weight.Desc()).Order(fsrs.Due.Asc()).
		Where(fsrs.CreatedAt.IsNotNull()).Where(fsrs.Due.Lte(todayEnd)).Where(fsrs.Due.Gt(todayStart))

	switch stype {
	case storage.NORMAL:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.NORMAL)).Find()
	case storage.POSTPONE:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.POSTPONE)).Find()
	default:
		heads, err = headDo.Where(h.ScheduledType.Eq(storage.NORMAL)).Find()
	}
	if err != nil {
		logger.Errorf("Get Headline due in today order by weight,duetime failed: %v", err)
		return nil
	}

	return heads
}
