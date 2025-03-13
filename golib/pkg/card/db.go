package card

import (
	"context"
	"github.com/maniartech/gotime"
	"gorm.io/gorm"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"time"
)

func NewFsrsDB() (*FsrsDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	return &FsrsDB{query: dal.Use(db), db: db}, nil
}

type FsrsDB struct {
	query *dal.Query
	db    *gorm.DB
}

func (f *FsrsDB) createFsrs(fsrsInfo *storage.FsrsInfo) error {
	fsrs := f.query.FsrsInfo
	err := fsrs.WithContext(context.Background()).Create(fsrsInfo)
	if err != nil {
		return logger.Errorf("Create fsrs info for headline %s failed: %v", fsrsInfo.HeadlineID, err)
	}
	return nil
}

// update fsrsinfo of headline.
func (f *FsrsDB) updateFsrs(Fsrs *storage.FsrsInfo) error {
	if Fsrs == nil {
		return nil
	}
	fsrs := f.query.FsrsInfo
	_, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(Fsrs.HeadlineID)).Updates(Fsrs)
	if err != nil {
		logger.Errorf("Update %s Headline FsrsInfo in database failed: %v", Fsrs.HeadlineID, err)
		return err
	}
	return nil
}

// 获取一张卡片的fsrs信息
func (f *FsrsDB) getFsrsInfoByOrgID(orgid string) *storage.FsrsInfo {
	fsrs := f.query.FsrsInfo
	FsrsInfo, err := fsrs.WithContext(context.Background()).Where(fsrs.HeadlineID.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get FsrsInfo by orgid failed: %v", err)
		return nil
	}
	return FsrsInfo
}

// 移除一张卡片,包括所有学习记录。
func (f *FsrsDB) removeFsrs(orgid string) error {
	fsrs := f.query.FsrsInfo
	r := f.query.ReviewLog

	_, err := fsrs.WithContext(context.Background()).Unscoped().Where(fsrs.HeadlineID.Eq(orgid)).Delete()
	if err != nil {
		return logger.Errorf("Remove Fsrs in by %s failed: %v", orgid, err)
	}
	_, err = r.WithContext(context.Background()).Unscoped().Where(r.HeadlineID.Eq(orgid)).Delete()
	if err != nil {
		return logger.Errorf("Remove ReviewLog in by %s failed: %v", orgid, err)
	}
	return nil
}

func (f *FsrsDB) createReviewLog(rlog *storage.ReviewLog) error {
	r := f.query.ReviewLog

	if rlog.HeadlineID == "" {
		return logger.Errorf("Add review log failed: headlineID is empty.")
	}

	err := r.WithContext(context.Background()).Create(rlog)
	if err != nil {
		return logger.Errorf("Add review log in db failed: %v", err)
	}
	return err
}

// 传入参数orgid,当对应的Card 卡片已经到期则返回true,否则返回false
func (f *FsrsDB) ifCardIsDue(orgid string) (bool, error) {
	fsrs := f.query.FsrsInfo

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

// get card which due time is today
func (f *FsrsDB) dueCardsInToday(stype string) []*storage.Headline {
	h := f.query.Headline
	fsrs := f.query.FsrsInfo

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

// 获取已经到期且已经延期的卡片
func (f *FsrsDB) dueCardsBeforeToday(stype string) []*storage.Headline {
	h := f.query.Headline
	fsrs := f.query.FsrsInfo

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
	h := f.query.Headline
	fsrs := f.query.FsrsInfo

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

// get heads that need init fsrs info.
func (f *FsrsDB) getFsrsEmptyHeadline() []*storage.Headline {
	headline := f.query.Headline
	fsrsInfo := f.query.FsrsInfo

	heads, err := headline.WithContext(context.Background()).
		LeftJoin(fsrsInfo, fsrsInfo.HeadlineID.EqCol(headline.ID)).
		Where(fsrsInfo.CreatedAt.IsNull()).
		Find()
	if err != nil {
		logger.Errorf("Get fsrs empty headline failed: %v", err)
		return nil
	}
	return heads
}
