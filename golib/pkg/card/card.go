package card

import (
	"context"
	"errors"
	"strconv"
	"time"

	"memo/pkg/logger"
	"memo/pkg/org/location"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/util"

	"github.com/jinzhu/copier"
	epc "github.com/kiwanami/go-elrpc"
	"github.com/maniartech/gotime"
	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
	"gorm.io/gen/field"
	//	"github.com/spewerspew/spew"
	"gorm.io/gorm"
)

func NewCardApi() (*CardApi, error) {
	DB, err := storage.InitDBEngine()
	scheduler := *gfsrs.NewFSRS(gfsrs.DefaultParam())
	return &CardApi{
		Query:     *dal.Use(DB),
		db:        DB,
		scheduler: scheduler,
	}, err
}

type CardApi struct {
	dal.Query
	db        *gorm.DB
	scheduler gfsrs.FSRS
}

func (api *CardApi) RegistryEpcMethod(service *epc.ServerService) *epc.ServerService {
	service.RegisterMethod(epc.MakeMethod("GetNextReviewNote", api.GetNextReviewNote, "string", "Get next review note"))
	service.RegisterMethod(epc.MakeMethod("ReviewNote", api.ReviewNote, "string", "Review note"))
	return service
}

// function to export to emacs rpc.
func (api *CardApi) GetNextReviewNote() util.Result {
	head := api.GetReviewCardByWeightDueTime()
	if head == nil {
		return util.Result{Data: nil, Err: errors.New("There is no card need review.")}
	}
	weight := strconv.Itoa(int(head.Weight))

	note := util.Note{
		ID:      head.ID,
		Weight:  weight,
		Content: head.Content,
		File:    head.File.FilePath,
		Source:  head.Source,
	}

	return util.Result{note, nil}
}

func (api *CardApi) ReviewNote(orgID string, rating string) error {
	if orgID == "" {
		return logger.Errorf("orgID is empty When review note.")
	}
	fsrsRate := storage.StringToRate(rating)
	r := api.ReviewCard(orgID, fsrsRate)
	logger.Debugf("Review note success, orgID: %s, rating: %s", r.ID, rating)
	return nil
}

// GetCard 获取一张卡片。
func (api *CardApi) getCardByOrgID(orgid string) *storage.Headline {
	n := api.Headline
	Card, err := n.WithContext(context.Background()).Preload(n.Fsrs).Where(n.ID.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get Headline by orgid failed: %v", err)
		return nil
	}
	return Card
}

// UpdateCard 设置一张卡片。
func (api *CardApi) UpdateCard(fCard *storage.Headline) *storage.Headline {
	n := api.Headline
	_, err := n.WithContext(context.Background()).Where(n.ID.Eq(fCard.ID)).Updates(fCard)
	if err != nil {
		logger.Errorf("Update Headline in database failed: %v", err)
		return nil
	}
	return fCard
}

// UpdateCardOfCard 更新一张卡片中的学习记录。
func (api *CardApi) UpdateCardOfCard(fCard *storage.Headline) *storage.Headline {
	api.db.Unscoped().Model(fCard).Association("Fsrs").Unscoped().Replace(&fCard.Fsrs)
	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fCard)
	return fCard
}

// RemoveCard 移除一张卡片,包括所有学习记录。
func (api *CardApi) RemoveCard(orgid string) error {
	n := api.Headline
	card, err := n.WithContext(context.Background()).Where(n.ID.Eq(orgid)).First()
	if err != nil {
		return logger.Errorf("Remove Headline in db failed: %v", err)
	}
	_, error := n.Select(field.AssociationFields).Delete(card)
	return error
}

// 增加复习记录
func (api *CardApi) AddReviewLog(orgid string, rlog *gfsrs.ReviewLog) error {
	n := api.Headline
	card := &storage.Headline{}
	log := &storage.ReviewLog{}

	copier.Copy(log, rlog)

	card, err := n.WithContext(context.Background()).
		Preload(n.ReviewLogs).
		Where(n.ID.Eq(orgid)).
		First()

	if err != nil {
		return logger.Errorf("Add review log in db failed: %v", err)
	}

	return n.ReviewLogs.Model(card).Append(log)
}

// CountCards 获取卡包中的闪卡数量。
func (api *CardApi) CountCards() int {
	return 0
}

// 获取在之后第day天当天到期，当天应该复习的所有闪卡，+1表示明天，-1表示昨天
func (api *CardApi) DueCardsInDay(day int64) []*storage.Headline {
	h := api.Headline
	f := api.FsrsInfo
	l := api.Location

	china, _ := time.LoadLocation("Asia/Shanghai")
	dueDay := time.Now().In(china).AddDate(0, 0, int(day))
	dueDayStart := gotime.SoD(dueDay)
	dueDayEnd := gotime.EoD(dueDay)

	heads, err := h.WithContext(context.Background()).
		Preload(h.Locations.On(l.Type.Eq(string(location.SourceType)))).Preload(h.File).
		Join(f, h.ID.EqCol(f.HeadlineID)).
		Order(h.Weight.Desc()).Order(f.Due.Asc()).Where(f.Due.Gte(dueDayStart)).
		Where(f.Due.Lte(dueDayEnd)).Find()

	if err != nil {
		_ = logger.Errorf("Get due heads in %d day failed: %v", day, err)
		return nil
	}
	return heads
}

// 获取已经到期且已经延期的卡片
func (api *CardApi) DueCardsBeforeToday(stype string) []*storage.Headline {
	h := api.Headline
	f := api.FsrsInfo
	l := api.Location

	var err error
	var heads []*storage.Headline

	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayStart := gotime.SoD(today)

	headDo := h.WithContext(context.Background()).
		Preload(h.Locations.On(l.Type.Eq(string(location.SourceType)))).Preload(h.File).
		Join(f, h.ID.EqCol(f.HeadlineID)).
		Order(h.Weight.Desc()).Order(f.Due.Asc()).
		Where(f.Due.Lte(todayStart))

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

// get card which due time is today
func (api *CardApi) DueCardsInToday(stype string) []*storage.Headline {
	h := api.Headline
	f := api.FsrsInfo
	l := api.Location

	var err error
	var heads []*storage.Headline

	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayStart := gotime.SoD(today)
	todayEnd := gotime.EoD(today)

	headDo := h.WithContext(context.Background()).
		Preload(h.Locations.On(l.Type.Eq(string(location.SourceType)))).Preload(h.Fsrs).
		Join(f, h.ID.EqCol(f.HeadlineID)).
		Order(h.Weight.Desc()).Order(f.Due.Asc()).
		Where(f.Due.Lte(todayEnd)).Where(f.Due.Gt(todayStart))

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

func (api *CardApi) GetReviewCardByWeightDueTime() *storage.Headline {
	for _, stype := range []string{storage.NORMAL, storage.POSTPONE} {
		cards := api.DueCardsInToday(stype)
		if len(cards) == 0 {
			cards = api.DueCardsBeforeToday(stype)
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

// 给指定过期的闪卡打分进行review,返回复习后的闪卡
func (api *CardApi) ReviewCard(orgID string, rating gfsrs.Rating) *storage.Headline {

	logger.Debugf("Start Review Headline with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fcard := api.getCardByOrgID(orgID)
	if fcard == nil {
		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}

	// review 状态为waitReview 的Card, 如果评价为easy,则设置为WaitCardInit
	needReview, error := api.ifCardIsDue(orgID)

	if needReview && error == nil {
		schedulingInfo := api.scheduler.Repeat(fcard.Fsrs.Card, now)
		updatedCard := schedulingInfo[rating].Card

		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := storage.ReviewLog{}
		reviewlog.ReviewLog = rLog

		fcard.Fsrs.Card = updatedCard
		fcard.ReviewLogs = append(fcard.ReviewLogs, reviewlog)
		return api.UpdateCardOfCard(fcard)
	}
	logger.Infof("Card %s no need to review.", orgID)
	return nil
}

// 传入参数orgid,当对应的Card 卡片已经到期则返回true,否则返回false
func (api *CardApi) ifCardIsDue(orgid string) (bool, error) {
	h := api.Headline
	f := api.FsrsInfo
	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayEnd := gotime.EoD(today)
	heads, err := h.WithContext(context.Background()).Join(f, h.ID.EqCol(f.HeadlineID)).Where(h.ID.Eq(orgid)).Where(f.Due.Lte(todayEnd)).Find()
	if err != nil {
		return false, logger.Errorf("Get due Card order by duetime failed: %v", err)
	} else if len(heads) == 0 {
		return false, nil
	} else {
		return true, nil
	}
}

func (api *CardApi) scanHeadlineInitFsrs() ([]*storage.Headline, error) {
	logger.Infof("Start to scan org for headline init.")
	headline := api.Headline
	fsrsInfo := api.FsrsInfo

	heads, err := headline.WithContext(context.Background()).
		LeftJoin(fsrsInfo, fsrsInfo.HeadlineID.EqCol(headline.ID)).
		Where(fsrsInfo.ID.IsNull()).
		Find()

	if err != nil {
		logger.Errorf("Search for headline to init failed in ScanHeadInitFsrs %s.", err.Error())
		return nil, err
	}
	for _, head := range heads {
		if head.Status == "" && head.Content != "" {
			fsrsinfo := &storage.FsrsInfo{}
			fsrsinfo.Card = gfsrs.NewCard()
			fsrsinfo.HeadlineID = head.ID
			err = fsrsInfo.WithContext(context.Background()).Create(fsrsinfo)
			if err != nil {
				return nil, logger.Errorf("Create fsrs info for headline %s failed: %v", head.ID, err)
			}
		}
	}
	return heads, err
}
