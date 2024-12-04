//package card
//
//import (
//	"context"
//	"time"
//
//	"memo/pkg/logger"
//	"memo/pkg/storage"
//	"memo/pkg/storage/dal"
//
//	"github.com/jinzhu/copier"
//	"github.com/maniartech/gotime"
//	gfsrs "github.com/open-spaced-repetition/go-fsrs/v3"
//	"gorm.io/gen/field"
//	//	"github.com/spewerspew/spew"
//	"gorm.io/gorm"
//)
//
//func NewCardApi() (*CardApi, error) {
//	DB, err := storage.InitDBEngine()
//	scheduler := *gfsrs.NewFSRS(gfsrs.DefaultParam())
//	return &CardApi{
//		Query:     *dal.Use(DB),
//		db:        DB,
//		scheduler: scheduler,
//	}, err
//}
//
//type CardApi struct {
//	dal.Query
//	db        *gorm.DB
//	scheduler gfsrs.FSRS
//}
//
//// GetCard 获取一张卡片。
//func (api *CardApi) getCardByOrgID(orgid string) *storage.Card {
//	n := api.Card
//	Card, err := n.WithContext(context.Background()).Preload(n.Fsrs).Where(n.Orgid.Eq(orgid)).First()
//	if err != nil {
//		logger.Errorf("Get Card by orgid failed: %v", err)
//		return nil
//	}
//	return Card
//}
//
//// UpdateCard 设置一张卡片。
//func (api *CardApi) UpdateCard(fCard *storage.Card) *storage.Card {
//	n := api.Card
//	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fCard.Orgid)).Updates(fCard)
//	if err != nil {
//		logger.Errorf("Update Card in database failed: %v", err)
//		return nil
//	}
//	return fCard
//}
//
//// UpdateCardOfCard 更新一张卡片中的学习记录。
//func (api *CardApi) UpdateCardOfCard(fCard *storage.Card) *storage.Card {
//	api.db.Unscoped().Model(fCard).Association("Fsrs").Unscoped().Replace(&fCard.Fsrs)
//	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fCard)
//	return fCard
//}
//
//// RemoveCard 移除一张卡片,包括所有学习记录。
//func (api *CardApi) RemoveCard(orgid string) error {
//	n := api.Card
//	Card, err := n.WithContext(context.Background()).Where(n.HeadlineID.Eq(orgid)).First()
//	if err != nil {
//		return logger.Errorf("Remove Card in db failed: %v", err)
//	}
//	_, error := n.Select(field.AssociationFields).Delete(Card)
//	return error
//}
//
//// 增加复习记录
//func (api *CardApi) AddReviewLog(orgid string, rlog *gfsrs.ReviewLog) error {
//	card := &storage.Card{}
//	log := &storage.ReviewLog{}
//	copier.Copy(log, rlog)
//	n := api.Card
//	card, err := n.WithContext(context.Background()).Preload(n.ReviewLogs).Where(n.HeadlineID.Eq(orgid)).First()
//	if err != nil {
//		return logger.Errorf("Add review log in db failed: %v", err)
//	}
//
//	return n.ReviewLogs.Model(card).Append(log)
//}
//
//// CountCards 获取卡包中的闪卡数量。
//func (api *CardApi) CountCards() int {
//	return 0
//}
//
//// 获取在之后第day天当天到期，当天应该复习的所有闪卡，+1表示明天，-1表示昨天
//func (api *CardApi) DueCardsInDay(day int64) []*storage.Card {
//	c := api.Card
//	f := api.FsrsInfo
//	china, _ := time.LoadLocation("Asia/Shanghai")
//	dueDay := time.Now().In(china).AddDate(0, 0, int(day))
//	dueDayStart := gotime.SoD(dueDay)
//	dueDayEnd := gotime.EoD(dueDay)
//	cards, err := c.WithContext(context.Background()).Join(f, c.ID.EqCol(f.CardID)).Where(f.Due.Gte(dueDayStart)).Where(f.Due.Lte(dueDayEnd)).Find()
//	if err != nil {
//		_ = logger.Errorf("Get due cards in %d day failed: %v", day, err)
//		return nil
//	}
//	return cards
//}
//
//// 获取已经到期且已经延期的卡片
//func (api *CardApi) DueCardsDeferred() []*storage.Card {
//	c := api.Card
//	f := api.FsrsInfo
//	china, _ := time.LoadLocation("Asia/Shanghai")
//	today := time.Now().In(china)
//	todayStart := gotime.SoD(today)
//	cards, err := c.WithContext(context.Background()).Join(f, c.ID.EqCol(f.CardID)).Where(f.Due.Lte(todayStart)).Find()
//	if err != nil {
//		logger.Errorf("Get due Card order by duetime failed: %v", err)
//		return nil
//	}
//	return cards
//}
//
//// TODO: 获取过期卡片的逻辑需要修正，优先当天的卡片，之后是延期的卡片
//func (api *CardApi) GetReviewCardByDueTime() *storage.Card {
//	cards := api.DueCardsDeferred()
//	if len(cards) == 0 {
//		logger.Infof("No Card need to review.")
//		return nil
//	}
//	return cards[0]
//}
//
//// 给指定过期的闪卡打分进行review,返回复习后的闪卡
//func (api *CardApi) ReviewCard(orgID string, rating gfsrs.Rating) *storage.Card {
//
//	logger.Debugf("Start Review Card with orgID: %s, rating: %d", orgID, rating)
//	now := time.Now()
//	fcard := api.getCardByOrgID(orgID)
//	if fcard == nil {
//		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
//		return nil
//	}
//
//	// review 状态为waitReview 的Card, 如果评价为easy,则设置为WaitCardInit
//	needReview, error := api.IfCardIsDue(orgID)
//
//	if needReview && error == nil {
//		schedulingInfo := api.scheduler.Repeat(fcard.Fsrs.Card, now)
//		updatedCard := schedulingInfo[rating].Card
//
//		rLog := schedulingInfo[rating].ReviewLog
//
//		reviewlog := storage.ReviewLog{}
//		reviewlog.ReviewLog = rLog
//
//		fcard.Fsrs.Card = updatedCard
//		fcard.ReviewLogs = append(fcard.ReviewLogs, reviewlog)
//		return api.UpdateCardOfCard(fcard)
//	}
//	logger.Infof("Card %s no need to review.", orgID)
//	return nil
//}
//
//// 传入参数orgid,当对应的Card 卡片已经到期则返回true,否则返回false
//func (api *CardApi) IfCardIsDue(orgid string) (bool, error) {
//	c := api.Card
//	f := api.FsrsInfo
//	china, _ := time.LoadLocation("Asia/Shanghai")
//	today := time.Now().In(china)
//	todayEnd := gotime.EoD(today)
//	Cards, err := c.WithContext(context.Background()).Join(f, c.ID.EqCol(f.CardID)).Where(c.HeadlineID.Eq(orgid)).Where(f.Due.Lte(todayEnd)).Find()
//	if err != nil {
//		return false, logger.Errorf("Get due Card order by duetime failed: %v", err)
//	} else if len(Cards) == 0 {
//		return false, nil
//	} else {
//		return true, nil
//	}
//}
//
//func (api *CardApi) ScanOrgForCardInit() ([]*storage.Card, error) {
//	logger.Infof("Start to scan org for card init.")
//	card := api.Card
//	fsrsInfo := api.FsrsInfo
//	//	cards, err := card.FindInitCard()
//	// TODO: 逻辑已经变化，需要修改，直接查询headline
//	cards, err := card.WithContext(context.Background()).LeftJoin(fsrsInfo, fsrsInfo.CardID.EqCol(card.ID)).Where(fsrsInfo.ID.IsNull()).Find()
//	if err != nil {
//		logger.Errorf("Search for card to init failed in ScanOrgForCardInit %s.", err.Error())
//		return nil, err
//	}
//
//	//var fcard gfsrs.Card
//	//var scard storage.FsrsInfo
//	//for _, fcard := range cards {
//	//	fcard = gfsrs.NewCard()
//	//	scard = storage.FsrsInfo{}
//	//	scard.Card = fcard
//	//	fcard.Fsrs = scard
//	//	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fcard)
//	//}
//	return cards, err
//}
