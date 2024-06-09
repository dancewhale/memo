package fsrs

import (
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	//"os"
	//"path/filepath"
	//"strconv"
	//"strings"
	"context"
	//"time"

	
	//import copier
	"github.com/jinzhu/copier"	
	"gorm.io/gorm"
)

var DB = storage.InitDBEngine()

type FSRSStore struct {
    	db      *gorm.DB
}

func NewFSRSStore() *FSRSStore {
	return &FSRSStore{db: DB}
}

    // AddCard 添加一张卡片。
func (store *FSRSStore) AddCard(fcard FSRSNote) FSRSNote {
	note := storage.Note{}
	card := storage.Card{}
	note.Card = card
	copier.Copy(&note, &fcard.N)
	copier.Copy(&card, &fcard.C)
	c := dal.Use(store.db).Card
	c.WithContext(context.Background()).Create(&card)
	return FSRSNote{}
}

    // GetCard 获取一张卡片。
func (store *FSRSStore) GetCard(card FSRSNote) error {

	return nil
}

    // SetCard 设置一张卡片。
func (store *FSRSStore) SetCard(card FSRSNote) error {
	return nil
}

    // RemoveCard 移除一张卡片。
func (store *FSRSStore) RemoveCard(id string) error {
	return nil
}

    // 获取指定OrgID的所有卡片。
func (store *FSRSStore) GetCardsByOrgID(blockID string) (ret []FSRSNote) {
	return nil
}

    // 获取指定OrgId的新的卡片（制卡后没有进行过复习的卡片）。
func (store *FSRSStore) GetNewCardsByOrgIDs(blockIDs []string) (ret []FSRSNote) {

	//	blockIDs = gulu.Str.RemoveDuplicatedElem(blockIDs)
	//	for _, card := range store.cards {
	//		c := card.Impl().(*fsrs.Card)
	//		if !c.LastReview.IsZero() {
	//			continue
	//		}
	//
	//		if gulu.Str.Contains(card.BlockID(), blockIDs) {
	//			ret = append(ret, card)
	//		}
	//	}
	return nil
}

    //获取指定OrgId的所有到期的卡片。
func (store *FSRSStore) GetDueCardsByOrgIDs(blockIDs []string) (ret []FSRSNote) {

	//	blockIDs = gulu.Str.RemoveDuplicatedElem(blockIDs)
	//	now := time.Now()
	//	for _, card := range store.cards {
	//		c := card.Impl().(*fsrs.Card)
	//		if now.Before(c.Due) {
	//			continue
	//		}
	//
	//		if gulu.Str.Contains(card.BlockID(), blockIDs) {
	//			ret = append(ret, card)
	//		}
	//	}
	return nil
}



// CountCards 获取卡包中的闪卡数量。
func (store *FSRSStore) CountCards() int {
	return 0
}

    // Review 闪卡复习。
func (store *FSRSStore) Review(cardId string, rating Rating) (ret *Log) {

	//	now := time.Now()
	//	card := store.cards[cardId]
	//	if nil == card {
	//		logging.LogWarnf("not found card [id=%s] to review", cardId)
	//		return
	//	}
	//
	//	schedulingInfo := store.params.Repeat(*card.C, now)
	//	updated := schedulingInfo[fsrs.Rating(rating)].Card
	//	card.SetImpl(&updated)
	//	store.cards[cardId] = card
	//
	//	reviewLog := schedulingInfo[fsrs.Rating(rating)].ReviewLog
	//	ret = &Log{
	//		ID:            newID(),
	//		CardID:        cardId,
	//		Rating:        rating,
	//		ScheduledDays: reviewLog.ScheduledDays,
	//		ElapsedDays:   reviewLog.ElapsedDays,
	//		Reviewed:      reviewLog.Review.Unix(),
	//		State:         State(reviewLog.State),
	//	}
	return
}

    // Dues 获取所有到期的闪卡列表。
func (store *FSRSStore) Dues() (ret []FSRSNote) {

	//	now := time.Now()
	//	for _, card := range store.cards {
	//		c := card.Impl().(*fsrs.Card)
	//		if now.Before(c.Due) {
	//			continue
	//		}
	//
	//		schedulingInfos := store.params.Repeat(*c, now)
	//		nextDues := map[Rating]time.Time{}
	//		for rating, schedulingInfo := range schedulingInfos {
	//			nextDues[Rating(rating)] = schedulingInfo.Card.Due
	//		}
	//		card.SetNextDues(nextDues)
	//		ret = append(ret, card)
	//	}
	return
}

    // SaveLog 保存复习日志。
func (store *FSRSStore) SaveLog(log *Log) (err error) {

	//	saveDir := store.GetSaveDir()
	//	saveDir = filepath.Join(saveDir, "logs")
	//	if !gulu.File.IsDir(saveDir) {
	//		if err = os.MkdirAll(saveDir, 0755); nil != err {
	//			return
	//		}
	//	}
	//
	//	yyyyMM := time.Now().Format("200601")
	//	p := filepath.Join(saveDir, yyyyMM+".msgpack")
	//	logs := []*Log{}
	//	var data []byte
	//	if filelock.IsExist(p) {
	//		data, err = filelock.ReadFile(p)
	//		if nil != err {
	//			logging.LogErrorf("load logs failed: %s", err)
	//			return
	//		}
	//
	//		if err = msgpack.Unmarshal(data, &logs); nil != err {
	//			logging.LogErrorf("unmarshal logs failed: %s", err)
	//			return
	//		}
	//	}
	//	logs = append(logs, log)
	//
	//	if data, err = msgpack.Marshal(logs); nil != err {
	//		logging.LogErrorf("marshal logs failed: %s", err)
	//		return
	//	}
	//	if err = filelock.WriteFile(p, data); nil != err {
	//		logging.LogErrorf("write logs failed: %s", err)
	//		return
	//	}
	return
}


