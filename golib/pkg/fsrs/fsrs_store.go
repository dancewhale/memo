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
	//"github.com/spewerspew/spew"
	"gorm.io/gorm"
	"gorm.io/gen/field"
)

var DB = storage.InitDBEngine()

type FSRSStore struct {
    	db      *gorm.DB
}

func NewFSRSStore() *FSRSStore {
	return &FSRSStore{db: DB}
}

func fsrsCardToDbCard(fnote *FSRSNote) *storage.Note {
	note := storage.Note{}
	copier.Copy(&note, fnote.N)
	copier.Copy(note.Card, fnote.C)

	return &note
}

func dbCardToFsrsCard(note *storage.Note) *FSRSNote {
	fnote := FSRSNote{}
	copier.Copy(fnote.N, note)
	copier.Copy(fnote.C, note.Card)

	return &fnote
}


    // CreateNote 添加一张卡片。
func (store *FSRSStore) CreateNote(fnote *FSRSNote) *FSRSNote {
	note := fsrsCardToDbCard(fnote)
	n := dal.Use(store.db).Note

	err := n.WithContext(context.Background()).Create(note)
	if err != nil {
		return  nil
	}
	return fnote
}

    // GetNote 获取一张卡片。
func (store *FSRSStore) GetNote(orgid string) *FSRSNote {
	note := &storage.Note{}
	store.db.Preload("Card").Where("org_id = ?", orgid).First(note)
	return dbCardToFsrsCard(note)
}

    // SetNote 设置一张卡片。
func (store *FSRSStore) SetNote(fnote *FSRSNote) *FSRSNote {
	note := fsrsCardToDbCard(fnote)
	n := dal.Use(store.db).Note

	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.N.OrgID)).Updates(note)
	if err != nil {
		return nil
	}
	return fnote
}

// RemoveNote 移除一张卡片。
func (store *FSRSStore) RemoveNote(fnote *FSRSNote) error {
	n := dal.Use(store.db).Note
	note, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.N.OrgID)).First()
	if err != nil {
		return err
	}
	_, error := n.Select(field.AssociationFields).Delete(note)
	return error
}

    // 获取指定OrgId的新的卡片（制卡后没有进行过复习的卡片）。
func (store *FSRSStore) GetNewNotesByOrgIDs(blockIDs []string) (ret []FSRSNote) {

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
func (store *FSRSStore) GetDueNotesByOrgIDs(blockIDs []string) (ret []FSRSNote) {

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



// CountNotes 获取卡包中的闪卡数量。
func (store *FSRSStore) CountNotes() int {
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


