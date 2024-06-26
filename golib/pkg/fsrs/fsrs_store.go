package fsrs

import (
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/logger"
	//"os"
	//"path/filepath"
	//"strconv"
	//"strings"
	"context"
	"time"

	
	"github.com/jinzhu/copier"	
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
	//"github.com/spewerspew/spew"
	"gorm.io/gorm"
	"gorm.io/gen/field"
)

func NewFSRSStore() *FSRSStore {
	var DB = storage.InitDBEngine()
	return &FSRSStore{db: DB}
}

type FSRSStore struct {
    	db      *gorm.DB
}

    // CreateNote 添加一张卡片。
func (store *FSRSStore) CreateNote(fnote *storage.Note) *storage.Note {
	fcard := gfsrs.Card{Due: time.Now(), Stability: 0.0, Difficulty: 0.0, ElapsedDays: 0, ScheduledDays: 0, Reps: 0, Lapses: 0, LastReview: time.Now(), State: gfsrs.New}
	scard := storage.Card{}
	scard.Card = fcard
	fnote.Card = scard
	store.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

    // GetNote 获取一张卡片。
func (store *FSRSStore) GetNoteByOrgID(orgid string) *storage.Note {
	note := &storage.Note{}
	store.db.Preload("Card").Where("orgid = ?", orgid).First(note)
	return note
}

    // UpdateNote 设置一张卡片。
func (store *FSRSStore) UpdateNote(fnote *storage.Note) *storage.Note {
	n := dal.Use(store.db).Note
	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.Orgid)).Updates(fnote)
	if err != nil {
		return nil
	}
	return fnote
}

    // UpdateCardOfNote 更新一张卡片中的记录。
func (store *FSRSStore) UpdateCardOfNote(fnote *storage.Note) *storage.Note {
	store.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

// RemoveNote 移除一张卡片。
func (store *FSRSStore) RemoveNote(orgid string) error {
	n := dal.Use(store.db).Note
	note, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Remove note in db failed: %v", err)
		return err
	}
	_, error := n.Select(field.AssociationFields).Delete(note)
	return error
}

// 增加复习记录
func (store *FSRSStore) AddReviewLog(orgid string, rlog *gfsrs.ReviewLog) error {
	note := &storage.Note{}
	log := &storage.ReviewLog{}
	copier.Copy(log, rlog)
	n := dal.Use(store.db).Note
	store.db.Preload("Logs").Where("org_id = ?", orgid).First(note)

	return n.Logs.Model(note).Append(log)
}


    // 获取指定OrgId的新的卡片（制卡后没有进行过复习的卡片）。
func (store *FSRSStore) GetNewNotesByOrgIDs(blockIDs []string) (ret []storage.Note) {

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
func (store *FSRSStore) GetDueNotesByOrgIDs(blockIDs []string) (ret []storage.Note) {

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

    // Dues 获取在day 天之后以前到期的所有闪卡。
func (store *FSRSStore) DueNotes(day int64) ([]*storage.Note) {
	n := dal.Use(store.db).Note
	notes, err := n.WithContext(context.Background()).Preload(n.Card).Find()
	if err != nil {
		logger.Errorf("Get all notes failed: %v", err)
		return nil
	}
	ret := []*storage.Note{}

	for _, note := range notes {
		cardDueYear := note.Card.Due.Year()
		cardDueMonth := note.Card.Due.Month()
		cardDueDay := note.Card.Due.Day()
		cardDue := time.Date(cardDueYear, cardDueMonth, cardDueDay, 0, 0, 0, 0, time.UTC)

		torrow  := time.Now().AddDate(0, 0, int(day))
		dueTimeYear := torrow.Year()
		dueTimeMonth := torrow.Month()
		dueTimeDay := torrow.Day()
		dueTime := time.Date(dueTimeYear, dueTimeMonth, dueTimeDay, 0, 0, 0, 0, time.UTC)
		
		//check  cardDue before tody 24:00
		if cardDue.Before(dueTime) || cardDue.Equal(dueTime) {
			ret = append(ret, note)
		}
	}
	return ret
}

