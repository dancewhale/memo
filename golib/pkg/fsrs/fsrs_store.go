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
	//"time"

	
	"github.com/jinzhu/copier"	
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
	"github.com/spewerspew/spew"
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
	n := dal.Use(store.db).Note
	spew.Dump(fnote)
	err := n.WithContext(context.Background()).Create(fnote)
	if err != nil {
		logger.Errorf("Create note in db failed: %v", err)
		return  nil
	}
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
func (store *FSRSStore) UpdateCardOfNote(fnote *storage.Note, newCard gfsrs.Card) *storage.Note {
	snote := storage.Note{}
	store.db.Preload("Card").Where("org_id = ?", fnote.Orgid).First(snote)
	err := store.db.Model(&snote).Association("Card").Replace(snote.Card, newCard)
	if err != nil {
		logger.Errorf("Update card of note in db association failed: %v", err)
		return nil
	}
	return &snote
}

// RemoveNote 移除一张卡片。
func (store *FSRSStore) RemoveNote(orgid string) error {
	n := dal.Use(store.db).Note
	note, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
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

    // Dues 获取所有到期的闪卡列表。
func (store *FSRSStore) Dues() (ret []storage.Note) {

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

