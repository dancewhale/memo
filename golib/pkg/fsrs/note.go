package fsrs

import (
	"strconv"
	"strings"
	"context"
	"time"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/logger"
	
	"github.com/jinzhu/copier"	
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
	"github.com/spewerspew/spew"
	"gorm.io/gorm"
	"gorm.io/gen/field"
)

func NewParams(requestRetention float64, maximumInterval int, weights string) gfsrs.Parameters {
	params := gfsrs.DefaultParam()
	params.RequestRetention = requestRetention
	params.MaximumInterval = float64(maximumInterval)
	params.W = [17]float64{}
	for i, w := range strings.Split(weights, ",") {
		w = strings.TrimSpace(w)
		params.W[i], _ = strconv.ParseFloat(w, 64)
	}

	return params
}

var defaultFsrsWeights = "0.5701, 1.4436, 4.1386, 10.9355, 5.1443, 1.2006, 0.8627, 0.0362, 1.629, 0.1342, 1.0166, 2.1174, 0.0839, 0.3204, 1.4676, 0.219, 2.8237"

func NewNoteApi() *NoteApi {
	var DB = storage.InitDBEngine()
	return &NoteApi{db: DB, params: NewParams(0.9, 365, defaultFsrsWeights)}
}

type NoteApi struct {
    	db      *gorm.DB
	params  gfsrs.Parameters
}

    // CreateNote 添加一张卡片。
func (api *NoteApi) CreateNote(fnote *storage.Note) *storage.Note {
	fcard := gfsrs.Card{Due: time.Now(), Stability: 0.0, Difficulty: 0.0, ElapsedDays: 0, ScheduledDays: 0, Reps: 0, Lapses: 0, LastReview: time.Now(), State: gfsrs.New}
	scard := storage.FsrsInfo{}
	scard.Card = fcard
	fnote.Fsrs = scard
	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

    // GetNote 获取一张卡片。
func (store *NoteApi) GetNoteByOrgID(orgid string) *storage.Note {
	note := &storage.Note{}
	store.db.Preload("Card").Where("orgid = ?", orgid).First(note)
	return note
}

    // UpdateNote 设置一张卡片。
func (store *NoteApi) UpdateNote(fnote *storage.Note) *storage.Note {
	n := dal.Use(store.db).Note
	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.Orgid)).Updates(fnote)
	if err != nil {
		return nil
	}
	return fnote
}

    // UpdateCardOfNote 更新一张卡片中的记录。
func (store *NoteApi) UpdateCardOfNote(fnote *storage.Note) *storage.Note {
	store.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

// RemoveNote 移除一张卡片。
func (store *NoteApi) RemoveNote(orgid string) error {
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
func (store *NoteApi) AddReviewLog(orgid string, rlog *gfsrs.ReviewLog) error {
	note := &storage.Note{}
	log := &storage.ReviewLog{}
	copier.Copy(log, rlog)
	n := dal.Use(store.db).Note
	store.db.Preload("Logs").Where("org_id = ?", orgid).First(note)

	return n.ReviewLogs.Model(note).Append(log)
}


    // 获取指定OrgId的新的卡片（制卡后没有进行过复习的卡片）。
func (store *NoteApi) GetNewNotesByOrgIDs(blockIDs []string) (ret []storage.Note) {

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
func (store *NoteApi) GetDueNotesByOrgIDs(blockIDs []string) (ret []storage.Note) {

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
func (store *NoteApi) CountNotes() int {
	return 0
}

    // Dues 获取在day 天之后以前到期的所有闪卡。
func (store *NoteApi) DueNotes(day int64) ([]*storage.Note) {
	n := dal.Use(store.db).Note
	notes, err := n.WithContext(context.Background()).Preload(n.Cards).Find()
	if err != nil {
		logger.Errorf("Get all notes failed: %v", err)
		return nil
	}
	ret := []*storage.Note{}

	for _, note := range notes {
		cardDueYear := note.Fsrs.Due.Year()
		cardDueMonth := note.Fsrs.Due.Month()
		cardDueDay := note.Fsrs.Due.Day()
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

    // Review 闪卡复习。
func (api *NoteApi) ReviewNote(orgID string, rating gfsrs.Rating) *storage.Note {

	logger.Debugf("Function Args print orgID: %s, rating: %s", orgID, rating)
	now := time.Now()
	fnote := api.GetNoteByOrgID(orgID)	
	if fnote == nil {
		logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}
		
	logger.Debugf("First find fnote: %s", spew.Sdump(fnote))

	schedulingInfo := api.params.Repeat(fnote.Fsrs.Card, now)
	updatedCard := schedulingInfo[rating].Card
	logger.Debugf("After Repeat function for now we Get: %s", spew.Sdump(schedulingInfo))
	
	rLog := schedulingInfo[rating].ReviewLog

	fnote.Fsrs.Card = updatedCard
	reviewlog := storage.ReviewLog{}
	reviewlog.ReviewLog = rLog
	fnote.ReviewLogs = append(fnote.ReviewLogs, reviewlog)
	
	logger.Debugf("After Repeat we Get fnote: %s", spew.Sdump(fnote))
	
	return api.UpdateCardOfNote(fnote)
}
