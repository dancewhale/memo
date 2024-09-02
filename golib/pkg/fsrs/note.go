package fsrs

import (
	"errors"
	"strconv"
	"strings"
	"context"
	"time"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/logger"
	
	"github.com/jinzhu/copier"	
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
	//	"github.com/spewerspew/spew"
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
func (api *NoteApi) CreateOrUpdateNote(fnote *storage.Note) *storage.Note {
	fcard := gfsrs.Card{Due: time.Now(), Stability: 0.0, Difficulty: 0.0, ElapsedDays: 0, ScheduledDays: 0, Reps: 0, Lapses: 0, LastReview: time.Now(), State: gfsrs.New}
	scard := storage.FsrsInfo{}
	scard.Card = fcard
	fnote.Fsrs = scard
	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

// GetNote 获取一张卡片。
func (store *NoteApi) GetNoteByOrgID(orgid string) *storage.Note {
	n := dal.Use(store.db).Note
	note, err := n.WithContext(context.Background()).Preload(n.Fsrs).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get note by orgid failed: %v", err)
		return nil
	}
	return note
}

// GetDueNote by due time order
func (store *NoteApi) GetDueNoteOrderByDueTime() []*storage.Note {
	torrow  := time.Now().AddDate(0, 0, 1)
	dueTimeYear := torrow.Year()
	dueTimeMonth := torrow.Month()
	dueTimeDay := torrow.Day()
	dueTime := time.Date(dueTimeYear, dueTimeMonth, dueTimeDay, 0, 0, 0, 0, time.UTC)

	n := dal.Use(store.db).Note
	notes, err := n.WithContext(context.Background()).GetNoteOrderByDueTime(dueTime.String())
	if err != nil {
		logger.Errorf("Get due note order by duetime failed: %v", err)
		return nil
	}
	return notes
}

    // UpdateNote 设置一张卡片。
func (store *NoteApi) UpdateNote(fnote *storage.Note) *storage.Note {
	n := dal.Use(store.db).Note
	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.Orgid)).Updates(fnote)
	if err != nil {
		logger.Errorf("Update note in database failed: %v", err)
		return nil
	}
	return fnote
}

    // UpdateCardOfNote 更新一张卡片中的记录。
func (store *NoteApi) UpdateCardOfNote(fnote *storage.Note) *storage.Note {
	store.db.Unscoped().Model(fnote).Association("Fsrs").Unscoped().Replace(&fnote.Fsrs)
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
	note, err := n.WithContext(context.Background()).Preload(n.ReviewLogs).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Add review log in db failed: %v", err)
		return err
	}

	return n.ReviewLogs.Model(note).Append(log)
}

// CountNotes 获取卡包中的闪卡数量。
func (store *NoteApi) CountNotes() int {
	return 0
}

    // Dues 获取在day 天之后以前到期的所有闪卡。
func (store *NoteApi) DueNotes(day int64) ([]*storage.Note) {
	n := dal.Use(store.db).Note
	notes, err := n.WithContext(context.Background()).Preload(n.Fsrs).Find()
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

// get nodes need to review
func (api *NoteApi) GetReviewNoteByDueTime() *storage.Note {
	notes := api.GetDueNoteOrderByDueTime()
	if  len(notes) == 0 {
		logger.Infof("No note need to review.")
		return nil
	}
	return notes[0]
}

    // Review 闪卡复习,返回复习后的闪卡,如果状态为WaitReviewInit,则返回未修改的note.
func (api *NoteApi) ReviewNote(orgID string, rating gfsrs.Rating) *storage.Note {

	logger.Debugf("Start Review Note with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fnote := api.GetNoteByOrgID(orgID)	
	if fnote == nil {
		logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}

	// review 状态为waitReview 的note, 如果评价为easy,则设置为WaitCardInit
	needReview, error := api.NeedReview(fnote)

	if needReview && error == nil {
		schedulingInfo := api.params.Repeat(fnote.Fsrs.Card, now)
		updatedCard := schedulingInfo[rating].Card
		
		rLog := schedulingInfo[rating].ReviewLog

		reviewlog := storage.ReviewLog{}
		reviewlog.ReviewLog = rLog

		fnote.Fsrs.Card = updatedCard
		fnote.ReviewLogs = append(fnote.ReviewLogs, reviewlog)
		return api.UpdateCardOfNote(fnote)
	}
	logger.Infof("Note %s no need to review.", orgID)
	return nil
}


func (api *NoteApi) NeedReview(note *storage.Note) (bool, error) {
	if note.Fsrs.IsEmpty() {
		logger.Errorf("Note %d has no fsrs", note.Orgid)
		return false, errors.New("Note has no fsrs info")
	}
	cardDueYear := note.Fsrs.Due.Year()
	cardDueMonth := note.Fsrs.Due.Month()
	cardDueDay := note.Fsrs.Due.Day()
	cardDue := time.Date(cardDueYear, cardDueMonth, cardDueDay, 0, 0, 0, 0, time.UTC)

	torrow  := time.Now().AddDate(0, 0, 1)
	tomorrowYear := torrow.Year()
	tomorrowMonth := torrow.Month()
	tomorrowDay := torrow.Day()
	tomorrowTime := time.Date(tomorrowYear, tomorrowMonth, tomorrowDay, 0, 0, 0, 0, time.UTC)
	
	//check  cardDue before tody 24:00
	if cardDue.Before(tomorrowTime) || cardDue.Equal(tomorrowTime) {
		return true, nil
	}
	return false, nil
}
