package note

import (
	"context"
	"strconv"
	"strings"
	"time"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/jinzhu/copier"
	"github.com/maniartech/gotime"
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
	"gorm.io/gen/field"
	//	"github.com/spewerspew/spew"
	"gorm.io/gorm"
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

func NewNoteApi() (*NoteApi, error) {
	DB, err := storage.InitDBEngine()
	return &NoteApi{
		Query:  *dal.Use(DB),
		db:     DB,
		params: NewParams(0.9, 365, defaultFsrsWeights),
	}, err
}

type NoteApi struct {
	dal.Query
	db     *gorm.DB
	params gfsrs.Parameters
}

// GetNote 获取一张卡片。
func (api *NoteApi) getNoteByOrgID(orgid string) *storage.Note {
	n := api.Note
	note, err := n.WithContext(context.Background()).Preload(n.Fsrs).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		logger.Errorf("Get note by orgid failed: %v", err)
		return nil
	}
	return note
}

// UpdateNote 设置一张卡片。
func (api *NoteApi) UpdateNote(fnote *storage.Note) *storage.Note {
	n := api.Note
	_, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(fnote.Orgid)).Updates(fnote)
	if err != nil {
		logger.Errorf("Update note in database failed: %v", err)
		return nil
	}
	return fnote
}

// UpdateCardOfNote 更新一张卡片中的学习记录。
func (api *NoteApi) UpdateCardOfNote(fnote *storage.Note) *storage.Note {
	api.db.Unscoped().Model(fnote).Association("Fsrs").Unscoped().Replace(&fnote.Fsrs)
	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	return fnote
}

// RemoveNote 移除一张卡片,包括所有学习记录。
func (api *NoteApi) RemoveNote(orgid string) error {
	n := api.Note
	note, err := n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		return logger.Errorf("Remove note in db failed: %v", err)
	}
	_, error := n.Select(field.AssociationFields).Delete(note)
	return error
}

// 增加复习记录
func (api *NoteApi) AddReviewLog(orgid string, rlog *gfsrs.ReviewLog) error {
	note := &storage.Note{}
	log := &storage.ReviewLog{}
	copier.Copy(log, rlog)
	n := api.Note
	note, err := n.WithContext(context.Background()).Preload(n.ReviewLogs).Where(n.Orgid.Eq(orgid)).First()
	if err != nil {
		return logger.Errorf("Add review log in db failed: %v", err)
	}

	return n.ReviewLogs.Model(note).Append(log)
}

// CountNotes 获取卡包中的闪卡数量。
func (api *NoteApi) CountNotes() int {
	return 0
}

// 获取在之后第day天当天到期，当天应该复习的所有闪卡，+1表示明天，-1表示昨天
func (api *NoteApi) DueNotesInDay(day int64) []*storage.Note {
	n := api.Note
	f := api.FsrsInfo
	china, _ := time.LoadLocation("Asia/Shanghai")
	dueDay := time.Now().In(china).AddDate(0, 0, int(day))
	dueDayStart := gotime.SoD(dueDay)
	dueDayEnd := gotime.EoD(dueDay)
	notes, err := n.WithContext(context.Background()).Join(f, n.Orgid.EqCol(f.NoteOrgid)).Where(f.Due.Gte(dueDayStart)).Where(f.Due.Lte(dueDayEnd)).Find()
	if err != nil {
		_ = logger.Errorf("Get due notes in %d day failed: %v", day, err)
		return nil
	}
	return notes
}

// 获取已经到期且已经延期的卡片
func (api *NoteApi) DueNotesDeferred() []*storage.Note {
	n := api.Note
	f := api.FsrsInfo
	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayStart := gotime.SoD(today)
	notes, err := n.WithContext(context.Background()).Join(f, n.Orgid.EqCol(f.NoteOrgid)).Where(f.Due.Lte(todayStart)).Find()
	if err != nil {
		logger.Errorf("Get due note order by duetime failed: %v", err)
		return nil
	}
	return notes
}

// TODO: 获取过期卡片的逻辑需要修正，优先当天的卡片，之后是延期的卡片
func (api *NoteApi) GetReviewNoteByDueTime() *storage.Note {
	notes := api.DueNotesDeferred()
	if len(notes) == 0 {
		logger.Infof("No note need to review.")
		return nil
	}
	return notes[0]
}

// 给指定过期的闪卡打分进行review,返回复习后的闪卡
func (api *NoteApi) ReviewNote(orgID string, rating gfsrs.Rating) *storage.Note {

	logger.Debugf("Start Review Note with orgID: %s, rating: %d", orgID, rating)
	now := time.Now()
	fnote := api.getNoteByOrgID(orgID)
	if fnote == nil {
		_ = logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}

	// review 状态为waitReview 的note, 如果评价为easy,则设置为WaitCardInit
	needReview, error := api.IfNoteIsDue(orgID)

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

// 传入参数orgid,当对应的note 卡片已经到期则返回true,否则返回false
func (api *NoteApi) IfNoteIsDue(orgid string) (bool, error) {
	n := api.Note
	f := api.FsrsInfo
	china, _ := time.LoadLocation("Asia/Shanghai")
	today := time.Now().In(china)
	todayEnd := gotime.EoD(today)
	notes, err := n.WithContext(context.Background()).Join(f, n.Orgid.EqCol(f.NoteOrgid)).Where(n.Orgid.Eq(orgid)).Where(f.Due.Lte(todayEnd)).Find()
	if err != nil {
		return false, logger.Errorf("Get due note order by duetime failed: %v", err)
	} else if len(notes) == 0 {
		return false, nil
	} else {
		return true, nil
	}
}

func (api *NoteApi) ScanOrgForNoteInit() ([]*storage.Note, error) {
	logger.Infof("Start to scan org for note init.")
	note := api.Note
	fsrsInfo := api.FsrsInfo
	//	notes, err := note.FindInitCard()
	notes, err := note.WithContext(context.Background()).LeftJoin(fsrsInfo, fsrsInfo.NoteOrgid.EqCol(note.Orgid)).Where(note.Type.IsNotNull()).Where(fsrsInfo.ID.IsNull()).Find()
	if err != nil {
		logger.Errorf("Search for note to init failed in ScanOrgForNoteInit %s.", err.Error())
		return nil, err
	}

	var fcard gfsrs.Card
	var scard storage.FsrsInfo
	for _, fnote := range notes {
		fcard = gfsrs.Card{Due: time.Now(), Stability: 0.0, Difficulty: 0.0, ElapsedDays: 0, ScheduledDays: 0, Reps: 0, Lapses: 0, LastReview: time.Now(), State: gfsrs.New}
		scard = storage.FsrsInfo{}
		scard.Card = fcard
		fnote.Fsrs = scard
		api.db.Session(&gorm.Session{FullSaveAssociations: true}).Save(fnote)
	}
	return notes, err
}
