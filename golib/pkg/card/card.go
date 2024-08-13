// 该package 实现了代review 卡片的队列相关功能,包括初始化卡片,卡片的入队和出队,卡片的review等
package card

import (
	"context"
	"time"
	"fmt"

	"memo/pkg/card/utils"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"gorm.io/gorm"
	//"github.com/spewerspew/spew"
)

func NewCardApi() *CardApi {
	var DB = storage.InitDBEngine()
	return &CardApi{db: DB}
}

type CardApi struct {
	db *gorm.DB
}

// 初始化note的卡片
func (api *CardApi) InitCardOfNote(note *storage.Note) *storage.Note {

	if note.Type == storage.QuestionType {
		fmt.Println("InitCardOfNote QuestionType")
		note.Cards = utils.QueNoteToCard(note)
	} else if note.Type == storage.ClozeType {
		note.Cards = utils.ClozeNoteToCard(note)
	}
	return note
}

// 从card队列中获取一个card
func (api *CardApi) GetReviewCard() *storage.Card {
	c := dal.Use(api.db).Card
	// 获取完成card初始化的note
	card, err := c.WithContext(context.Background()).First()
	if err != nil {
		logger.Errorf("Get review card failed: %v", err)
		return nil
	}
	return card
}

// ReviewCard 评价卡片, 删除card, 更新note reviewState
// 返回card review后的note
func (api *CardApi) ReviewCard(orgid string, rate int8) *storage.Note {
	// 删除card
	c := dal.Use(api.db).Card
	info, error := c.WithContext(context.Background()).Where(c.Orgid.Eq(orgid)).Delete()
	if error != nil  {
		logger.Errorf("Delete card failed: %v, delete card %d", error, info.RowsAffected)
		return nil
	}
	logger.Debugf("Delete card %d", info.RowsAffected)

	// 更新note reviewState, 如果review 的结果比当前note reviewState 小,则更新
	// easy > good > hard > again = 1
	n := dal.Use(api.db).Note
	info, error = n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).Where(n.ReviewState.Gt(rate)).Update(n.ReviewState, rate)

	if error != nil {
		logger.Errorf("Card note info update failed: %v.", error)
		return nil
	}

	note, error := n.WithContext(context.Background()).Preload(n.Cards).Where(n.Orgid.Eq(orgid)).First()
	if error != nil {
		logger.Errorf("Note search failed: %v.", error)
		return nil
	}
	return note
}


// 对当天所有到期和已到期的卡片做判断和card 初始化处理.
// 在emacs 中的内容修改后.除非强制更新note 并初始化card,否则不会应用review 的卡片内容.
func (api *CardApi) InitTodayDueNotes(dueday int) {
	sh, _ := time.LoadLocation("Asia/Shanghai")
	year, month, day := time.Now().In(sh).Date()
	today := time.Date(year, month, day, 0, 0, 0, 0, sh).AddDate(0, 0, dueday)
	logger.Debugf("Init Card of note which due before: %s", today.String())

	n := dal.Use(api.db).Note
	notes, err := n.FindDueCard(today.String())
	if err != nil {
		logger.Errorf("Get today dued notes failed: %v", err)
	}

	if len(notes) == 0 {
		logger.Debugf("Init Card of note find 0 note due before: %s", today.String())
		return
	}

	for _, note := range notes {
		if note.ReviewState == storage.WaitReview {
			note = api.InitCardOfNote(note)
			if note.Cards != nil {
				note.ReviewState = storage.ReviewCardsReady
				api.db.Session(&gorm.Session{FullSaveAssociations: true}).Updates(note)
			}
		} else if note.ReviewState != storage.WaitReview {
			logger.Debugf("Note %s is not WaitReview, skip.", note.Orgid)
			continue
		}
	}
}


