// 该package 实现了代review 卡片的队列相关功能,包括初始化卡片,卡片的入队和出队,卡片的review等
package card

import (
	"fmt"
	"context"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"memo/pkg/card/utils"
	"memo/pkg/logger"

	"gorm.io/gorm"
)


// 初始化note的卡片
func InitCardOfNote(note *storage.Note) *storage.Note {

	if note.Type == storage.QuestionType {
		fmt.Println("InitCardOfNote QuestionType")
		note.Cards = utils.QueNoteToCard(note)
	} else if note.Type == storage.ClozeType {
		note.Cards = utils.ClozeNoteToCard(note)
	}
	return note
}


func  GetReviewCard(db *gorm.DB) *storage.Card {
	c := dal.Use(db).Card
	// 获取完成card初始化的note
	card, err := c.WithContext(context.Background()).First()
	if err != nil {
		logger.Errorf("Get review card failed: %v", err)
		return nil
	}
	return card
}

// ReviewCard 评价卡片, 删除card, 更新note reviewState
func  ReviewCard(orgid string, rate string, db *gorm.DB) *storage.Note {
	// 删除card
	c := dal.Use(db).Card
	info, error := c.WithContext(context.Background()).Where(c.Orgid.Eq(orgid)).Delete()
	if error != nil || info.RowsAffected != 1 {
		logger.Errorf("Delete card failed: %v, delete card %d", error, info.RowsAffected)
		return nil
	}

	// 更新note reviewState
	n := dal.Use(db).Note
	info, error = n.WithContext(context.Background()).Where(n.Orgid.Eq(orgid)).Where(n.ReviewState .Gt(storage.RateInt(rate))).Update(n.ReviewState, storage.RateInt(rate))
}
