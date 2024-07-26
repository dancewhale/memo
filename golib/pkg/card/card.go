// 该package 实现了代review 卡片的队列相关功能,包括初始化卡片,卡片的入队和出队,卡片的review等
package card

import (
	"memo/pkg/fsrs"
	"memo/pkg/storage"
	//"memo/pkg/storage/dal"
	"memo/pkg/card/utils"

	"gorm.io/gorm"
)

func NewCardApi() *CardApi {
	var DB = storage.InitDBEngine()
	return &CardApi{db: DB}
}

type CardApi struct {
    	db      *gorm.DB
}

// 初始化note的卡片
func (api *CardApi) InitCardOfNote(note *storage.Note) {
	//var newCards, oldCards  []storage.Card
	//oldCards = note.Cards

	//n := dal.Use(api.db).Note
	if note.Type == fsrs.QuestionType {
		note.Cards = utils.QueNoteToCard(note)
	} else if note.Type == fsrs.ClozeType {
		note.Cards = utils.ClozeNoteToCard(note)
	}
	//api.db.Model(note).Association("Cards").Clear()
	api.db.Session(&gorm.Session{FullSaveAssociations: true}).Updates(note)
}

