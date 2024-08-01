// 该package 实现了代review 卡片的队列相关功能,包括初始化卡片,卡片的入队和出队,卡片的review等
package card

import (
	"fmt"

	"memo/pkg/storage"
	//"memo/pkg/storage/dal"
	"memo/pkg/card/utils"
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

