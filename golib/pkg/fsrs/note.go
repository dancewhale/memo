// 本文件用于抽象最上层卡片的操作
// 操作函数定义在api 中
package fsrs

import (
	"strconv"
	"strings"
	"time"
	//"errors"

	"memo/pkg/logger"
	"memo/pkg/storage"

	//import spew
	"github.com/spewerspew/spew"
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
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

func NewFsrsApi() *FSRSApi {
	// TODO: 从环境变量中读取相应的配置
	return &FSRSApi{store: NewFSRSStore(), params: NewParams(0.9, 365, defaultFsrsWeights)}
}

type FSRSApi struct {
	store  *FSRSStore
	params gfsrs.Parameters

}

func (api *FSRSApi) CreateNote(note *storage.Note) *storage.Note {
	return api.store.CreateNote(note)
}

	// 创建新卡
 func (api *FSRSApi) GetNoteByOrgID(orgid string) *storage.Note {
	// 通过store 函数获取卡片, 返回本身内部card
	return api.store.GetNoteByOrgID(orgid)
 }
	// 更新闪卡 ID。
 func (api *FSRSApi) UpdateNote(note *storage.Note) *storage.Note {
	// 通过store 函数更新卡片
	return api.store.UpdateNote(note)
 }


func (api *FSRSApi) RemoveNote(orgid string) error {
	return api.store.RemoveNote(orgid)
}

    // Review 闪卡复习。
func (api *FSRSApi) ReviewNote(orgID string, rating gfsrs.Rating) *storage.Note {

	logger.Debugf("Function Args print orgID: %s, rating: %s", orgID, rating)
	now := time.Now()
	fnote := api.store.GetNoteByOrgID(orgID)	
	if fnote == nil {
		logger.Errorf("not found card [orgid=%s] to review", orgID)
		return nil
	}
		
	logger.Debugf("First find fnote: %s", spew.Sdump(fnote))

	schedulingInfo := api.params.Repeat(fnote.Card.Card, now)
	updatedCard := schedulingInfo[rating].Card
	logger.Debugf("After Repeat function for now we Get: %s", spew.Sdump(schedulingInfo))
	
	rLog := schedulingInfo[rating].ReviewLog

	fnote.Card.Card = updatedCard
	reviewlog := storage.ReviewLog{}
	reviewlog.ReviewLog = rLog
	fnote.Logs = append(fnote.Logs, reviewlog)
	
	logger.Debugf("After Repeat we Get fnote: %s", spew.Sdump(fnote))
	
	return api.store.UpdateCardOfNote(fnote)
}

	// DueCards 返回当前和未来到期的闪卡。
 func (api *FSRSApi) DueNotes(day int64) []*storage.Note {
	// 通过store 函数获取到期卡片
	return api.store.DueNotes(day)
 }

	// NextDues 返回每种评分对应的下次到期时间。
 func (api *FSRSApi) 	NextDues() map[gfsrs.Rating]time.Time {
	return nil
 }

	// SetNextDues 设置每种评分对应的下次到期时间。
 func (api *FSRSApi) 	SetNextDues(map[gfsrs.Rating]time.Time) {}

	// SetDue 设置到期时间。
 func (api *FSRSApi) 	SetDue(time.Time) {}

	// GetLapses 返回闪卡的遗忘次数。
func (api *FSRSApi) 	GetLapses() int {
	return 0
}

	// GetReps 返回闪卡的复习次数。
func (api *FSRSApi) 	GetReps() int {
	return 0
}

	// GetState 返回闪卡状态。
 func (api *FSRSApi) 	GetState() gfsrs.State {
	return gfsrs.State(gfsrs.Hard)
 }

	// GetLastReview 返回闪卡的最后复习时间。
 func (api *FSRSApi) 	GetLastReview() time.Time {
	return time.Now()
 }


func (card *FSRSApi) Clone() storage.Note {
//	data, err := gulu.JSON.MarshalJSON(card)
//	if nil != err {
//		logging.LogErrorf("marshal card failed: %s", err)
//		return nil
//	}
//	ret := &storage.Note{}
//	if err = gulu.JSON.UnmarshalJSON(data, ret); nil != err {
//		logging.LogErrorf("unmarshal card failed: %s", err)
//		return nil
//	}
//	return ret
	return storage.Note{}
}
