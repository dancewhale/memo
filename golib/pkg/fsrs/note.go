// 本文件用于抽象最上层卡片的操作
// 操作函数定义在api 中
package fsrs

import (
	"strconv"
	"strings"
	"time"
	"errors"

	"memo/pkg/logger"

	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)

// note 描述了闪卡保存内容。
type Note struct {
	Content   string  `json:"Content",copier:"Content`
	Type      string  `json:"Type",copier:"Type`
	OrgID     string  `gorm:"unique",copier:"Orgid`
	Hash      string  `json:"Hash",copier:"Hash"`
}

func (n *Note) 	GetOrgId() string {
	return n.OrgID
}
func (n *Note) 	GetContent() string {
	return n.Content
}

func (n *Note) 	GetType() string {
	return n.Type
}

type FSRSNote struct {
	N       *Note
	C       *gfsrs.Card
	Logs    *gfsrs.ReviewLog
}

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

func (api *FSRSApi) CreateNote(note *Note) *FSRSNote {
	fnote := FSRSNote{}
	fnote.N = note
	card := gfsrs.NewCard()
	fnote.C = &card
	return api.store.CreateNote(&fnote)
}

	// 创建新卡
 func (api *FSRSApi) GetNoteByOrgID(orgid string) *FSRSNote {
	// 通过store 函数获取卡片, 返回本身内部card
	return api.store.GetNoteByOrgID(orgid)
 }
	// 更新闪卡 ID。
 func (api *FSRSApi) UpdateNote(note Note) *FSRSNote {
	// 通过store 函数更新卡片
	fnote := &FSRSNote{}
	fnote.N = &note
	return api.store.UpdateNote(fnote)
 }


func (api *FSRSApi) RemoveNote(orgid string) error {
	return api.store.RemoveNote(orgid)
}

    // Review 闪卡复习。
func (api *FSRSApi) ReviewNote(orgID string, rating Rating) error {

	now := time.Now()
	fnote := api.store.GetNoteByOrgID(orgID)	
	if fnote.N == nil {
		logger.Errorf("not found card [orgid=%s] to review", orgID)
		return errors.New("When review card, not found card.")
	}
	
	schedulingInfo := api.params.Repeat(*fnote.C, now)
	updatedCard := schedulingInfo[gfsrs.Rating(rating)].Card

	api.store.UpdateCardOfNote(fnote, updatedCard)
	
	reviewLog := schedulingInfo[gfsrs.Rating(rating)].ReviewLog

	return api.store.AddReviewLog(orgID, &reviewLog)
}


	// NextDues 返回每种评分对应的下次到期时间。
 func (api *FSRSApi) 	NextDues() map[Rating]time.Time {
	return nil
 }

	// SetNextDues 设置每种评分对应的下次到期时间。
 func (api *FSRSApi) 	SetNextDues(map[Rating]time.Time) {}

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
 func (api *FSRSApi) 	GetState() State {
	return State(Hard)
 }

	// GetLastReview 返回闪卡的最后复习时间。
 func (api *FSRSApi) 	GetLastReview() time.Time {
	return time.Now()
 }


func (card *FSRSApi) Clone() FSRSNote {
//	data, err := gulu.JSON.MarshalJSON(card)
//	if nil != err {
//		logging.LogErrorf("marshal card failed: %s", err)
//		return nil
//	}
//	ret := &FSRSNote{}
//	if err = gulu.JSON.UnmarshalJSON(data, ret); nil != err {
//		logging.LogErrorf("unmarshal card failed: %s", err)
//		return nil
//	}
//	return ret
	return FSRSNote{}
}
