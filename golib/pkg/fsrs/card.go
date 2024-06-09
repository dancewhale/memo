// 本文件用于抽象最上层卡片的操作
// 操作函数定义在api 中
package fsrs

import (
	"time"
	"strings"
	"strconv"

	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)

func InitParams(requestRetention float64, maximumInterval int, weights string) gfsrs.Parameters {
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

type FSRSNote struct {
	N *Note
	C *gfsrs.Card
}

func (c *FSRSNote) AddCard() FSRSNote {
	store := NewFSRSStore()
	return store.AddCard(*c)
}

	// 创建新卡
 func (c *FSRSNote) GetCard(Orgid string) error {
	// 通过store 函数获取卡片, 返回本身内部card
	return nil
 }
	// 返回闪卡 ID。
 func (c *FSRSNote) SetCard() error {
	// 通过store 函数更新卡片
	return nil
 }


func (c *FSRSNote) RemoveCard() {}

	// NextDues 返回每种评分对应的下次到期时间。
 func (c *FSRSNote) 	NextDues() map[Rating]time.Time {
	return nil
 }

	// SetNextDues 设置每种评分对应的下次到期时间。
 func (c *FSRSNote) 	SetNextDues(map[Rating]time.Time) {}

	// SetDue 设置到期时间。
 func (c *FSRSNote) 	SetDue(time.Time) {}

	// GetLapses 返回闪卡的遗忘次数。
func (c *FSRSNote) 	GetLapses() int {
	return 0
}

	// GetReps 返回闪卡的复习次数。
func (c *FSRSNote) 	GetReps() int {
	return 0
}

	// GetState 返回闪卡状态。
 func (c *FSRSNote) 	GetState() State {
	return State(Hard)
 }

	// GetLastReview 返回闪卡的最后复习时间。
 func (c *FSRSNote) 	GetLastReview() time.Time {
	return time.Now()
 }


func (card *FSRSNote) Clone() FSRSNote {
//	data, err := gulu.JSON.MarshalJSON(card)
//	if nil != err {
//		logging.LogErrorf("marshal card failed: %s", err)
//		return nil
//	}
//	ret := &FSRSCard{}
//	if err = gulu.JSON.UnmarshalJSON(data, ret); nil != err {
//		logging.LogErrorf("unmarshal card failed: %s", err)
//		return nil
//	}
//	return ret
	return FSRSNote{}
}
