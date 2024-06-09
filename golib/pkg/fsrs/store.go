package fsrs


// Rating 描述了闪卡复习的评分。
type Rating int8

const (
    Again Rating = iota + 1 // 完全不会，必须再复习一遍
    Hard                    // 有点难
    Good                    // 一般
    Easy                    // 很容易
)

// State 描述了闪卡的状态。
type State int8

const (
	New State = iota
	Learning
	Review
	Relearning
)
