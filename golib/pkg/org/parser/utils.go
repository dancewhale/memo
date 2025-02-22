package parser

import (
	"memo/cmd/options"
	"memo/pkg/util/gods/lists/arraylist"
	"strconv"

	"memo/pkg/storage"
	"memo/pkg/util/gods/stacks/arraystack"
)

// 用于对[]org.Nodes 过滤
func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := []T{}
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func FilterContentForHeadline(node Node) bool {
	_, d := node.(Drawer)
	_, p := node.(PropertyDrawer)
	_, h := node.(Headline)
	if !d && !p && !h {
		return true
	} else {
		return false
	}
}

func getWeightFromPropertyDrawer(pd *PropertyDrawer) int64 {
	weight, exist := pd.Get(options.EmacsPropertyWeight)
	if !exist {
		return 50
	} else {
		w, err := strconv.Atoi(weight)
		if err != nil {
			return 50
		} else {
			return int64(w)
		}
	}
}

func getSourceFromPropertyDrawer(pd *PropertyDrawer) string {
	source, exist := pd.Get(options.EmacsPropertySource)
	if !exist {
		return ""
	} else {
		return source
	}
}

func getScheduleFromPropertyDrawer(pd *PropertyDrawer) string {
	schedule, exist := pd.Get(options.EmacsPropertySchedule)
	if !exist {
		return storage.NORMAL
	} else {
		switch schedule {
		case storage.NORMAL:
			return storage.NORMAL
		case storage.SUSPEND:
			return storage.SUSPEND
		case storage.POSTPONE:
			return storage.POSTPONE
		default:
			return storage.NORMAL
		}
	}
}

func getHeadlineProperty(headline *storage.Headline, pd *PropertyDrawer) {
	if pd != nil {
		headline.ID, _ = pd.Get(options.EmacsPropertyID)
		headline.Weight = getWeightFromPropertyDrawer(pd)
		headline.ScheduledType = getScheduleFromPropertyDrawer(pd)
		headline.Source = getSourceFromPropertyDrawer(pd)
	}
}

// Get the order of headline.
func getHeadOrder(stack *arraystack.Stack, currentHead storage.Headline) int {
	order := 1
	if stack.Size() == 0 {
		return order
	}
	it := stack.Iterator()
	for it.End(); it.Prev(); {
		v := it.Value()
		if v.(storage.Headline).Level == currentHead.Level {
			order++
		} else {
			break
		}
	}
	return order
}

func FindHeadByID(list *arraylist.List, id string) (*arraylist.List, int) {
	if list == nil {
		return nil, -1
	}
	it := list.Iterator()
	for it.Next() {
		if h, ok := it.Value().(Headline); ok {
			if h.ID() == id {
				return list, it.Index()
			}
			hl, index := FindHeadByID(h.Children, id)
			if hl == nil {
				continue
			} else {
				return hl, index
			}
		}
	}
	return nil, -1
}
