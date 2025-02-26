package parser

import (
	"memo/pkg/logger"
	"strconv"

	"github.com/google/uuid"

	"github.com/emirpasic/gods/stacks/arraystack"
	"memo/cmd/options"
	"memo/pkg/storage"
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

func getIDFromPropertyDrawer(pd *PropertyDrawer) string {
	// 如果pd ==nil,返回类似1ad3025f-c304-4227-9765-ba88905380e2 格式的随机字符串
	// 如果pd不为nil,返回pd.Get(options.EmacsPropertyID)的值
	if pd == nil {
		return uuid.New().String()
	}
	id, exist := pd.Get(options.EmacsPropertyID)
	if !exist {
		return uuid.New().String()
	}
	return id
}

func getWeightFromPropertyDrawer(pd *PropertyDrawer) int64 {
	if pd == nil {
		return 50
	}
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

func setProperty(headline *storage.Headline, key, value string) error {
	if headline == nil {
		return logger.Errorf("headline is nil")
	}
	if headline.Properties == nil {
		headline.Properties = []storage.Property{}
	}
	if key == options.EmacsPropertyID {
		headline.ID = value
		return nil
	} else if key == options.EmacsPropertySource {
		headline.Source = value
		return nil
	} else if key == options.EmacsPropertySchedule {
		headline.ScheduledType = value
		return nil
	} else if key == options.EmacsPropertyWeight {
		w, err := strconv.Atoi(value)
		if err != nil {
			return logger.Errorf("convert weight to int error: %v", err)
		}
		headline.Weight = int64(w)
		return nil
	} else {
		headline.Properties = append(headline.Properties, storage.Property{Key: key, Value: value})
		return nil
	}
}

func getPropertyDrawer(id string, pd *PropertyDrawer) []storage.Property {
	if pd == nil {
		return nil
	}
	properties := []storage.Property{}
	for _, kv := range pd.Properties {
		if kv[0] != options.EmacsPropertyID && kv[0] != options.EmacsPropertySource && kv[0] != options.EmacsPropertyWeight && kv[0] != options.EmacsPropertySchedule {
			properties = append(properties, storage.Property{HeadlineID: id, Key: kv[0], Value: kv[1]})
		}
	}
	return properties
}

func getSourceFromPropertyDrawer(pd *PropertyDrawer) string {
	if pd == nil {
		return ""
	}
	source, exist := pd.Get(options.EmacsPropertySource)
	if !exist {
		return ""
	} else {
		return source
	}
}

func getScheduleFromPropertyDrawer(pd *PropertyDrawer) string {
	if pd == nil {
		return storage.NORMAL
	}
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

func parseHeadlineProperty(h *storage.Headline, pd *PropertyDrawer) {
	h.ID = getIDFromPropertyDrawer(pd)
	h.Weight = getWeightFromPropertyDrawer(pd)
	h.ScheduledType = getScheduleFromPropertyDrawer(pd)
	h.Source = getSourceFromPropertyDrawer(pd)
	h.Properties = getPropertyDrawer(h.ID, pd)
}

// Get the order of headline.
func getHeadOrder(stack *arraystack.Stack, currentHead storage.Headline) int {
	order := 1
	if stack.Size() == 0 {
		return order
	}
	it := stack.Iterator()
	for it.Begin(); it.Next(); {
		v := it.Value()
		if v.(storage.Headline).Level == currentHead.Level {
			order++
		} else {
			break
		}
	}
	return order
}
