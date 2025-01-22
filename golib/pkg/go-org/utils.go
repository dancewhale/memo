package orgp

import (
	"github.com/emirpasic/gods/stacks/arraystack"
	"memo/pkg/logger"
	"memo/pkg/org/db"
	"memo/pkg/org/location"
	"strconv"

	"memo/cmd/libmemo/options"
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

func getWeightFromPropertyDrawer(pd *PropertyDrawer) int64 {
	weight, exist := pd.Get(options.GetPropertyWeight())
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
	source, exist := pd.Get(options.GetPropertySource())
	if !exist {
		return ""
	} else {
		return source
	}
}

func getScheduleFromPropertyDrawer(pd *PropertyDrawer) string {
	schedule, exist := pd.Get(options.GetPropertySchedule())
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

func getHeadlineProperty(headline *db.Headline, pd *PropertyDrawer) {
	if pd != nil {
		headline.Data.ID, _ = pd.Get(options.GetPropertyID())
		headline.Data.Weight = getWeightFromPropertyDrawer(pd)
		headline.Data.ScheduledType = getScheduleFromPropertyDrawer(pd)
		location := location.ParseOrgLink(getSourceFromPropertyDrawer(pd), location.SourceType)
		if location != nil {
			headline.Data.Locations = append(headline.Data.Locations, location.Get())
		}
	}
}

func getFileID(d *OrgFile) (string, error) {
	var ID string
	if len(d.Nodes) != 0 {
		for _, node := range d.Nodes {
			switch n := node.(type) {
			case Headline:
				break
			case PropertyDrawer:
				id, exist := n.Get(options.GetPropertyID())
				if exist && ID != "" {
					return "", FoundDupID
				} else if exist {
					ID = id
				}
			}
		}
	}
	if ID == "" {
		logger.Warnf("No FileID found in file %s.", d.Path)
		return "", MissFileID
	} else {
		return ID, nil
	}
}

// Get the order of headline.
func getHeadOrder(stack *arraystack.Stack, currentHead db.Headline) int {
	order := 1
	if stack.Size() == 0 {
		return order
	}
	it := stack.Iterator()
	for it.End(); it.Prev(); {
		v := it.Value()
		if v.(db.Headline).Data.Level == currentHead.Data.Level {
			order++
		} else {
			break
		}
	}
	return order
}
