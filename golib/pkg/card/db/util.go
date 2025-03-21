package db

import (
	"strconv"

	"github.com/samber/lo"
)

func ParseIntForList(values []string) []int64 {
	return lo.Map(values, func(value string, index int) int64 {
		num, _ := strconv.ParseInt(value, 10, 64)
		return num
	})
}

func ParseStateList(values []string) []int8 {
	return lo.Map(values, func(value string, index int) int8 {
		switch value {
		case "new":
			return 0
		case "learning":
			return 1
		case "review":
			return 2
		case "relearning":
			return 3
		}
		return 0
	})
}
