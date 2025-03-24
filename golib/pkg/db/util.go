package db

import (
	"fmt"
	"github.com/maniartech/gotime"
	"net"
	"strconv"
	"time"

	"github.com/samber/lo"
)

// get the start/end time after n day
func GetDayTime(n int64) (dayStart, dayEnd time.Time) {
	china, _ := time.LoadLocation("Asia/Shanghai")
	dueDay := time.Now().In(china).AddDate(0, 0, int(n))
	dayStart = gotime.SoD(dueDay)
	dayEnd = gotime.EoD(dueDay)
	return dayStart, dayEnd
}

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

type Result struct {
	Data interface{}
	Err  error
}

func QueryFreePort() (int64, error) {
	s, err := net.Listen("tcp", ":0")
	if err != nil {
		return -1, fmt.Errorf("could not listen TCP port 0: %v", err)
	}
	defer s.Close()
	tcpa, _ := s.Addr().(*net.TCPAddr)
	return int64(tcpa.Port), nil
}
