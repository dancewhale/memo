package options

import (
	log "gorm.io/gorm/logger"
	"os"
	"time"
)

var (
	EmacsPropertyID       = "ID"
	EmacsPropertySource   = "MEMO_NOTE_SOURCE"
	EmacsPropertyWeight   = "MEMO_NOTE_WEIGHT"
	EmacsPropertySchedule = "MEMO_NOTE_SCHEDULE"
)

var Config *config

func ConfigInit() *config {
	if Config != nil {
		return Config
	} else {
		Config = &config{
			DBParseTime:       true,
			DBMaxIdleConn:     10,
			DBMaxOpenConn:     100,
			DBConnMaxLifetime: 1 * time.Hour,
		}
		return Config
	}
}

type config struct {
	DBDirPath         string
	DBParseTime       bool
	DBMaxIdleConn     int
	DBMaxOpenConn     int
	DBConnMaxLifetime time.Duration
	LogLevel          int64
	Host              string
	Port              int64
}

func (c *config) GetDBLogLevel() log.LogLevel {
	level := c.LogLevel
	switch level {
	case -1:
		return log.Info
	case 0:
		return log.Warn
	case 1:
		return log.Error
	case 2:
		return log.Silent
	default:
		return log.Warn
	}
}

func (c *config) GetLogLevel() int {
	return int(c.LogLevel)
}

func (c *config) GetDBPath() string {
	var err error
	dbPath := Config.DBDirPath
	if dbPath == "" {
		dbPath, err = os.UserHomeDir()
		dbPath = dbPath + "/.mcache"
	}

	err = os.MkdirAll(dbPath, os.ModePerm)
	if err != nil {
		return ""
	}

	return dbPath
}
