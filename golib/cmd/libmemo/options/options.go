package options

import (
	"os"
	"strconv"
	"time"

	"gorm.io/gorm/logger"
)

var EmacsVar *EmacsEnv

type EmacsEnv struct {
	MemoTypeProverty      string
	MemoIdProverty        string
	MemoDBPath            string
	MemoDBMaxIdleConn     int
	MemoDBMaxOpenConn     int
	MemoDBConnMaxLifetime time.Duration
	MemoLogLevel          int
}

func EmacsEnvInit() *EmacsEnv {
	if EmacsVar != nil {
		return EmacsVar
	}
	EmacsVar = &EmacsEnv{}
	EmacsVar.GetMemoTypeProverty()
	EmacsVar.GetMemoIdProverty()
	EmacsVar.GetMemoDBPath()
	EmacsVar.GetMemoDBMaxIdleConn()
	EmacsVar.GetMemoDBMaxOpenConn()
	EmacsVar.GetMemoDBConnMaxLifetime()
	EmacsVar.GetMemoLogLevel()
	return EmacsVar
}

func (e *EmacsEnv) GetMemoTypeProverty() string {
	e.MemoTypeProverty, _ = os.LookupEnv("MEMO_TYPE_PROVERTY")
	if e.MemoTypeProverty == "" {
		e.MemoTypeProverty = "MEMO_NOTE_TYPE"
	}
	return e.MemoTypeProverty
}

func (e *EmacsEnv) GetMemoIdProverty() string {
	e.MemoIdProverty, _ = os.LookupEnv("MEMO_ID_PROVERTY")
	if e.MemoIdProverty == "" {
		e.MemoIdProverty = "ID"
	}
	return e.MemoIdProverty
}

func (e *EmacsEnv) GetMemoDBPath() string {
	e.MemoDBPath, _ = os.LookupEnv("MEMO_DB_PATH")
	if e.MemoDBPath == "" {
		dirname, err := os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		e.MemoDBPath = dirname + "/.memo.db"
	}
	return e.MemoDBPath
}

func (e *EmacsEnv) GetMemoDBMaxIdleConn() int {
	e.MemoDBMaxIdleConn, _ = strconv.Atoi(os.Getenv("MEMO_DB_MAX_IDLE_CONN"))
	if e.MemoDBMaxIdleConn == 0 {
		e.MemoDBMaxIdleConn = 10
	}
	return e.MemoDBMaxIdleConn
}

func (e *EmacsEnv) GetMemoDBMaxOpenConn() int {
	e.MemoDBMaxOpenConn, _ = strconv.Atoi(os.Getenv("MEMO_DB_MAX_OPEN_CONN"))
	if e.MemoDBMaxOpenConn == 0 {
		e.MemoDBMaxOpenConn = 100
	}
	return e.MemoDBMaxOpenConn
}

func (e *EmacsEnv) GetMemoDBConnMaxLifetime() time.Duration {
	e.MemoDBConnMaxLifetime, _ = time.ParseDuration(os.Getenv("MEMO_DB_CONN_MAX_LIFETIME"))
	if e.MemoDBConnMaxLifetime == 0 {
		e.MemoDBConnMaxLifetime = 1 * time.Hour
	}
	return e.MemoDBConnMaxLifetime
}

func (e *EmacsEnv) GetMemoLogLevel() int {
	e.MemoLogLevel, _ = strconv.Atoi(os.Getenv("MEMO_LOG_LEVEL"))
	return e.MemoLogLevel
}

func (e *EmacsEnv) GetMemoDbLogLevel() logger.LogLevel {
	switch e.MemoLogLevel {
	case -1:
		return logger.Info
	case 0:
		return logger.Warn
	case 1:
		return logger.Error
	case 2:
		return logger.Silent
	default:
		return logger.Warn
	}
}
