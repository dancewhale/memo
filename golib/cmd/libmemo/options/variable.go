package options

import (
	"fmt"
	"os"
	"strconv"

	"github.com/dancewhale/go-emacs"
	log "gorm.io/gorm/logger"
)

var Evariable Variable

type Variable struct {
	Stdl emacs.StdLib
	Env  emacs.Environment
}

func (v *Variable) get(name string) string {
	value, err := v.Stdl.Funcall(v.Stdl.Intern("symbol-value"), v.Stdl.Intern(name))
	if err != nil {
		return ""
	} else {
		str, err := v.Env.GoString(value)
		if err != nil {
			return ""
		} else {
			return str
		}
	}
}

// GetPropertyID get property id from emacs, if not found, return default value
func getStringValue(propertyName, defaultValue string) string {
	var result string
	if Evariable.Env != nil {
		result = Evariable.get(propertyName)
	}
	if Evariable.Env == nil || result == "" {
		result = defaultValue
	}
	return result
}

// GetPropertyID get property id from emacs, if not found, return default value
func getIntValue(propertyName string, defaultValue int) int {
	var value string
	if Evariable.Env != nil {
		value = Evariable.get(propertyName)
	}
	if Evariable.Env == nil || value == "" {
		return defaultValue
	} else {
		result, err := strconv.Atoi(value)
		if err != nil {
			fmt.Printf("Emacs variable %s is not a number, use default value %d", propertyName, defaultValue)
			return defaultValue
		}
		return result
	}
}

func GetPropertyID() string {
	return getStringValue("memo-prop-note-id", "ID")
}

func GetPropertyWeight() string {
	return getStringValue("memo-prop-note-weight", "MEMO_NOTE_WEIGHT")
}

func GetPropertySchedule() string {
	return getStringValue("memo-prop-note-schedule", "MEMO_NOTE_SCHEDULE")
}

func GetPropertySource() string {
	return getStringValue("memo-prop-note-source", "MEMO_NOTE_SOURCE")
}

func GetLogLevel() int {
	return getIntValue("memo-log-level", 0)
}

func GetDbLogLevel() log.LogLevel {
	level := GetLogLevel()
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

func GetDBFilePath() string {
	dbDir := GetDBDirPath()
	if dbDir == "" {
		return ""
	}
	return dbDir + "/memo.sqlite"
}

func GetDBDirPath() string {
	dirname, err := os.UserHomeDir()
	if err != nil {
		panic(err)
	}
	defaultPath := dirname + "/.mcache"
	dbPath := getStringValue("memo-db-directory", defaultPath)
	err = os.MkdirAll(dbPath, os.ModePerm)
	if err != nil {
		return ""
	}
	return dbPath
}

func GetDBMaxIdleConn() int {
	return getIntValue("memo-db-max-idle-conn", 10)
}

func GetDBMaxOpenConn() int {
	return getIntValue("memo-db-max-open-conn", 100)
}
