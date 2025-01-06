package options

import (
	"fmt"
	"os"
	"strconv"

	"github.com/dancewhale/go-emacs"
	log "gorm.io/gorm/logger"
)

var Evariable *Variable

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
	if Evariable != nil {
		result = Evariable.get(propertyName)
	}
	if Evariable == nil || result == "" {
		result = defaultValue
	}
	return result
}

// GetPropertyID get property id from emacs, if not found, return default value
func getIntValue(propertyName string, defaultValue int) int {
	var value string
	if Evariable != nil {
		value = Evariable.get(propertyName)
	}
	if Evariable == nil || value == "" {
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
	return getStringValue("memo-prop-note-weight", "MEMO-NOTE-WEIGHT")
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

func GetDBPath() string {
	dirname, err := os.UserHomeDir()
	if err != nil {
		panic(err)
	}
	defaultPath := dirname + "/.memo.db"
	return getStringValue("memo-db-path", defaultPath)
}

func GetDBMaxIdleConn() int {
	return getIntValue("memo-db-max-idle-conn", 10)
}

func GetDBMaxOpenConn() int {
	return getIntValue("memo-db-max-open-conn", 100)
}
