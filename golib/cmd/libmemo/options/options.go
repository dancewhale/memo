package options

import (
	"os"
	"time"
	"strconv"
	
	"gorm.io/gorm/logger"
)


type Config struct {
	DBPath              string
	DBMaxIdleConn       int
	DBMaxOpenConn       int
	DBConnMaxLifetime   time.Duration
	LogLevel            int
}


func (c *Config) ConfigInit() Config{
	config := Config{
		DBPath:  c.initDbPath(),
		DBMaxIdleConn:  10,
		DBMaxOpenConn:  100,
		DBConnMaxLifetime:  1 * time.Hour,
		LogLevel:  c.initLogLevel(),
	}
	return config
}


func (c *Config) initDbPath() string {
	dbpath, exist := os.LookupEnv("MEMO_DB_PATH")
	if !exist {
		dirname, err :=  os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		dbpath = dirname + "/.memo.db"
	}
	return dbpath
}

func (c *Config) initLogLevel() int {
// -1 是debug; 0 是info, 1 是warn, 2 是error
// 转变环境变量为数字
	var loglevel int
	level, exist := os.LookupEnv("MEMO_LOG_LEVEl")
	if !exist {
		loglevel = 0
	} else {
		loglevel, err := strconv.Atoi(level)
		if err != nil {
			panic(err)
		}
		return loglevel
	}
	return loglevel
}


func (c *Config) GetLogLevel() logger.LogLevel {
	var loglevel logger.LogLevel
	switch c.LogLevel {
	case -1:
		loglevel = logger.Info
	case 0:
		loglevel = logger.Warn
	case 1:
		loglevel = logger.Error
	case 2:
		loglevel = logger.Silent
	default:
		loglevel = logger.Warn
	}
	return loglevel
}
