package storage

import (
	"memo/cmd/libmemo/options"
	mlog "memo/pkg/logger"
	"time"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

var Engine *gorm.DB

func InitDBEngine() (*gorm.DB, error) {
	if Engine != nil {
		return Engine, nil
	}
	var err error
	mlog.Infof("Current memo log level is %d, db log level is %d", options.GetLogLevel(), options.GetDbLogLevel())
	if Engine == nil {
		Engine, err = gorm.Open(sqlite.Open(options.GetDBPath()), &gorm.Config{
			Logger: logger.Default.LogMode(options.GetDbLogLevel()),
		})
		// TODO retry
		if err != nil {
			return nil, err
		}
	}
	err = Engine.AutoMigrate(&FsrsInfo{}, &ReviewLog{}, &Headline{}, &File{}, &Clock{})
	if err != nil {
		return nil, err
	}

	sqlDB, err := Engine.DB()
	if err != nil {
		return nil, err
	}
	sqlDB.SetMaxIdleConns(options.GetDBMaxIdleConn())
	sqlDB.SetMaxOpenConns(options.GetDBMaxOpenConn())
	sqlDB.SetConnMaxLifetime(1 * time.Hour)

	return Engine, err
}
