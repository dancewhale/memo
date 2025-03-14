package storage

import (
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
	"memo/cmd/options"
	mlog "memo/pkg/logger"
)

var Engine *gorm.DB

func InitDBEngine() (*gorm.DB, error) {
	if Engine != nil {
		return Engine, nil
	}
	var err error
	con := options.ConfigInit()
	mlog.Infof("Current memo log level is %d, db log level is %d, dbdir is %s", con.GetLogLevel(), con.GetDBLogLevel(), con.GetDBPath())
	if Engine == nil {
		dbPath := con.GetDBPath() + "/memo.sqlite"
		Engine, err = gorm.Open(sqlite.Open(dbPath), &gorm.Config{
			Logger: logger.Default.LogMode(options.Config.GetDBLogLevel()),
		})
		// TODO retry
		if err != nil {
			return nil, err
		}
	}
	err = Engine.AutoMigrate(&FsrsInfo{}, &ReviewLog{}, &Headline{}, &File{}, &Clock{}, &Location{}, &Property{}, &Tag{})
	if err != nil {
		return nil, err
	}

	sqlDB, err := Engine.DB()
	if err != nil {
		return nil, err
	}
	sqlDB.SetMaxIdleConns(options.Config.DBMaxIdleConn)
	sqlDB.SetMaxOpenConns(options.Config.DBMaxOpenConn)
	sqlDB.SetConnMaxLifetime(options.Config.DBConnMaxLifetime)

	return Engine, err
}
