package storage

import (
	"memo/cmd/libmemo/options"
	mlog "memo/pkg/logger"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

var Engine *gorm.DB

func InitDBEngine() (*gorm.DB, error) {
	var err error
	emacsv := options.EmacsEnvInit()
	mlog.Infof("Current memo log level is %d, db log level is %d", emacsv.GetMemoLogLevel(), emacsv.GetMemoDbLogLevel())
	if Engine == nil {
		Engine, err = gorm.Open(sqlite.Open(emacsv.GetMemoDBPath()), &gorm.Config{
			Logger: logger.Default.LogMode(emacsv.GetMemoDbLogLevel()),
		})
		// TODO retry
		if err != nil {
			return nil, err
		}
	}
	err = Engine.AutoMigrate(&Note{}, &FsrsInfo{}, &ReviewLog{}, &Headline{}, &File{})
	if err != nil {
		return nil, err
	}

	sqlDB, err := Engine.DB()
	if err != nil {
		return nil, err
	}
	sqlDB.SetMaxIdleConns(emacsv.GetMemoDBMaxIdleConn())
	sqlDB.SetMaxOpenConns(emacsv.GetMemoDBMaxOpenConn())
	sqlDB.SetConnMaxLifetime(emacsv.GetMemoDBConnMaxLifetime())

	return Engine, err
}
