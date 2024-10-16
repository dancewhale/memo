package storage

import (
	"memo/cmd/libmemo/options"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

type DBEngine struct {
	DB *gorm.DB
}

func NewDBEngine() *DBEngine {
	return &DBEngine{InitDBEngine()}
}


func InitDBEngine() *gorm.DB {
	var err error
	var config options.Config
	c := config.ConfigInit()
	Engine, err := gorm.Open(sqlite.Open(c.DBPath), &gorm.Config{
		Logger: logger.Default.LogMode(c.GetLogLevel()),
	})
	// TODO retry
	if err != nil {
		panic(err)
	}

        Engine.AutoMigrate(&Note{}, &FsrsInfo{}, &ReviewLog{})

        sqlDB, err := Engine.DB()
	sqlDB.SetMaxIdleConns(c.DBMaxIdleConn)
	sqlDB.SetMaxOpenConns(c.DBMaxOpenConn)
	sqlDB.SetConnMaxLifetime(c.DBConnMaxLifetime)


	return Engine
}
