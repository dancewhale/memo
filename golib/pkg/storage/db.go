package storage

import (
	"memo/cmd/options"

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
	DBConfig := options.Config.DB
	if DBConfig.DBPath == "" {
		DBConfig.DBPath = "./memo.sqlite"
	} else {
		DBConfig.DBPath = DBConfig.DBPath + "/memo.sqlite"
	}
	Engine, err := gorm.Open(sqlite.Open(DBConfig.DBPath), &gorm.Config{
		Logger: logger.Default.LogMode(logger.Info),
	})
	// TODO retry
	if err != nil {
		panic(err)
	}

        Engine.AutoMigrate(&Note{}, FsrsInfo{}, &ReviewLog{})

        sqlDB, err := Engine.DB()
	sqlDB.SetMaxIdleConns(DBConfig.MaxIdleConn)
	sqlDB.SetMaxOpenConns(DBConfig.MaxOpenConn)
	sqlDB.SetConnMaxLifetime(DBConfig.ConnMaxLifetime)


	return Engine
}
