package options

import (
	"time"
)


var Config config

type config struct {
	DB         DB
	LogLevel   int64
}

type DB struct {
	DBPath          string
	MaxIdleConn     int
	MaxOpenConn     int
	ConnMaxLifetime time.Duration
}


func (db *DB) DBinit() {
	db.MaxIdleConn =  10,
	db.MaxOpenConn =  100,
	db.ConnMaxLifetime = 1 * time.Hour,
	db.DBPath = db.GetDbPath()
}


func (db *DB) GetDbPath() string {
	dbpath, exit := os.LookupEnv("MEMO_DB_PATH")
	if !exit {
		dirname, err :=  os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		dbpath = dirname + "/.memo.db"
	}
	return dbpath
}
