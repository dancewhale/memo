package emodule

import (
	"os"
)

// 以下变量用于定义emacs 中的环境变量
func GetDbPath() string {
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
