package options

import (
	"time"
)


var Config config

type config struct {
	DB         DB
	Log        Log
	Server     Server
}

type DB struct {
	DBPath          string
	ParseTime       bool
	MaxIdleConn     int
	MaxOpenConn     int
	ConnMaxLifetime time.Duration
}

type Log struct {
	Level int64
}

type Server struct {
	Host string
	Port string
}

func DBinit() {
	Config.DB = DB{
		ParseTime:       true,
		MaxIdleConn:     10,
		MaxOpenConn:     100,
		ConnMaxLifetime: 1 * time.Hour,
	}
}
