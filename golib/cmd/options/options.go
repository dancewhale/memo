package options

import (
	"github.com/spf13/viper"
	"go.uber.org/zap/zapcore"
	"path"
	"time"
)


type Options struct {
	ConfigPath string
}

func NewOptions() *Options {
	return &Options{}
}


var sections = make(map[string]any)
var Config config

type config struct {
	DB         DB
	Log        Log
}

type DB struct {
	DBName          string
	DBPath          string
	ParseTime       bool
	MaxIdleConn     int
	MaxOpenConn     int
	ConnMaxLifetime time.Duration
}

type Log struct {
	Level zapcore.Level
}

func Init(configPath string) {
	vp := viper.New()

	dir, file := path.Split(configPath)
	vp.SetConfigName(file)
	vp.AddConfigPath(dir)
	vp.SetConfigType("yaml")

	err := vp.ReadInConfig()
	if err != nil {
		panic(err)
	}

	err = vp.Unmarshal(&Config)
	if err != nil {
		panic(err)
	}
	InitSections()
}

func InitSections() {
	sections["DB"] = &Config.DB
	sections["Log"] = &Config.Log
}
