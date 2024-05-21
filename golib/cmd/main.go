package main

import (
	"github.com/spf13/pflag"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"
)


func main() {
	option := options.NewOptions()
	pflag.StringVarP(&option.ConfigPath, "config", "c", "config/config.yaml", "config path")
	pflag.Parse()

	options.Init(option.ConfigPath)
	logger.Init()

	// use for test function
	DBEngine := storage.NewDBEngine()
	card := storage.Card{}
	card.Create(DBEngine.DB)
}
