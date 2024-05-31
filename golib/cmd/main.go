package main

import (
	"time"
	"context"
	"memo/pkg/emodule"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/spf13/pflag"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo-create-card", em.Create_Card, 2, "Create card, first args is card front string, second args is card back string", nil)

	env.ProvideFeature("memo")
}

func main() {
	option := options.NewOptions()
	pflag.StringVarP(&option.ConfigPath, "config", "c", "config/config.yaml", "config path")
	pflag.Parse()

	options.Init(option.ConfigPath)
	logger.Init()

	// use for test function
	DBEngine := storage.NewDBEngine().DB
	note := storage.Note{Front: "test", Back: "test"}
	n := dal.Use(DBEngine).Note
        ctx, _ := context.WithTimeout(context.Background(), 5*time.Second)

	n.WithContext(ctx).Create(&note)
}
