package emodule

import (
	"context"
	"time"

	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/spf13/pflag"
	"gorm.io/gorm"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)


type EModule struct {
	DB  *gorm.DB
}

func (e *EModule) Init() {
	option := options.NewOptions()
	pflag.StringVarP(&option.ConfigPath, "config", "c", "./config.yaml", "config path")
	pflag.Parse()

	options.Init(option.ConfigPath)
	logger.Init()

	// use for test function
	e.DB = storage.InitDBEngine()
}

func (e *EModule)Create_Card(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ctx.Environment()
	font, err := ctx.GoStringArg(0)
	if err != nil {
		return nil, err
	}
	back, err := ctx.GoStringArg(1)
	if err != nil {
		return nil, err
	}

	note := storage.Note{
		Front: font,
		Back: back,
	}

	DBEngine := storage.InitDBEngine()
	n := dal.Use(DBEngine).Note
        gctx, _ := context.WithTimeout(context.Background(), 5*time.Second)

	err = n.WithContext(gctx).Create(&note)

	if err != nil {
		return env.Bool(false), err
	} else {
		return env.Bool(true), nil
	}
}
