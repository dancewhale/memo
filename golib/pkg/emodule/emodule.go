package emodule

import (
	"memo/pkg/storage"
	"memo/cmd/options"
	"memo/pkg/logger"

	"github.com/spf13/pflag"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)


type EModule struct {
	DB  *storage.DBEngine
}

func (e *EModule) Init() {
	option := options.NewOptions()
	pflag.StringVarP(&option.ConfigPath, "config", "c", "./config.yaml", "config path")
	pflag.Parse()

	options.Init(option.ConfigPath)
	logger.Init()

	// use for test function
	e.DB = storage.NewDBEngine()
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

	Note := storage.Note{
		Front: font,
		Back: back,
	}
	err = Note.Create(e.DB.DB)

	if err != nil {
		return env.Bool(false), err
	} else {
		return env.Bool(true), nil
	}
}

