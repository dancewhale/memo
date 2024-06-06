package emodule

import (
	"memo/pkg/logger"
	"memo/pkg/storage"

	"gorm.io/gorm"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)


type EModule struct {
	DB  *gorm.DB
}

func (e *EModule) Init() {
	logger.Init()

	// use for test function
	e.DB = storage.InitDBEngine()
}

func (e *EModule)Create_Card(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ctx.Environment()

	return env.Bool(false), nil
}
