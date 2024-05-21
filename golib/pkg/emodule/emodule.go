package emodule

import (
	"fmt"
	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)


type EModule struct {
	tmp string
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
	stdlib := env.StdLib()
	mes := fmt.Sprintf("Create card: %s, %s", font, back)
	stdlib.Message(mes)

	return env.String("Hello every one, come here."), nil
}

