package main

import (
	"fmt"
	"log"
	
	"github.com/spf13/pflag"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	log.Println("module initialization started")

	stdlib := env.StdLib()
	stdlib.Message("hello from go init")

	log.Println("creating native function")
	helloFunc := env.MakeFunction(Hello, 1, "hello", nil)

	log.Println("creating symbol")
	helloSym := stdlib.Intern("hello")

	log.Println("calling function")
	stdlib.Funcall(helloFunc, env.String("function"))

	log.Println("calling symbol before it's bound")
	_, err := stdlib.Funcall(helloSym, env.String("symbol"))
	if err != nil {
		fmt.Println(err)
	}

	log.Println("binding symbol to function")
	stdlib.Fset(helloSym, helloFunc)

	log.Println("calling symbol after it's bound")
	stdlib.Funcall(helloSym, env.String("symbol"))

	stdlib.Provide(helloSym)
	log.Println("module initialization complete")
}

// Hello is a sample function that calls "message"
func Hello(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ctx.Environment()
	stdlib := ctx.Environment().StdLib()

	// we're guaranteed to be called with 1 argument
	s, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}

	messages := make(chan string)
	go func() { messages <- s }()

	stdlib.Message(fmt.Sprintf("Hello %s!", <-messages))
	return env.String("Hello every one, come here."), nil
}

func test() {
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

func main() {}
