package main

import (
	"net"
	"log"
	"memo/pkg/emodule"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/api"

	"github.com/spf13/pflag"
	"google.golang.org/grpc"

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

	// grpc server code.
	lis, err := net.Listen("tcp", ":50051")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	log.Printf("server listening at %v", lis.Addr())

        // Register all grpc api function.
	api.ApiRegister(s)

	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
