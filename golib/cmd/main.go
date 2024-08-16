package main

import (
	"log"
	"memo/cmd/options"
	"memo/pkg/api"
	"memo/pkg/logger"
	"memo/pkg/emodule"
	"net"
	"os"

	emacs "github.com/sigma/go-emacs"
        _ "github.com/sigma/go-emacs/gpl-compatible"
	"github.com/urfave/cli/v2"
	"google.golang.org/grpc"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo--create-note", em.CreateNote, 3, "Create note, first args is card orgid string, second args is note type string, third args is note content string.", nil)
	env.RegisterFunction("memo--delete-note", em.DeleteNote, 1, "Delete note, first args is card orgid string.", nil)
	env.RegisterFunction("memo--review-note", em.ReviewNote, 1, "Review note, first args is card orgid string.", nil)
	env.RegisterFunction("memo--get-note", em.GetNote, 1, "Get note, first args is card orgid string.", nil)
	env.RegisterFunction("memo--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (orgid, type, content).", nil)

	env.ProvideFeature("memo")
}


func main() {
	app := &cli.App{
		Commands: []*cli.Command{
			{
				Name:    "daemon",
				Aliases: []string{"d"},
				Usage:   "start the daemon",
				Action:  appstart,
			},
		},
		Flags: []cli.Flag{
			&cli.StringFlag{
				Name:        "port",
				Value:       "50051",
				Usage:       "Port for the grpc server",
				EnvVars:     []string{"MEMO_PORT"},
				Destination: &options.Config.Server.Port,
			},
			&cli.StringFlag{
				Name:        "host",
				Value:       "0.0.0.0",
				Usage:       "Host for the grpc server",
				EnvVars:     []string{"MEMO_HOST"},
				Destination: &options.Config.Server.Host,
			},
			&cli.StringFlag{
				Name:        "dbpath",
				Value:       "",
				Usage:       "Port for the grpc server",
				EnvVars:     []string{"MEMO_DB_PATH"},
				Destination: &options.Config.DB.DBPath,
			},
			&cli.Int64Flag{
				Name:        "loglevel",
				Value:       0,
				Usage:       "Log level for server, -1 is debug, 0 is info, 1 is warn, 2 is error, 3 is dpanic, 4 is panic, 5 is fatal",
				EnvVars:     []string{"MEMO_LOG_LEVEL"},
				Destination: &options.Config.Log.Level,
			},
		},
		Action: nil,
	}

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}

func appstart(cCtx *cli.Context) error {
	logger.Init()

	options.DBinit()
	// grpc server code.
	lis, err := net.Listen("tcp", options.Config.Server.Host+":"+options.Config.Server.Port)
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
	return nil
}
