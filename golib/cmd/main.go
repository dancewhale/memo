package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"memo/cmd/options"
	"memo/pkg/card"
	"memo/pkg/client"
	"memo/pkg/db"
	"memo/pkg/logger"
	"memo/pkg/org"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/dancewhale/go-elrpc"
	"github.com/urfave/cli/v3"
)

func main() {
	con := options.ConfigInit()

	app := &cli.Command{
		Commands: []*cli.Command{
			{
				Name:    "daemon",
				Aliases: []string{"d"},
				Usage:   "start the daemon",
				Action:  appstart,
			},
			{
				Name:    "test",
				Aliases: []string{"t"},
				Usage:   "start the test",
				Action:  test,
			},
		},
		Flags: []cli.Flag{
			&cli.IntFlag{
				Name:        "emacs-port",
				Usage:       "Port for the emacs grpc server.",
				Required:    true,
				Sources:     cli.EnvVars("MEMO_EMACS_PORT"),
				Destination: &con.EmacsPort,
			},
			&cli.IntFlag{
				Name:        "go-port",
				Value:       0,
				Usage:       "Port for the go grpc server.",
				Sources:     cli.EnvVars("MEMO_GO_PORT"),
				Destination: &con.GoPort,
			},
			&cli.StringFlag{
				Name:        "host",
				Value:       "0.0.0.0",
				Usage:       "Host for the grpc server",
				Sources:     cli.EnvVars("MEMO_HOST"),
				Destination: &con.Host,
			},
			&cli.StringFlag{
				Name:        "dbpath",
				Usage:       "Directory for the memo db.",
				Sources:     cli.EnvVars("MEMO_DB_PATH"),
				Destination: &con.DBDirPath,
			},
			&cli.IntFlag{
				Name:        "log-level",
				Value:       0,
				Usage:       "Log level for server, -1 is debug, 0 is info, 1 is warn, 2 is error, 3 is dpanic, 4 is panic, 5 is fatal",
				Sources:     cli.EnvVars("MEMO_LOG_LEVEL"),
				Destination: &con.LogLevel,
			},
		},
	}

	if err := app.Run(context.Background(), os.Args); err != nil {
		log.Fatal(err)
	}
}

func appstart(ctx context.Context, cmd *cli.Command) error {
	logger.Init()

	var err error
	con := options.ConfigInit()
	DB, err := storage.InitDBEngine()
	if err != nil {
		logger.Errorf("Failed to init db engine: %v", err)
	}
	dal.SetDefault(DB)

	// construct epc server
	if con.GoPort == 0 {
		con.GoPort, err = db.QueryFreePort()
	}

	if err != nil {
		return logger.Errorf("Failed to query free port: %v", err)
	}
	s, err := elrpc.StartServerWithPort(nil, int(con.GoPort))
	if err != nil {
		return logger.Errorf("Failed to start server: %v", err)
	}
	defer s.Close()

	if con.LogLevel == -1 {
		s.SetDebug(true)
	}

	// register org method
	orgApi, err := org.NewOrgApi()
	if err != nil {
		return logger.Errorf("Failed to create org api: %v", err)
	}
	orgApi.RegistryEpcMethod(s)

	// register card method
	cardApi, err := card.NewCardApi()
	if err != nil {
		return logger.Errorf("Failed to create card api: %v", err)
	}
	cardApi.RegistryEpcMethod(s)

	client.NewEmacsEpcClient(int(con.EmacsPort))
	client.EClient.RefreshEmacsGoPort(int(con.GoPort))

	s.Wait()
	return nil
}

func test(ctx context.Context, cmd *cli.Command) error {
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2024-10-22.org", true)
	//err := api.UploadFile("/Users/whale/Seafile/Dropbox/roam/20200402150453-记忆力的几个本质要点.org", true)
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2021-01-05.org", false)
	//err := api.UploadFilesUnderDir("/Users/whale/Dropbox/memo/resource", true)
	//result := api.UploadFile("/Users/whale/Dropbox/memo/resource/elisp-manual.org", 1, 0)
	//err := api.UploadFile("/Users/whale/Dropbox/memo/tasks.org", false)
	//err := api.UploadFile("/Users/whale/elisp-manual.org", true)
	//err := api.UploadFile("/tmp/elisp-manual.org", true)
	logger.Init()
	con := options.ConfigInit()
	client.NewEmacsEpcClient(int(con.EmacsPort))
	vara, err := client.EClient.GetEmacsVars("pis")
	if err != nil {
		fmt.Printf(err.Error())
		fmt.Println(vara)
	}

	//heads, err := cardDB.GetFileHasNewCard()
	//if err != nil {
	//	fmt.Println(err)
	//	fmt.Println(heads[0])
	//	return err
	//}
	cards, err := db.GetFileHasNewCard()
	if err != nil {
		fmt.Println(err)
	}
	if len(cards) > 0 {
		fmt.Println(cards[0])
	}
	total, expired, wait, review, err := db.GetFileStats("1ad3025f-c304-4227-9765-ba88905380e2")
	fmt.Println(total, expired, wait, review)

	fmt.Println("jkljresult")
	return nil

}
