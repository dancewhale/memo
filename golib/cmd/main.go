package main

import (
	"context"
	"fmt"
	"log"
	"memo/cmd/options"
	"memo/pkg/card"
	"memo/pkg/util"
	"os"
	"runtime/pprof"

	"memo/pkg/logger"
	"memo/pkg/org"

	"github.com/kiwanami/go-elrpc"
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

	// construct epc server
	if con.GoPort == 0 {
		con.GoPort, err = util.QueryFreePort()
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

	cs, err := elrpc.StartClient(int(con.EmacsPort), nil)
	if err != nil {
		return logger.Errorf("Failed to start epc client: %v", err)
	}
	startCommand := fmt.Sprintf("(memo-bridge--first-start %d)", con.GoPort)
	r, err := cs.Call("eval-in-emacs", startCommand)
	if r != nil || err != nil {
		return logger.Errorf("Failed to create epc server for go in emacs: %v", err)
	}
	if cs.Stop() != nil {
		return logger.Errorf("Failed to stop epc client: %v", err)
	}

	s.Wait()
	return nil
}

func test(ctx context.Context, cmd *cli.Command) error {
	f, err := os.Create("/tmp/cpu.prof")
	if err != nil {
		log.Fatal(err)
	}
	pprof.StartCPUProfile(f)
	defer pprof.StopCPUProfile()
	logger.Init()
	api, _ := org.NewOrgApi()

	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2024-10-22.org", true)
	//err := api.UploadFile("/Users/whale/Seafile/Dropbox/roam/20200402150453-记忆力的几个本质要点.org", true)
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2021-01-05.org", false)
	//err := api.UploadFilesUnderDir("/Users/whale/Dropbox/memo/resource", true)
	result := api.UploadFile("/Users/whale/Dropbox/memo/resource/elisp-manual.org", 1)
	//err := api.UploadFile("/Users/whale/Dropbox/memo/tasks.org", false)
	//err := api.UploadFile("/Users/whale/elisp-manual.org", true)
	//err := api.UploadFile("/tmp/elisp-manual.org", true)
	if result.Err != nil {
		fmt.Println(err)
	}
	//_, err = memorg.GetFileFromHeadID("4ABF840C-3E73-42EA-805E-A616C331DFE8")
	err = api.ExportOrgFileToDisk("1ad3025f-c304-4227-9765-ba88905380e2", "/tmp/elisp-export.org")
	if err != nil {
		fmt.Println(err)
	}
	//content := "ceshiyixai\nnojbk\njasfj;luwpr"
	//err = api.UpdateOrgHeadContent("4ABF840C-3E73-42EA-805E-A616C331DFE8", content)
	result = api.UpdateOrgHeadProperty("4ABF840C-3E73-42EA-805E-A616C331DFE8", "MEMO_NOTE_TEST", "jjjxsjj")
	//capi, _ := card.NewCardApi()
	//capi.ScanHeadlineInitFsrs()
	//card := capi.GetReviewCardByWeightDueTime()
	//fmt.Println(card)
	//
	//if len(card.Locations) != 0 {
	//	lo := location.ParseLocation(card.Locations[0])
	//	l := lo.String()
	//	fmt.Println(l)
	//
	//}
	//fcard := capi.GetReviewCardByDueTime()
	//_, err := api.GetHeadlineByOrgID("752ee978-d160-4228-a545-a2967b2c7772")
	//if err != nil {
	//	fmt.Printf(err.Error(), fcard)
	//}
	//capi, err := card.NewCardApi()
	//head, err := api.GetHeadlineByOrgID("73BAF7E2-F30B-44EC-BF2D-0581C82E8712")
	//fmt.Println(head)
	//
	//fmt.Println("=======================================================\n", elapsing2.Stats(), "\n=======================================================")
	//fmt.Println(result)
	fmt.Println("jkljresult")
	return nil

	//index.Index()
}
