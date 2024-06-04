package main

import (
	"time"
	"net"
	"log"
	"context"
	"memo/pkg/emodule"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	card "memo/proto/grpc/memo/v1"

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

	// use for test function
	DBEngine := storage.InitDBEngine()
	note := storage.Note{Content: "test", Type: "test"}
	n := dal.Use(DBEngine).Note
        ctx, _ := context.WithTimeout(context.Background(), 5*time.Second)

	n.WithContext(ctx).Create(&note)

	// grpc server code.
	lis, err := net.Listen("tcp", ":50051")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	card.RegisterNoteServiceServer(s, &server{})
	log.Printf("server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
	
}

type server struct {
	card.UnimplementedNoteServiceServer
}

func (s *server) GetNote(ctx context.Context, in *card.GetNoteRequest) (*card.GetNoteResponse, error) {
	log.Printf("Received: %v", in.GetContent())
	return &card.GetNoteResponse{Content: "Hello " + in.GetContent()}, nil
}
