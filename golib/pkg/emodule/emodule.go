package emodule

import (
	"context"
	"time"

	"memo/pkg/logger"
	pb "memo/proto/grpc/memo/v1"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type EModule struct {
	addr string
	con *grpc.ClientConn
}

func (e *EModule) Init() {

	// use for test function
}

func (e *EModule) CreateNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	logger.Init()

	env := ectx.Environment()

	con, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		logger.Errorf("did not connect: %v", err)
		return env.Bool(false), nil
	}
	defer con.Close()

	noteClient := pb.NewNoteServiceClient(con)
	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	orgid, err := ectx.GoStringArg(0) 
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	ntype, err := ectx.GoStringArg(1)
	if err != nil {
		logger.Errorf("Pass arg type from emacs create note failed: %v", err)
	}

	ncontent, err := ectx.GoStringArg(2)
	if err != nil {
		logger.Errorf("Pass arg content from emacs create note failed: %v", err)
	}

	noteReq := pb.CreateNoteRequest{Orgid: orgid, Type: ntype, Content: ncontent}
	r, err := noteClient.CreateNote(ctx, &noteReq)
	if err != nil {
		logger.Infof("Create note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Create note success: %s", r.GetOrgid())

	return env.Bool(false), nil
}

func (e *EModule) DeleteNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	logger.Init()

	env := ectx.Environment()

	con, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		logger.Errorf("did not connect: %v", err)
	}
	defer con.Close()

	noteClient := pb.NewNoteServiceClient(con)
	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.DeleteNoteRequest{Orgid: orgid}
	r, err := noteClient.RemoveNote(ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Delete note success: %s", r.GetResult())


	return env.Bool(false), nil
}

func (e *EModule) GetNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	logger.Init()

	env := ectx.Environment()

	con, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		logger.Errorf("did not connect: %v", err)
	}
	defer con.Close()

	noteClient := pb.NewNoteServiceClient(con)
	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.GetNoteRequest{Orgid: orgid}
	r, err := noteClient.GetNote(ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Get note success: %s", r.GetOrgid())


	return env.Bool(false), nil
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	logger.Init()

	env := ectx.Environment()

	con, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		logger.Errorf("did not connect: %v", err)
	}
	defer con.Close()

	noteClient := pb.NewNoteServiceClient(con)
	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.ReviewNoteRequest{Orgid: orgid}
	r, err := noteClient.ReviewNote(ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Review note success: %s", r.GetOrgid())


	return env.Bool(false), nil
}
