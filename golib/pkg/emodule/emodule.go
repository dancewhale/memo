package emodule

import (
	"errors"
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
	noteClient pb.NoteServiceClient	
	ctx context.Context
	ctxCancel context.CancelFunc
}

func (e *EModule) Init() {

	// use for test function
}

func (e *EModule) common() error{
	logger.Init()

	con, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		logger.Errorf("Server not connect: %v", err)
		return errors.New("Server did not connect")
	}
	e.con = con
	e.ctx, e.ctxCancel = context.WithTimeout(context.Background(), time.Second)

	e.noteClient = pb.NewNoteServiceClient(con)
	// Contact the server and print out its response.
	return nil

}

func (e *EModule) CreateNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()
	defer e.con.Close()
	defer e.ctxCancel()

	orgid, err := ectx.GoStringArg(0) 
	if err != nil || orgid == "" {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
		return env.Bool(false), err
	}

	ntype, err := ectx.GoStringArg(1)
	if err != nil {
		logger.Errorf("Pass arg type from emacs create note failed: %v", err)
		return env.Bool(false), err
	}

	ncontent, err := ectx.GoStringArg(2)
	if err != nil {
		logger.Errorf("Pass arg content from emacs create note failed: %v", err)
		return env.Bool(false), err
	}

	noteReq := pb.CreateNoteRequest{Orgid: orgid, Type: ntype, Content: ncontent}
	r, err := e.noteClient.CreateNote(e.ctx, &noteReq)
	if err != nil {
		logger.Infof("Create note failed: %v", err)
		return env.Bool(false), err
	}
	logger.Infof("Create note success: %s", r.GetOrgid())

	return env.Bool(true), nil
}

func (e *EModule) DeleteNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()
	defer e.con.Close()
	defer e.ctxCancel()

	noteClient := pb.NewNoteServiceClient(e.con)

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.DeleteNoteRequest{Orgid: orgid}
	r, err := noteClient.RemoveNote(e.ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Delete note success: %s", r.GetResult())


	return env.Bool(false), nil
}

func (e *EModule) GetNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()
	defer e.con.Close()
	defer e.ctxCancel()

	noteClient := pb.NewNoteServiceClient(e.con)

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.GetNoteRequest{Orgid: orgid}
	r, err := noteClient.GetNote(e.ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Get note success: %s", r.GetOrgid())


	return env.Bool(false), nil
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()
	defer e.con.Close()
	defer e.ctxCancel()

	noteClient := pb.NewNoteServiceClient(e.con)

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	noteReq := pb.ReviewNoteRequest{Orgid: orgid}
	r, err := noteClient.ReviewNote(e.ctx, &noteReq)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Review note success: %s", r.GetOrgid())


	return env.Bool(false), nil
}
