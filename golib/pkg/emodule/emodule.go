package emodule

import (
	"errors"
	"context"
	"time"

	"memo/pkg/note"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type EModule struct {
	api  note.NoteApi
}

func (e *EModule) Init() {

	// use for test function
}

func (e *EModule) common() {
	e.api = note.NewNoteApi()
}

func (e *EModule) CreateOrUpdateNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()

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
        note := storage.Note{Orgid: orgid, Type: ntype, Content: ncontent}

	n := e.api.CreateOrUpdateNote{&note}
	
	logger.Infof("Create note success: %s", n.ID )

	return env.Bool(true), nil
}

func (e *EModule) DeleteNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	err := e.api.RemoveNote(orgid)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return env.Bool(false), nil
	}
	logger.Infof("Delete note success: %s", orgid)


	return env.Bool(false), nil
}

func (e *EModule) GetNextReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()

	fnote := e.api.GetReviewNoteByDueTime()
	reviewNote := env.StdLib().List(env.String(fnote.Orgid), env.String(fnote.Type), env.String(fnote.Content))


	return reviewNote, nil
}

func (e *EModule) GetNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	fnote := e.api.GetNoteByOrgID(orgid)
	logger.Infof("Get note success: %s", fnote.Orgid)


	return env.Bool(false), nil
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	e.common()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in review note failed: %v", err)
	}
	if orgid == "" {
		return env.Bool(false), nil
	}

	rate, err := ectx.GoStringArg(1)
	if err != nil {
		logger.Errorf("Pass arg rate from emacs in review note failed: %v", err)
	}
	if rate == "" {
		return env.Bool(false), nil
	}
	rate := storage.StringToRate(rate)

	r := e.api.ReviewNote(orgid, rate)
	logger.Infof("Review note success: %s", orgid)


	return env.Bool(false), nil
}
