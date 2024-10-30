package emodule

import (
	"memo/pkg/logger"
	"memo/pkg/note"
	"memo/pkg/org"
	"memo/pkg/storage"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)

type EModule struct {
	napi *note.NoteApi
	hapi *org.OrgApi
	stdl emacs.StdLib
}

func (e *EModule) Init() {
	e.napi = note.NewNoteApi()
	e.hapi = org.NewOrg()
}

// return (ErrMessage (value1 value2 value3 ...))
// errMessage 为nil 或者string
// evalue 代表(value1 value2 value3)
func (e *EModule) EmacsReturn(ectx emacs.FunctionCallContext, err error, result ...emacs.Value) (emacs.List, error) {
	env := ectx.Environment()
	stdl := env.StdLib()
	var evalue emacs.List
	if err != nil {
		eerr := env.String(err.Error())
		evalue = stdl.List(eerr, stdl.Nil())
		return evalue, err
	} else {
		evalue = stdl.List(result...)
		return stdl.List(stdl.Nil(), evalue), err
	}
}

// TODO: 暂停卡片
func (e *EModule) HangNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if orgid == "" {
		error := logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(ectx, error)
	}

	err = e.napi.RemoveNote(orgid)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	logger.Infof("Delete note success: %s", orgid)

	return e.EmacsReturn(ectx, nil, env.StdLib().T())
}

func (e *EModule) GetNextReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	stdl := env.StdLib()
	fnote := e.napi.GetReviewNoteByDueTime()
	head, err := e.hapi.GetHeadlineByOrgID(fnote.Orgid)
	if err != nil {
		return e.EmacsReturn(ectx, err, stdl.Nil())
	}
	return e.EmacsReturn(ectx, err, env.String(*head.OrgID), env.String(*fnote.Type), env.String(head.Content))
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in review note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if orgid == "" {
		error := logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(ectx, error)
	}

	rate, err := ectx.GoStringArg(1)
	if err != nil {
		logger.Errorf("Pass arg rate from emacs in review note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if rate == "" {
		err := logger.Errorf("Rate arg from emacs in review note is empty.: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	fsrsRate := storage.StringToRate(rate)

	r := e.napi.ReviewNote(orgid, fsrsRate)
	logger.Infof("Review note success: %s", r.Orgid)

	return e.EmacsReturn(ectx, nil)
}

func (e *EModule) UploadFile(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	filePath, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg filePath from emacs in upload file failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	_, err = e.hapi.UploadFile(filePath)
	if err != nil {
		return e.EmacsReturn(ectx, err)
	}
	_, err = e.napi.ScanOrgForNoteInit()
	return e.EmacsReturn(ectx, err)
}
