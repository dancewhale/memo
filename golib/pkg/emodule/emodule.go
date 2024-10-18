package emodule

import (
	"memo/pkg/note"
	"memo/pkg/logger"
	"memo/pkg/storage"

	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)

type EModule struct {
	api  *note.NoteApi
}

func (e *EModule) Init() {
	e.api = note.NewNoteApi()
	// use for test function
}

// return (ErrMessage (value1 value2 value3 ...))
// errMessage 为nil 或者string
// evalue 代表(value1 value2 value3) 
func (e *EModule) EmacsReturn(ectx emacs.FunctionCallContext, err error, result ...emacs.Value) (emacs.List, error) {
	env := ectx.Environment()
	stdl :=	env.StdLib()
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

func (e *EModule) CreateOrUpdateNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

	orgid, err := ectx.GoStringArg(0) 
	if err != nil || orgid == "" {
		logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}

	ntype, err := ectx.GoStringArg(1)
	if err != nil {
		logger.Errorf("Pass arg type from emacs create note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}

	ncontent, err := ectx.GoStringArg(2)
	if err != nil {
		logger.Errorf("Pass arg content from emacs create note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
        note := storage.Note{Orgid: orgid, Type: ntype, Content: ncontent}

	n := e.api.CreateOrUpdateNote(&note)
	
	logger.Infof("Create note success: %s", n.Orgid)

	return e.EmacsReturn(ectx, nil, env.StdLib().T())
}

func (e *EModule) DeleteNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
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

	err = e.api.RemoveNote(orgid)
	if err != nil {
		logger.Errorf("Delete note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	logger.Infof("Delete note success: %s", orgid)


	return e.EmacsReturn(ectx, nil, env.StdLib().T())
}

func (e *EModule) GetNextReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

	fnote := e.api.GetReviewNoteByDueTime()


	return e.EmacsReturn(ectx, nil, env.String(fnote.Orgid), env.String(fnote.Type), env.String(fnote.Content))
}

func (e *EModule) GetNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg orgid from emacs in GetNote failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if orgid == "" {
		error := logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(ectx, error)
	}

	fnote := e.api.GetNoteByOrgID(orgid)
	logger.Infof("Get note success: %s", fnote.Orgid)


	return e.EmacsReturn(ectx, nil, env.String(fnote.Orgid), env.String(fnote.Type), env.String(fnote.Content))
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

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

	r := e.api.ReviewNote(orgid, fsrsRate)
	logger.Infof("Review note success: %s", r.Orgid)


	return e.EmacsReturn(ectx, nil, env.StdLib().T())
}
