package emodule

import (
	"errors"
	"github.com/karrick/godirwalk"
	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
	"memo/pkg/logger"
	"memo/pkg/note"
	"memo/pkg/org"
	"memo/pkg/storage"
	"memo/pkg/util"
	"strings"
)

type EModule struct {
	napi *note.NoteApi
	hapi *org.OrgApi
	stdl emacs.StdLib
}

func (e *EModule) Init() {
	e.napi, _ = note.NewNoteApi()
	e.hapi, _ = org.NewOrgApi()
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
		return evalue, nil
	} else {
		evalue = stdl.List(result...)
		return stdl.List(stdl.Nil(), evalue), nil
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
	if fnote == nil {
		err := logger.Errorf("There is no card wait for review tody.")
		return e.EmacsReturn(ectx, err)
	}
	head, err := e.hapi.GetHeadlineByOrgID(fnote.Orgid)
	if err != nil {
		return e.EmacsReturn(ectx, err, stdl.Nil())
	}
	if head == nil {
		if fnote.Type == nil {
			return e.EmacsReturn(ectx, err, env.String(fnote.Orgid), env.String(""), env.String(""))
		} else {
			return e.EmacsReturn(ectx, err, env.String(fnote.Orgid), env.String(*(fnote.Type)), env.String(""))
		}
	}
	logger.Errorf("Get next review note success: %s", fnote.Orgid)
	return e.EmacsReturn(ectx, err, env.String(fnote.Orgid), env.String(*fnote.Type), env.String(head.Content))
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
		err = logger.Errorf("Pass arg filePath from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	force, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg force from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if force == "true" {
		_, err = e.hapi.UploadFile(filePath, true)
	} else {
		_, err = e.hapi.UploadFile(filePath, false)
	}
	if err != nil {
		logger.Errorf("Upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(ectx, err)
	}
	_, err = e.napi.ScanOrgForNoteInit()
	if err != nil {
		logger.Errorf("Scan for init card after upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(ectx, err)
	}
	return e.EmacsReturn(ectx, err)
}

func (e *EModule) UploadFilesUnderDir(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	stdl := env.StdLib()

	dirPath, err := ectx.GoStringArg(0)
	if err != nil {
		logger.Errorf("Pass arg dirPath %s from emacs in upload file in dir failed: %v", dirPath, err)
		return e.EmacsReturn(ectx, err)
	}
	force, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg force from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	var needForce bool
	if force == "true" {
		needForce = true
	} else {
		needForce = false
	}

	count, err := GetOrgFileCountInDir(dirPath)
	logger.Infof("Get org file count %d in dirPath %s", count, dirPath)
	if err != nil {
		logger.Errorf("Get org file count in dirPath %s failed: %v", dirPath, err)
		return e.EmacsReturn(ectx, err)
	}
	makeProgressReporter := stdl.Intern("make-progress-reporter")
	progressMess := "Start to upload org file under dir " + dirPath
	progressReporter, err := stdl.Funcall(makeProgressReporter, env.String(progressMess), env.Int(-1), env.Int(int64(count)))
	if err != nil {
		logger.Errorf("Progress report create failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	updateProgressReporter := stdl.Intern("progress-reporter-update")
	doneProgressReporter := stdl.Intern("progress-reporter-done")

	progressCount := 0
	err = godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				_, err = e.hapi.UploadFile(osPathname, needForce)
				if err != nil && !errors.Is(err, util.NoFileIdFoundError) {
					logger.Errorf("Upload file %s failed in upload file in dir: %v", osPathname, err)
					return err
				}
				progressCount++
				_, err := stdl.Funcall(updateProgressReporter, progressReporter, env.Int(int64(progressCount)))
				if err != nil {
					logger.Errorf("Progress report update failed: %v", err)
					return err
				}
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	if err != nil {
		logger.Errorf("Error while walking directory  for org file: %s", err)
		return e.EmacsReturn(ectx, err)
	}
	_, err = stdl.Funcall(doneProgressReporter, progressReporter)
	if err != nil {
		logger.Errorf("Progress report done failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}

	_, err = e.napi.ScanOrgForNoteInit()
	if err != nil {
		logger.Errorf("Scan for init card after upload file %s failed: %v", dirPath, err)
		return e.EmacsReturn(ectx, err)
	}
	return e.EmacsReturn(ectx, nil)
}
