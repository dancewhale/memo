package emodule

import (
	"errors"
	"strings"

	"memo/pkg/card"
	"memo/pkg/logger"
	"memo/pkg/org"
	"memo/pkg/storage"

	emacs "github.com/dancewhale/go-emacs"
	_ "github.com/dancewhale/go-emacs/gpl-compatible"
	"github.com/karrick/godirwalk"
)

type EModule struct {
	capi   *card.CardApi
	orgApi *org.OrgApi
	stdl   emacs.StdLib
}

func (e *EModule) Init() {
	e.capi, _ = card.NewCardApi()
	e.orgApi, _ = org.NewOrgApi()
}

// 将emacs.Value 转变为list
func (e *EModule) EmacsReturn(ectx emacs.FunctionCallContext, err error, result ...emacs.Value) (emacs.Value, error) {
	env := ectx.Environment()
	stdl := env.StdLib()
	var evalue emacs.List
	if err != nil {
		return stdl.Nil(), err
	} else if len(result) > 1 {
		evalue = stdl.List(result...)
		return evalue, nil
	} else if len(result) == 1 {
		return result[0], nil
	} else {
		return stdl.Nil(), nil
	}
}

// TODO: 暂停卡片
func (e *EModule) HangNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()

	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg orgid from emacs in create note failed: %v", err)
	}
	if orgid == "" {
		err = logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(ectx, err)
	}

	err = e.capi.RemoveCard(orgid)
	if err != nil {
		err = logger.Errorf("Delete note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	logger.Infof("Delete note success: %s", orgid)

	return e.EmacsReturn(ectx, nil, env.StdLib().T())
}

func (e *EModule) GetNextReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	stdl := env.StdLib()
	fcard := e.capi.GetReviewCardByDueTime()
	if fcard == nil {
		err := logger.Errorf("There is no card wait for review tody.")
		return e.EmacsReturn(ectx, err)
	}
	head, err := e.orgApi.GetHeadlineByOrgID(fcard.ID)
	if err != nil {
		return e.EmacsReturn(ectx, err, stdl.Nil())
	}
	if head == nil {
		err = logger.Errorf("Get headline by orgid %s failed: %v", fcard.ID, err)
		return e.EmacsReturn(ectx, err, env.String(fcard.ID), env.String(""), env.String(""), env.String(""))
	}
	return e.EmacsReturn(ectx, nil, env.String(fcard.ID), env.String(head.Type), env.String(head.Content), env.String(head.File.FilePath))
}

func (e *EModule) ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg orgid from emacs in review note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if orgid == "" {
		err = logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(ectx, err)
	}

	rate, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg rate from emacs in review note failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	if rate == "" {
		err := logger.Errorf("Rate arg from emacs in review note is empty.: %v", err)
		return e.EmacsReturn(ectx, err)
	}
	fsrsRate := storage.StringToRate(rate)

	r := e.capi.ReviewCard(orgid, fsrsRate)
	logger.Infof("Review note success: %s", r.ID)

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
		err = e.orgApi.UploadFile(filePath, true)
	} else {
		err = e.orgApi.UploadFile(filePath, false)
	}
	if err != nil {
		err = logger.Errorf("Upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(ectx, err)
	}
	_, err = e.capi.ScanHeadlineInitFsrs()
	if err != nil {
		err = logger.Errorf("Scan for init card after upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(ectx, err)
	}
	return e.EmacsReturn(ectx, err)
}

func (e *EModule) UploadFilesUnderDir(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ectx.Environment()
	stdl := env.StdLib()

	dirPath, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg dirPath %s from emacs in upload file in dir failed: %v", dirPath, err)
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
		err = logger.Errorf("Get org file count in dirPath %s failed: %v", dirPath, err)
		return e.EmacsReturn(ectx, err)
	}
	makeProgressReporter := stdl.Intern("make-progress-reporter")
	progressMess := "Start to upload org file under dir " + dirPath
	progressReporter, err := stdl.Funcall(makeProgressReporter, env.String(progressMess), env.Int(-1), env.Int(int64(count)))
	if err != nil {
		err = logger.Errorf("Progress report create failed: %v", err)
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
				err = e.orgApi.UploadFile(osPathname, needForce)
				if errors.Is(err, org.MissFileID) {
					return nil
				} else if errors.Is(err, org.FoundDupID) {
					return nil
				} else if err != nil {
					return logger.Errorf("Upload file %s failed in upload file in dir: %v", osPathname, err)
				}
				progressCount++
				_, err := stdl.Funcall(updateProgressReporter, progressReporter, env.Int(int64(progressCount)))
				if err != nil {
					return logger.Errorf("Progress report update failed: %v", err)
				}
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	if err != nil {
		err = logger.Errorf("Error while walking directory  for org file: %s", err)
		return e.EmacsReturn(ectx, err)
	}
	_, err = stdl.Funcall(doneProgressReporter, progressReporter)
	if err != nil {
		err = logger.Errorf("Progress report done failed: %v", err)
		return e.EmacsReturn(ectx, err)
	}

	_, err = e.capi.ScanHeadlineInitFsrs()
	if err != nil {
		err = logger.Errorf("Scan for init card after upload file %s failed: %v", dirPath, err)
		return e.EmacsReturn(ectx, err)
	}
	return e.EmacsReturn(ectx, nil)
}
