package emodule

import (
	"errors"
	"strings"

	"memo/cmd/libmemo/options"
	"memo/pkg/card"
	"memo/pkg/logger"
	"memo/pkg/org"
	"memo/pkg/org/parser"
	"memo/pkg/storage"

	emacs "github.com/dancewhale/go-emacs"
	_ "github.com/dancewhale/go-emacs/gpl-compatible"
	"github.com/karrick/godirwalk"
)

func apiInit(ectx emacs.FunctionCallContext) EApi {
	e := EApi{}
	e.env = ectx.Environment()
	e.stdl = e.env.StdLib()
	logger.Init()
	options.Evariable = options.Variable{Stdl: e.stdl, Env: e.env}

	e.capi, _ = card.NewCardApi()
	e.orgApi, _ = org.NewOrgApi()
	return e
}

type EApi struct {
	capi   *card.CardApi
	orgApi *org.OrgApi
	stdl   emacs.StdLib
	env    emacs.Environment
}

// 将emacs.Value 转变为list
func (e *EApi) EmacsReturn(err error, result ...emacs.Value) (emacs.Value, error) {
	var evalue emacs.List
	if err != nil {
		return e.stdl.Nil(), err
	} else if len(result) > 1 {
		evalue = e.stdl.List(result...)
		return evalue, nil
	} else if len(result) == 1 {
		return result[0], nil
	} else {
		return e.stdl.Nil(), nil
	}
}

func GetNextReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	head := e.capi.GetReviewCardByWeightDueTime()
	if head == nil {
		err := logger.Errorf("There is no card wait for review tody.")
		return e.EmacsReturn(err)
	}

	return e.EmacsReturn(nil, e.env.String(head.ID),
		e.env.Int(int64(head.Weight)),
		e.env.String(head.Content),
		e.env.String(head.File.FilePath),
		e.env.String(head.Source),
	)
}

func ReviewNote(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg orgid from emacs in review note failed: %v", err)
		return e.EmacsReturn(err)
	}
	if orgid == "" {
		err = logger.Errorf("Orgid in args from emacs call  is empty.")
		return e.EmacsReturn(err)
	}

	rate, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg rate from emacs in review note failed: %v", err)
		return e.EmacsReturn(err)
	}
	if rate == "" {
		err := logger.Errorf("Rate arg from emacs in review note is empty.: %v", err)
		return e.EmacsReturn(err)
	}
	fsrsRate := storage.StringToRate(rate)

	r := e.capi.ReviewCard(orgid, fsrsRate)
	logger.Infof("Review note success: %s", r.ID)

	return e.EmacsReturn(nil)
}

func UploadFile(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	filePath, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg filePath from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(err)
	}
	force, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg force from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(err)
	}
	if force == "true" {
		err = e.orgApi.UploadFile(filePath, true)
	} else {
		err = e.orgApi.UploadFile(filePath, false)
	}
	if err != nil {
		err = logger.Errorf("Upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(err)
	}
	_, err = e.capi.ScanHeadlineInitFsrs()
	if err != nil {
		err = logger.Errorf("Scan for init card after upload file %s failed: %v", filePath, err)
		return e.EmacsReturn(err)
	}
	return e.EmacsReturn(err)
}

func UploadFilesUnderDir(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	dirPath, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg dirPath %s from emacs in upload file in dir failed: %v", dirPath, err)
		return e.EmacsReturn(err)
	}
	force, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg force from emacs in UploadFile failed: %v", err)
		return e.EmacsReturn(err)
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
		return e.EmacsReturn(err)
	}
	makeProgressReporter := e.stdl.Intern("make-progress-reporter")
	progressMess := "Start to upload org file under dir " + dirPath
	logger.Infof(progressMess)
	progressReporter, err := e.stdl.Funcall(makeProgressReporter, e.env.String(progressMess), e.env.Int(-1), e.env.Int(int64(count)))
	if err != nil {
		err = logger.Errorf("Progress report create failed: %v", err)
		return e.EmacsReturn(err)
	}
	updateProgressReporter := e.stdl.Intern("progress-reporter-update")
	doneProgressReporter := e.stdl.Intern("progress-reporter-done")

	progressCount := 0
	err = godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				err = e.orgApi.UploadFile(osPathname, needForce)
				if errors.Is(err, parser.MissFileID) {
					return nil
				} else if errors.Is(err, parser.FoundDupID) {
					return nil
				} else if err != nil {
					return logger.Errorf("Upload file %s failed in upload file in dir: %v", osPathname, err)
				}
				progressCount++
				_, err := e.stdl.Funcall(updateProgressReporter, progressReporter, e.env.Int(int64(progressCount)))
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
		return e.EmacsReturn(err)
	}
	_, err = e.stdl.Funcall(doneProgressReporter, progressReporter)
	if err != nil {
		err = logger.Errorf("Progress report done failed: %v", err)
		return e.EmacsReturn(err)
	}

	_, err = e.capi.ScanHeadlineInitFsrs()
	if err != nil {
		err = logger.Errorf("Scan for init card after upload file %s failed: %v", dirPath, err)
		return e.EmacsReturn(err)
	}
	return e.EmacsReturn(nil)
}

func UpdateCardContent(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg orgid from emacs in update content failed: %v", err)
		return e.EmacsReturn(err)
	}
	content, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg content from emacs in udpate content failed: %v", err)
	}
	err = e.orgApi.UpdateOrgHeadContent(orgid, content)
	return e.EmacsReturn(err)
}

func UpdateCardProperty(ectx emacs.FunctionCallContext) (emacs.Value, error) {
	e := apiInit(ectx)
	orgid, err := ectx.GoStringArg(0)
	if err != nil {
		err = logger.Errorf("Pass arg orgid from emacs in update property failed: %v", err)
		return e.EmacsReturn(err)
	}
	key, err := ectx.GoStringArg(1)
	if err != nil {
		err = logger.Errorf("Pass arg key from emacs in udpate property failed: %v", err)
	}
	value, err := ectx.GoStringArg(2)
	if err != nil {
		err = logger.Errorf("Pass arg value from emacs in udpate property failed: %v", err)
	}
	err = e.orgApi.UpdateOrgHeadProperty(orgid, key, value)
	return e.EmacsReturn(err)
}
