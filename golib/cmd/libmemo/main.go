package main

import (
	"fmt"
	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
	"memo/pkg/emodule"
	"memo/pkg/logger"
	"memo/pkg/note"
	memorg "memo/pkg/org"
)

func init() {
	logger.Init()
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo-api--sync-file", em.UploadFile, 1, "Upload file to database, ARG1 is filePath string.", nil)
	env.RegisterFunction("memo-api--sync-dir", em.UploadFilesUnderDir, 1, "Upload file under diretory to database, ARG1 is dirPath string.", nil)
	env.RegisterFunction("memo-api--hang-note", em.HangNote, 1, "Hang note, ARG1 is card orgid string.", nil)
	env.RegisterFunction("memo-api--review-note", em.ReviewNote, 2, "Review note, ARG1 is card orgid string, ARG2 is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (err (orgid, type, content)).", nil)
	env.RegisterFunction("memo-api--progress", em.UploadFilesUnderDir, 1, "Upload org file under dir, ARG1 is dir path.", nil)

	env.ProvideFeature("memo")
}

func main() {
	api, err := memorg.NewOrgApi()
	result, err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2024-10-22.org")
	if err != nil {
		fmt.Println(err)
	}

	napi, err := note.NewNoteApi()
	_, err = napi.ScanOrgForNoteInit()
	fmt.Println(result)
}
