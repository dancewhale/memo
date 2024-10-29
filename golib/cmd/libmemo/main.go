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

	env.RegisterFunction("memo-api--upload-file", em.UploadFile, 1, "Upload file to database, ARG1 is filePath string.", nil)
	env.RegisterFunction("memo-api--hang-note", em.HangNote, 1, "Hang note, ARG1 is card orgid string.", nil)
	env.RegisterFunction("memo-api--review-note", em.ReviewNote, 2, "Review note, ARG1 is card orgid string, ARG2 is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (err (orgid, type, content)).", nil)

	env.ProvideFeature("memo")
}

func main() {
	api := memorg.NewOrg()
	result, err := api.UploadFile("/Users/whale/Dropbox/code/go-org/test.org")
	if err != nil {
		panic(err)
	}

	napi := note.NewNoteApi()
	napi.ScanOrgForNoteInit()
	fmt.Println(result)

}
