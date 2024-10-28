package main

import (
	"fmt"
	emacs "github.com/sigma/go-emacs"
	_ "github.com/sigma/go-emacs/gpl-compatible"
	memorg "memo/pkg/org"

	"memo/pkg/emodule"
	"memo/pkg/logger"
)

func init() {
	logger.Init()
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo-api--create-or-update-note", em.CreateOrUpdateNote, 3, "Create note, ARG1 is card orgid string, ARG2 is note type string, ARG3 is note content string.", nil)
	env.RegisterFunction("memo-api--delete-note", em.DeleteNote, 1, "Delete note, ARG1 is card orgid string.", nil)
	env.RegisterFunction("memo-api--review-note", em.ReviewNote, 2, "Review note, ARG1 is card orgid string, ARG2 is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-note", em.GetNote, 1, "Get note,ARG1 is card orgid string.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (orgid, type, content).", nil)

	env.ProvideFeature("memo")
}

func main() {
	api := memorg.NewOrg()
	result, err := api.UploadFile("/Users/whale/Dropbox/code/go-org/test.org")
	if err != nil {
		panic(err)
	}
	fmt.Println(result)
}
