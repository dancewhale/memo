package main

import (
	"memo/pkg/emodule"

	emacs "github.com/sigma/go-emacs"
        _ "github.com/sigma/go-emacs/gpl-compatible"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo-api--create-or-update-note", em.CreateOrUpdateNote, 3, "Create note, first args is card orgid string, second args is note type string, third args is note content string.", nil)
	env.RegisterFunction("memo-api--delete-note", em.DeleteNote, 1, "Delete note, first args is card orgid string.", nil)
	env.RegisterFunction("memo-api--review-note", em.ReviewNote, 2, "Review note, first args is card orgid string, second args is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-note", em.GetNote, 1, "Get note, first args is card orgid string.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (orgid, type, content).", nil)

	env.ProvideFeature("memo")
}


func main() {

}
