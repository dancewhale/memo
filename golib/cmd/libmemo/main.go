package main

import (
	"fmt"
	"github.com/sigma/go-emacs"
	"memo/pkg/card"
	"memo/pkg/emodule"
	"memo/pkg/logger"

	memorg "memo/pkg/org"

	"github.com/nekomeowww/elapsing"
	_ "github.com/sigma/go-emacs/gpl-compatible"
)

func init() {
	logger.Init()
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	em := emodule.EModule{}
	em.Init()

	env.RegisterFunction("memo-api--sync-file", em.UploadFile, 2, "Upload file to database, ARG1 is filePath string, ARG2 is true/false if need force upload file.", nil)
	env.RegisterFunction("memo-api--sync-dir", em.UploadFilesUnderDir, 2, "Upload file under diretory to database, ARG1 is dirPath string, ARG2 is  true/false if need force upload files.", nil)
	env.RegisterFunction("memo-api--hang-note", em.HangNote, 1, "Hang note, ARG1 is card orgid string.", nil)
	env.RegisterFunction("memo-api--review-note", em.ReviewNote, 2, "Review note, ARG1 is card orgid string, ARG2 is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", em.GetNextReviewNote, 0, "Get next review note, return (err (orgid, type, content, file)).", nil)
	env.RegisterFunction("memo-api--progress", em.UploadFilesUnderDir, 1, "Upload org file under dir, ARG1 is dir path.", nil)

	env.ProvideFeature("memo")
}

func main() {
	logger.Init()

	api, _ := memorg.NewOrgApi()
	elapsing1 := elapsing.New()
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2024-10-22.org", true)
	//err = api.UploadFile("/Users/whale/Seafile/Dropbox/roam/20200402150453-记忆力的几个本质要点.org", true)
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2019-08-26.org", true)
	err := api.UploadFilesUnderDir("/Users/whale/Dropbox/roam/", true)
	elapsing1.StepEnds()
	fmt.Println("=======================================================\n", elapsing1.Stats(), "\n=======================================================")
	if err != nil {
		fmt.Println(err)
	}
	capi, _ := card.NewCardApi()
	capi.ScanHeadlineInitFsrs()
	_, _ = api.GetHeadlineByOrgID("752ee978-d160-4228-a545-a2967b2c7772")

	//capi, err := card.NewCardApi()
	//head, err := api.GetHeadlineByOrgID("73BAF7E2-F30B-44EC-BF2D-0581C82E8712")
	//fmt.Println(head)
	//
	//elapsing2 := elapsing.New()
	//_, err = capi.ScanOrgForCardInit()
	//elapsing2.StepEnds()
	//fmt.Println("=======================================================\n", elapsing2.Stats(), "\n=======================================================")
	//fmt.Println(result)
}
