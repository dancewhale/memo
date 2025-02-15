package main

import (
	"fmt"
	"github.com/dancewhale/go-emacs"
	_ "github.com/dancewhale/go-emacs/gpl-compatible"
	"log"
	"memo/cmd/libmemo/options"
	"memo/pkg/emodule"
	"memo/pkg/logger"
	memorg "memo/pkg/org"
	"os"
	"runtime/pprof"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {

	env.RegisterFunction("memo-api--sync-file", emodule.UploadFile, 2, "Upload file to database, ARG1 is filePath string, ARG2 is true/false if need force upload file.", nil)
	env.RegisterFunction("memo-api--sync-dir", emodule.UploadFilesUnderDir, 2, "Upload file under diretory to database, ARG1 is dirPath string, ARG2 is  true/false if need force upload files.", nil)
	env.RegisterFunction("memo-api--review-note", emodule.ReviewNote, 2, "Review note, ARG1 is card orgid string, ARG2 is review options in Good, Easy, Hard, Again.", nil)
	env.RegisterFunction("memo-api--get-next-review-note", emodule.GetNextReviewNote, 0, "Get next review note, return (orgid, weight, content, filepath, source).", nil)
	env.RegisterFunction("memo-api--update-content", emodule.UpdateCardContent, 2, "Upload content of card with id, ARG1 is id and ARG2 is content.", nil)
	env.RegisterFunction("memo-api--update-property", emodule.UpdateCardProperty, 3, "Upload property of card with id, ARG1 is id and ARG2 is property key and ARG3 is property value.", nil)

	env.ProvideFeature("memo")
}

func main() {
	f, err := os.Create("/tmp/cpu.prof")
	if err != nil {
		log.Fatal(err)
	}
	pprof.StartCPUProfile(f)
	defer pprof.StopCPUProfile()
	options.Evariable = options.Variable{}
	logger.Init()
	api, _ := memorg.NewOrgApi()

	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2024-10-22.org", true)
	//err := api.UploadFile("/Users/whale/Seafile/Dropbox/roam/20200402150453-记忆力的几个本质要点.org", true)
	//err := api.UploadFile("/Users/whale/Dropbox/roam/daily/2021-01-05.org", false)
	//err := api.UploadFilesUnderDir("/Users/whale/Dropbox/memo/resource", true)
	err = api.UploadFile("/Users/whale/Dropbox/memo/resource/elisp-manual.org", true)
	//err := api.UploadFile("/Users/whale/Dropbox/memo/tasks.org", false)
	//err := api.UploadFile("/Users/whale/elisp-manual.org", true)
	//err := api.UploadFile("/tmp/elisp-manual.org", true)
	if err != nil {
		fmt.Println(err)
	}
	//_, err = memorg.GetFileFromHeadID("4ABF840C-3E73-42EA-805E-A616C331DFE8")
	//err = api.ExportOrgFileToDisk("1ad3025f-c304-4227-9765-ba88905380e2", "/tmp/elisp-export.org")
	//if err != nil {
	//	fmt.Println(err)
	//}
	//content := "ceshiyixai\nnojbk\njasfj;luwpr"
	//err = api.UpdateOrgHeadContent("4ABF840C-3E73-42EA-805E-A616C331DFE8", content)
	err = api.UpdateOrgHeadProperty("4ABF840C-3E73-42EA-805E-A616C331DFE8", "MEMO_NOTE_TEST", "jjjxsjj")
	//capi, _ := card.NewCardApi()
	//capi.ScanHeadlineInitFsrs()
	//card := capi.GetReviewCardByWeightDueTime()
	//fmt.Println(card)
	//
	//if len(card.Locations) != 0 {
	//	lo := location.ParseLocation(card.Locations[0])
	//	l := lo.String()
	//	fmt.Println(l)
	//
	//}
	//fcard := capi.GetReviewCardByDueTime()
	//_, err := api.GetHeadlineByOrgID("752ee978-d160-4228-a545-a2967b2c7772")
	//if err != nil {
	//	fmt.Printf(err.Error(), fcard)
	//}
	//capi, err := card.NewCardApi()
	//head, err := api.GetHeadlineByOrgID("73BAF7E2-F30B-44EC-BF2D-0581C82E8712")
	//fmt.Println(head)
	//
	//fmt.Println("=======================================================\n", elapsing2.Stats(), "\n=======================================================")
	//fmt.Println(result)
	fmt.Println("jkljresult")

	//index.Index()
}
