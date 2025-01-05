package emodule

import (
	"github.com/karrick/godirwalk"
	"memo/pkg/logger"
	"path/filepath"
)

func GetOrgFileCountInDir(dirname string) (int, error) {
	count := 0
	err := godirwalk.Walk(dirname, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			ext := filepath.Ext(osPathname)
			if ext == ".org" && !de.IsDir() {
				count++
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	if err != nil {
		logger.Errorf("Error while walking directory %s for org file: %s", dirname, err)
		return 0, err
	}
	return count, nil
}
