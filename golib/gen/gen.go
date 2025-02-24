package main

import (
	"gorm.io/gen"
	"memo/pkg/storage"
)

func main() {

	g := gen.NewGenerator(gen.Config{
		OutPath: "../pkg/storage/dal",
		Mode:    gen.WithoutContext | gen.WithDefaultQuery,
	})

	g.ApplyBasic(storage.FsrsInfo{}, storage.ReviewLog{}, storage.Headline{}, storage.File{}, storage.Clock{}, storage.Location{}, storage.Property{})

	g.Execute()
}
