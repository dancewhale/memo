package main

import (
	"gorm.io/gen"
	"memo/pkg/storage"
)

func main() {

	g := gen.NewGenerator(gen.Config{
		OutPath: "../pkg/storage/dal",
		Mode:    gen.WithoutContext|gen.WithDefaultQuery,
	})

	g.ApplyBasic(storage.Card{}, storage.Note{}, storage.FsrsInfo{}, storage.ReviewLog{})

	g.ApplyInterface(func(storage.NoteMethod) {}, storage.Note{})

	g.Execute()
}
