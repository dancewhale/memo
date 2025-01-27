package parser

import "errors"

var (
	FoundDupID    = errors.New("Found duplicate ID.")
	MissFileID    = errors.New("Miss file id.")
	FileExistInKv = errors.New("File exist in kv db.")
)
