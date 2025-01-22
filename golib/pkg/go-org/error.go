package orgp

import "errors"

var (
	FoundDupID     = errors.New("Found duplicate ID.")
	MissFileID     = errors.New("Miss file id.")
	FoundDupFileID = errors.New("Found duplicate file id.")
)
