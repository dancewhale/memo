package org

import "errors"

var (
	FoundDupID = errors.New("Found duplicate ID.")
	MissFileID = errors.New("Miss file id.")
)
