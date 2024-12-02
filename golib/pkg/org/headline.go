package org

import "memo/pkg/storage"

type Headline struct {
	storage.Headline
	Children []Headline
}
