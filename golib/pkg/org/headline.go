package org

import (
	"memo/pkg/org/db"
	"memo/pkg/storage"
)

func NewHeadline(head *storage.Headline) *OrgHeadline {
	hdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return nil
	}
	return &OrgHeadline{db: hdb, Headline: head}
}

type OrgHeadline struct {
	db       *db.OrgHeadlineDB
	Headline *storage.Headline
}
