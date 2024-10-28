package org

import (
	"context"
	"os"

	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/niklasfasching/go-org/org"
	"gorm.io/gorm"
)

type Org struct {
	db *gorm.DB
}

func NewOrg() *Org {
	return &Org{storage.InitDBEngine()}
}

var DB = storage.InitDBEngine()

func (o *Org) UploadFile(filePath string) (bool, error) {
	f, err := os.Open(filePath)
	if err != nil {
		return false, err
	}

	doc := org.New().Parse(f, filePath)
	if doc.Error != nil {
		return false, doc.Error
	}
	sql := NewSqlWriter()
	_, err = doc.Write(sql)
	if err != nil {
		return false, err
	}

	h := dal.Use(o.db).Headline
	err = h.WithContext(context.Background()).Create(sql.Headline...)
	if err != nil {
		return false, err
	}
	return true, nil
}
