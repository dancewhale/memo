package db

import (
	"context"

	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
)

// 该文件中的函数用于数据库中对Headline表的操作，包括文件的创建，更新，删除等操作。

func LoadHeadlineFromDB(id string) (*storage.Headline, error) {
	r := dal.Use(storage.Engine).Headline
	headlines, err := r.WithContext(context.Background()).Where(r.OrgID.Eq(id)).Find()
	if err != nil {
		return nil, logger.Errorf("Headline search error for %s: %v", id, err)
	}
	if len(headlines) == 0 {
		return nil, nil
	} else if len(headlines) == 1 {
		return headlines[0], nil
	} else {
		return nil, logger.Errorf("Headline search error for %s: more than one record found", id)
	}
}

// 从数据库中加载file的第一层级headline数据。
// TODO: wait for fix.
func LoadHeadlinesFromDB(fileId string) ([]*storage.Headline, error) {
	r := dal.Use(storage.Engine).Headline
	headlines, err := r.WithContext(context.Background()).Where(r.OrgID.Eq(fileId)).Find()
	if err != nil {
		return nil, logger.Errorf("Headline search error for %s: %v", fileId, err)
	}
	return headlines, nil
}
