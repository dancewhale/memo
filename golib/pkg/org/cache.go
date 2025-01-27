package org

import (
	"log"
	"memo/pkg/logger"

	"github.com/timshannon/badgerhold"
)

func KvInit(defaultDir string) *fileStructCache {
	options := badgerhold.DefaultOptions
	options.Dir = defaultDir
	options.ValueDir = defaultDir

	store, err := badgerhold.Open(options)
	if err != nil {
		// handle error
		log.Fatal(err)
	}
	FileCache := &fileStructCache{
		store: store,
	}
	return FileCache
}

type fileStructCache struct {
	store *badgerhold.Store
}

func (f *fileStructCache) Close() {
	f.store.Close()
}

func (f *fileStructCache) Save(orgFile *OrgFile) error {
	var file *OrgFile
	if orgFile == nil {
		return logger.Errorf("The orgFile is nil or the hash or id is empty.")
	} else if orgFile.Hash == "" {
		return logger.Errorf("The orgFile hash is empty.")
	} else if orgFile.ID == "" {
		return logger.Errorf("The orgFile id is empty.")
	}

	err := f.store.Get(orgFile.ID, &file)
	if err != nil && err != badgerhold.ErrNotFound {
		logger.Errorf("Get file with ID %s error: %v", orgFile.Hash, err)
		return err
	} else if err == badgerhold.ErrNotFound {
		err = f.store.Insert(orgFile.ID, orgFile)
		if err != nil {
			return logger.Errorf("Insert file with id %s error: %v", orgFile.ID, err)
		}
	} else if file.Hash == orgFile.Hash && file.Path == orgFile.Path {
		logger.Debugf("File with id %s is already in the cache and content is no change.", orgFile.ID)
	} else {
		err = f.store.Update(file.Path, orgFile)
		if err != nil {
			return logger.Errorf("Update file with id %s error: %v", orgFile.ID, err)
		}
	}
	return nil
}

func (f *fileStructCache) LoadFromID(id string) (*OrgFile, error) {
	var file *OrgFile
	err := f.store.Get(id, file)
	if err != nil {
		return nil, logger.Errorf("Find file with ID %s error: %v", id, err)
	}
	return file, nil
}

func (f *fileStructCache) LoadFromHash(hash string) (*OrgFile, error) {
	files := []OrgFile{}
	err := f.store.Find(&files, badgerhold.Where("Hash").Eq(hash))
	if err != nil {
		return nil, logger.Errorf("Find file with hash %s error: %v", hash, err)
	}
	if len(files) == 1 {
		return &files[0], nil
	} else {
		return nil, nil
	}
}
