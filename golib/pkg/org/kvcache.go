package org

import (
	"errors"
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

func (f *fileStructCache) Save(orgFile *OrgFile, force bool) error {
	var file *OrgFile
	if orgFile == nil {
		return logger.Errorf("The orgFile is nil or the hash or id is empty.")
	} else if orgFile.file.Hash == "" {
		return logger.Errorf("The orgFile hash is empty.")
	} else if orgFile.file.ID == "" {
		return logger.Errorf("The orgFile id is empty.")
	}

	err := f.store.Get(orgFile.file.ID, &file)
	if err != nil && err != badgerhold.ErrNotFound {
		logger.Errorf("Get file with ID %s error: %v", orgFile.file.Hash, err)
		return err
	} else if err == badgerhold.ErrNotFound {
		err = f.store.Insert(orgFile.file.ID, orgFile)
		if err != nil {
			return logger.Errorf("Insert file with id %s error: %v", orgFile.file.ID, err)
		}
	} else if file.file.Hash == orgFile.file.Hash && file.file.FilePath == orgFile.file.FilePath && !force {
		logger.Debugf("File with id %s is already in the cache and content is no change.", orgFile.file.ID)
	} else {
		err = f.store.Update(orgFile.file.ID, orgFile)
		if err != nil {
			return logger.Errorf("Update file with id %s error: %v", orgFile.file.ID, err)
		}
	}
	return nil
}

func (f *fileStructCache) LoadFromFileID(id string) (*OrgFile, error) {
	var file *OrgFile
	err := f.store.Get(id, &file)
	if err != nil && errors.Is(err, badgerhold.ErrNotFound) {
		return nil, nil
	} else if err != nil {
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
