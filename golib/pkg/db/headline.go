package db

import (
	"context"
	"errors"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/org/parser"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"strconv"

	"github.com/emirpasic/gods/maps/linkedhashmap"
	"gorm.io/gorm"
)

func NewOrgHeadlineDB() (*OrgHeadlineDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	if dal.Headline == nil {
		dal.SetDefault(db)
	}
	return &OrgHeadlineDB{db: db}, nil
}

type OrgHeadlineDB struct {
	db *gorm.DB
}

func (h *OrgHeadlineDB) UpdatePropertyByID(id string, currentProperties []storage.Property) error {
	p := dal.Property
	dbProperties, err := p.WithContext(context.Background()).Where(p.HeadlineID.Eq(id)).Find()
	if err != nil {
		return logger.Errorf("Get headline %s property error: %v", id, err)
	}
	dbHashMap := linkedhashmap.New()
	for _, dbProperty := range dbProperties {
		dbHashMap.Put(dbProperty.Key, *dbProperty)
	}
	for _, currentProperty := range currentProperties {
		if dbProperty, ok := dbHashMap.Get(currentProperty.Key); ok {
			if dbProperty.(storage.Property).Value != currentProperty.Value {
				err := h.db.Save(&currentProperty).Error
				if err != nil {
					return logger.Errorf("Update headline %s property %s error: %v", id, currentProperty.Key, err)
				}
			}
			dbHashMap.Remove(currentProperty.Key)
		} else {
			err = p.WithContext(context.Background()).Create(&currentProperty)
			if err != nil {
				return logger.Errorf("Update headline %s property %s error: %v", id, currentProperty.Key, err)
			}
		}
	}
	it := dbHashMap.Iterator()
	for it.Begin(); it.Next(); {
		prop := it.Value().(storage.Property)
		_, err := p.WithContext(context.Background()).Delete(&prop)
		if err != nil {
			return logger.Errorf("Delete headline %s property %s error: %v", id, prop.Key, err)
		}
	}
	return nil
}

func (h *OrgHeadlineDB) UpdateTagByID(id string, tags []storage.Tag) error {
	t := dal.Tag
	dbTags, err := t.WithContext(context.Background()).Where(t.HeadlineID.Eq(id)).Find()
	if err != nil {
		return logger.Errorf("Get headline %s tag error: %v", id, err)
	}
	dbHashMap := linkedhashmap.New()
	for _, dbTag := range dbTags {
		dbHashMap.Put(dbTag.Name, *dbTag)
	}
	for _, tag := range tags {
		if _, ok := dbHashMap.Get(tag.Name); ok {
			dbHashMap.Remove(tag.Name)
		} else {
			err = t.WithContext(context.Background()).Create(&tag)
			if err != nil {
				return logger.Errorf("Update headline %s tag %s error: %v", id, tag.Name, err)
			}
		}
	}
	it := dbHashMap.Iterator()
	for it.Begin(); it.Next(); {
		tag := it.Value().(storage.Tag)
		_, err := t.WithContext(context.Background()).Delete(&tag)
		if err != nil {
			return logger.Errorf("Delete headline %s tag %s error: %v", id, tag.Name, err)
		}
	}
	return nil
}

func (h *OrgHeadlineDB) UpdateClockByID(id string, clocks []*storage.Clock) error {
	c := dal.Clock
	_, err := c.WithContext(context.Background()).Where(c.HeadlineID.Eq(id)).Delete()
	if err != nil {
		return logger.Errorf("Delete headline %s clock error: %v", id, err)
	}
	if clocks != nil {
		return c.WithContext(context.Background()).Create(clocks...)
	}
	return nil
}

// 修改保存依靠双键，ID和FileID都必须存在
func (h *OrgHeadlineDB) UpdateHeadline(Data storage.Headline) error {
	err := h.UpdatePropertyByID(Data.ID, Data.Properties)
	if err != nil {
		return err
	}
	err = h.UpdateTagByID(Data.ID, Data.Tags)
	if err != nil {
		return err
	}
	err = h.UpdateClockByID(Data.ID, Data.LogBook)
	if err != nil {
		return err
	}
	err = storage.Engine.Save(&Data).Error
	return err
}

// Create headline record.
func (h *OrgHeadlineDB) Create(Data storage.Headline) error {
	headline := dal.Headline

	err := headline.WithContext(context.Background()).Create(&Data)
	if err != nil {
		return logger.Errorf("Insert headline %v error: %v", Data, err)
	}
	return nil
}

func (h *OrgHeadlineDB) Delete(Data storage.Headline) error {
	head := dal.Headline
	_, err := head.WithContext(context.Background()).Unscoped().
		Select(head.Properties.Field(), head.Tags.Field(), head.LogBook.Field()).
		Delete(&Data)

	if err != nil {
		return logger.Errorf("Delete head %s error: %v", Data.ID, err)
	}
	return nil
}

// Load all headline attach to id from database.
func (h *OrgHeadlineDB) LoadFileHeadFromDB(fileID string, filetype int) (*linkedhashmap.Map, error) {
	var headlines []*storage.Headline
	var err error
	headlinesDBCache := linkedhashmap.New()
	headline := dal.Headline

	if filetype == storage.NormalFile {
		headlines, err = headline.WithContext(context.Background()).
			Preload(headline.Properties).Preload(headline.LogBook).Preload(headline.Tags).
			Where(headline.FileID.Eq(fileID)).Find()
	} else if filetype == storage.VirtualFile {
		headlines, err = headline.WithContext(context.Background()).
			Preload(headline.Properties).Preload(headline.LogBook).Preload(headline.Tags).
			Where(headline.HeadlineID.Eq(fileID)).Find()
	}
	if err != nil {
		return nil, logger.Errorf("Headlines load for file %s error: %v", fileID, err)
	}
	for _, headline := range headlines {
		headlinesDBCache.Put(headline.ID, *headline)
	}
	return headlinesDBCache, nil
}

func (h *OrgHeadlineDB) GetHeadlineByID(id string) (*storage.Headline, error) {
	headline := dal.Headline
	heads, err := headline.WithContext(context.Background()).
		Where(headline.ID.Eq(id)).
		Preload(headline.Properties).Preload(headline.Tags).Preload(headline.LogBook).
		Find()
	if err != nil {
		return nil, logger.Errorf("Get headline by id %s error: %v", id, err)
	}
	if len(heads) == 1 {
		return heads[0], nil
	} else if len(heads) == 0 {
		return nil, nil
	} else {
		return nil, logger.Errorf("Get headline by id %s error: more than one headline found.", id)
	}
}

func (h *OrgHeadlineDB) GetHeadFilePath(id string) (string, error) {
	headline := dal.Headline
	heads, err := headline.WithContext(context.Background()).Preload(headline.File).
		Where(headline.ID.Eq(id)).Find()
	if err != nil {
		return "", logger.Errorf("Get headline by id %s error: %v", id, err)
	}
	if len(heads) == 1 {
		return heads[0].File.FilePath, nil
	} else if len(heads) == 0 {
		return "", nil
	} else {
		return "", logger.Errorf("Get headline by id %s error: more than one headline found.", id)
	}
}

func (h *OrgHeadlineDB) GetFileIDByOrgID(orgid string) (*string, error) {
	head, err := h.GetFirstHeadByOrgID(orgid)
	if err != nil {
		return nil, err
	}
	if head != nil {
		return head.FileID, nil
	} else {
		return nil, logger.Errorf("Get file by headline id %s error: file not found", orgid)
	}
}

func (h *OrgHeadlineDB) GetFirstHeadByOrgID(orgid string) (*storage.Headline, error) {
	headline := dal.Headline
	head, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(orgid)).First()
	if err != nil {
		return nil, logger.Errorf("Get headline by id %s error: %v", orgid, err)
	}
	if head.FileID != nil && *head.FileID != "" {
		return head, nil
	} else {
		if head.HeadlineID != nil && *head.HeadlineID != "" {
			head, err = headline.WithContext(context.Background()).Where(headline.ID.Eq(*head.HeadlineID)).First()
			if err != nil {
				return nil, logger.Errorf("Get first file headline by id %s error: %v", *head.HeadlineID, err)
			}
			return head, nil
		}
		return nil, logger.Errorf("Get first file headline by id %s error: head has no fileid or headlineid")
	}
}

func (h *OrgHeadlineDB) UpdateHeadlineHash(id string) error {
	head, err := h.GetHeadlineByID(id)
	if err != nil {
		return err
	}
	hash := parser.HashContent(head.String())
	if head.Hash != hash {
		hd := dal.Headline
		_, err = hd.WithContext(context.Background()).Where(hd.ID.Eq(id)).UpdateSimple(hd.Hash.Value(hash))
		if err != nil {
			return logger.Errorf("Update headline %s hash error: %v", id, err)
		}
	}
	return nil
}

func (h *OrgHeadlineDB) UpdateHeadlineContent(id, body string) error {
	headline := dal.Headline
	_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(id)).
		UpdateSimple(headline.Content.Value(body))
	if err != nil {
		return logger.Errorf("Update headline %s body content error: %v", id, err)
	}
	return nil
}

func (h *OrgHeadlineDB) GetVirtPropertyMap(headID, key string) map[string]string {
	headline := dal.Headline
	heads, err := headline.WithContext(context.Background()).Preload(headline.Properties).
		Where(headline.HeadlineID.Eq(headID)).Find()
	if err != nil {
		logger.Errorf("Get headline by id %s error: %v", headID, err)
		return nil
	}
	if len(heads) == 0 {
		logger.Errorf("Get headline by id %s error: headline not found", headID)
		return nil
	}
	properties := make(map[string]string)
	for _, head := range heads {
		if head.Properties == nil {
			logger.Infof("Get headline by id %s error: properties not found", headID)
			continue
		}
		for _, property := range head.Properties {
			if property.Key == key {
				properties[head.ID] = property.Value
			}
		}
	}
	return properties
}

func (h *OrgHeadlineDB) GetPropertyValue(headID, key string) string {
	headline := dal.Headline
	head, err := headline.WithContext(context.Background()).Preload(headline.Properties).
		Where(headline.ID.Eq(headID)).First()
	if err != nil {
		logger.Errorf("Get headline by id %s error: %v", headID, err)
		return ""
	}
	if head == nil {
		logger.Errorf("Get headline by id %s error: headline not found", headID)
		return ""
	}
	if head.Properties == nil {
		logger.Infof("Get headline by id %s error: properties not found", headID)
		return ""
	}
	for _, property := range head.Properties {
		if property.Key == key {
			return property.Value
		}
	}
	logger.Infof("Get headline by id %s error: property %s not found", headID, key)
	return ""
}

func (h *OrgHeadlineDB) UpdateProperty(headID, key, value string) error {
	headline := dal.Headline
	prop := dal.Property
	fileID, err := h.GetFileIDByOrgID(headID)
	if err != nil {
		return err
	}
	if key == options.EmacsPropertyID {
		return errors.New("property ID can't update direct in UpdateProperty")
	}

	if key == options.EmacsPropertyID {
		return logger.Errorf("ID is not allowed to update.")
	} else if key == options.EmacsPropertySource {
		_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headID)).
			UpdateSimple(headline.Source.Value(value))
		return err
	} else if key == options.EmacsPropertySchedule {
		_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headID)).
			UpdateSimple(headline.ScheduledType.Value(value))
		if cacheManager := GetCacheManager(); cacheManager != nil {
			cacheManager.RefreshCacheByFileID(*fileID)
		}
		return err
	} else if key == options.EmacsPropertyWeight {
		w, err := strconv.Atoi(value)
		if err != nil {
			return logger.Errorf("convert weight to int error: %v", err)
		}
		_, err = headline.WithContext(context.Background()).Where(headline.ID.Eq(headID)).
			UpdateSimple(headline.Weight.Value(int64(w)))
		return err
	} else {
		property := storage.Property{HeadlineID: headID, Key: key, Value: value}
		properties, err := prop.WithContext(context.Background()).Where(prop.HeadlineID.Eq(headID)).
			Where(prop.Key.Eq(key)).Find()
		if len(properties) == 0 {
			if value != "" {
				return prop.WithContext(context.Background()).Create(&property)
			} else {
				return nil
			}
		} else {
			if value == "" {
				_, err = prop.WithContext(context.Background()).Where(prop.HeadlineID.Eq(headID)).
					Where(prop.Key.Eq(key)).Delete()
				return err
			} else {
				_, err = prop.WithContext(context.Background()).Where(prop.HeadlineID.Eq(headID)).
					Where(prop.Key.Eq(key)).UpdateSimple(prop.Value.Value(value))
				return err
			}
		}
	}
}

func (h *OrgHeadlineDB) GetFileByHeadlineID(headlineID string) (*storage.File, error) {
	headline := dal.Headline
	file := dal.File
	head, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headlineID)).First()
	if err != nil {
		return nil, logger.Errorf("Get headline by id %s error: %v", headlineID, err)
	}
	if head.FileID != nil && *head.FileID != "" {
		f, err := file.WithContext(context.Background()).Where(file.ID.Eq(*head.FileID)).First()
		if err != nil {
			return nil, logger.Errorf("Get file by id %s in db error: %v", *head.FileID, err)
		}
		return f, nil
	} else {
		if head.HeadlineID != nil && *head.HeadlineID != "" {
			head, err = headline.WithContext(context.Background()).Where(headline.ID.Eq(*head.HeadlineID)).First()
			if err != nil {
				return nil, logger.Errorf("Get ancestor file headline by id %s error: %v", *head.HeadlineID, err)
			}
			f, err := file.WithContext(context.Background()).Where(file.ID.Eq(*head.FileID)).First()
			if err != nil {
				return nil, logger.Errorf("Get file by id %s in db error: %v", *head.FileID, err)
			}
			return f, nil
		}
		return nil, logger.Errorf("Get file by id %s error: head has no fileid or headlineid")
	}
}

func (h *OrgHeadlineDB) CreateVirtHead(headID, title, content string) (string, error) {
	head := dal.Headline
	parentHead, err := head.WithContext(context.Background()).
		Where(head.ID.Eq(headID)).
		First()
	if err != nil {
		return "", logger.Errorf("CreateVirtHead: Find parentHead with id %s error: %v", headID, err)
	}

	count, err := head.WithContext(context.Background()).
		Where(head.ParentID.Eq(headID)).Where(head.HeadlineID.IsNotNull()).Count()
	if err != nil {
		return "", logger.Errorf("Count virtual headline by parent id %s error: %v", headID, err)
	}
	var headlineID *string
	if parentHead.HeadlineID != nil || *parentHead.HeadlineID != "" {
		headlineID = parentHead.HeadlineID
	} else {
		headlineID = &parentHead.ID
	}

	headline := storage.Headline{ID: parser.GenerateID(),
		Title:         title,
		Content:       content,
		Weight:        storage.DefaultWeight,
		ParentID:      &headID,
		HeadlineID:    headlineID,
		ScheduledType: storage.NORMAL,
		Order:         int(count + 1),
	}
	headline.Hash = parser.HashContent(headline.String())

	err = head.WithContext(context.Background()).Create(&headline)
	if err != nil {
		return "", logger.Errorf("Create virtual headline by parent id failed: %v", err)
	}
	return headline.ID, err
}
