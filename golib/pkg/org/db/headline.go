package db

import (
	"context"
	"github.com/creker/hashstructure"
	"github.com/emirpasic/gods/maps/linkedhashmap"
	"gorm.io/gorm"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/org/location"
	"memo/pkg/org/parser"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"strconv"
)

func NewOrgHeadlineDB() (*OrgHeadlineDB, error) {
	db, err := storage.InitDBEngine()
	if err != nil {
		return nil, logger.Errorf("Init db engine error: %v", err)
	}
	return &OrgHeadlineDB{query: dal.Use(db), db: db}, nil
}

type OrgHeadlineDB struct {
	query *dal.Query
	db    *gorm.DB
}

// 修改保存依靠双键，ID和FileID都必须存在
func (h *OrgHeadlineDB) UpdateByIDFile(Data storage.Headline) error {
	head := h.query.Headline
	loc := h.query.Location
	clock := h.query.Clock

	_, err := clock.WithContext(context.Background()).Unscoped().Where(clock.HeadlineID.Eq(Data.ID)).Delete()
	if err != nil {
		return logger.Errorf("Delete logbook of head %s error: %v", Data.ID, err)
	}

	// delete location of headline which type is source, and then append new location
	locs, _ := head.Locations.Where(loc.Type.Eq(string(location.SourceType))).Model(&Data).Find()
	head.Locations.Model(&Data).Delete(locs...)
	// Warn: only unattach source location, other type location may duplicate append.
	if len(Data.Locations) != 0 {
		head.Locations.Model(&Data).Append(Data.Locations...)
	}

	storage.Engine.Session(&gorm.Session{FullSaveAssociations: true}).Updates(Data)

	return nil
}

// Create headline record.
// need to deal with situation that headline unattach from file，then fileid of file is 为null
func (h *OrgHeadlineDB) Create(Data storage.Headline) error {
	headline := h.query.Headline
	result, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(Data.ID)).UpdateSimple(headline.FileID.Value(*Data.FileID))
	if err != nil {
		return logger.Errorf("Update fileid of headline %v error: %v", Data, err)
	}
	if result.RowsAffected == 0 {
		err := headline.WithContext(context.Background()).Create(&Data)
		if err != nil {
			return logger.Errorf("Insert headline %v error: %v", Data, err)
		}
	}
	return nil
}

func (h *OrgHeadlineDB) UnattachFromFile(Data storage.Headline) error {
	headline := h.query.Headline
	_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(Data.ID)).UpdateSimple(headline.FileID.Null(), headline.ParentID.Null())
	if err != nil {
		return logger.Errorf("Delete logbook of head %s error: %v", Data.ID, err)
	}
	return nil
}

// Load all headline attach to id from database.
func (h *OrgHeadlineDB) LoadFileHeadFromDB(fileID string) (*linkedhashmap.Map, error) {
	headlinesDBCache := linkedhashmap.New()
	headline := h.query.Headline
	headlines, err := headline.WithContext(context.Background()).Where(headline.FileID.Eq(fileID)).Find()
	if err != nil {
		return nil, logger.Errorf("Headlines load for file %s error: %v", fileID, err)
	}
	if len(headlines) != 0 {
		for _, headline := range headlines {
			options := hashstructure.HashOptions{IgnoreZeroValue: true, ZeroNil: true}
			hash, err := hashstructure.Hash(*headline, hashstructure.FormatV2, &options)
			if err != nil {
				return nil, logger.Errorf("Hash headline struct error: %v", err)
			}
			headline.Hash = string(hash)
			headlinesDBCache.Put(headline.ID, *headline)
		}
	}
	return headlinesDBCache, nil
}

func (h *OrgHeadlineDB) UpdateHeadlineBody(id, body string) error {
	headline := h.query.Headline
	_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(id)).UpdateSimple(headline.Content.Value(body))
	if err != nil {
		return logger.Errorf("Update headline %s body content error: %v", id, err)
	}
	return nil
}

func (h *OrgHeadlineDB) UpdateProperty(id, key, value string) error {
	headline := h.query.Headline
	if key == options.EmacsPropertyID {
		return logger.Errorf("ID is not allowed to update.")
	} else if key == options.EmacsPropertySource {
		_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(id)).UpdateSimple(headline.Source.Value(value))
		return err
	} else if key == options.EmacsPropertySchedule {
		_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(id)).UpdateSimple(headline.ScheduledType.Value(value))
		return err
	} else if key == options.EmacsPropertyWeight {
		w, err := strconv.Atoi(value)
		if err != nil {
			return logger.Errorf("convert weight to int error: %v", err)
		}
		_, err = headline.WithContext(context.Background()).Where(headline.ID.Eq(id)).UpdateSimple(headline.Weight.Value(int64(w)))
		return err
	} else {
		property := storage.Property{HeadlineID: id, Key: key, Value: value}
		err := h.db.Save(&property).Error
		return err
	}
}

func (h *OrgHeadlineDB) GetFileByHeadlineID(headlineID string) (*storage.File, error) {
	headline := h.query.Headline
	head, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headlineID)).First()
	if err != nil {
		return nil, logger.Errorf("Get headline by id %s error: %v", headlineID, err)
	}
	if head.FileID == nil {
		return nil, logger.Errorf("OrgHeadlineDB %s has no file attach to it.", headlineID)
	} else {
		file := dal.Use(storage.Engine).File
		f, err := file.WithContext(context.Background()).Where(file.ID.Eq(*head.FileID)).First()
		if err != nil {
			return nil, logger.Errorf("Get file by id %s in db error: %v", *head.FileID, err)
		}
		return f, nil
	}
}

func UpdateHeadContentByID(headlineID, bodyContent string) error {
	headline := dal.Use(storage.Engine).Headline
	_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headlineID)).UpdateSimple(headline.Content.Value(bodyContent))
	if err != nil {
		return logger.Errorf("Update headline %s body content error: %v", headlineID, err)
	}
	return nil
}

func UpdateHeadScheduleTypeByID(headlineID, stype string) error {
	headline := dal.Use(storage.Engine).Headline
	if stype == storage.POSTPONE || stype == storage.NORMAL || stype == storage.SUSPEND {
		_, err := headline.WithContext(context.Background()).Where(headline.ID.Eq(headlineID)).
			UpdateSimple(headline.ScheduledType.Value(stype))

		if err != nil {
			return logger.Errorf("Update headline %s body content error: %v", headlineID, err)
		}
		return nil
	} else {
		return logger.Errorf("Update headline %s type %s error: Only %s, %s, %s is allowed",
			headlineID, stype, storage.NORMAL, storage.POSTPONE, storage.SUSPEND)
	}
}

func (h *OrgHeadlineDB) CreateVirtualHead(parentID, title, content string) error {
	head := h.query.Headline
	source := "[[id:" + parentID + "]]"

	count, err := head.WithContext(context.Background()).Where(head.ID.Eq(parentID)).Where(head.Type.Eq(storage.VirtualHead)).Count()
	if err != nil {
		return logger.Errorf("Count virtual headline by parent id %s error: %v", parentID, err)
	}

	headline := storage.Headline{ID: parser.GenerateID(),
		ParentID: &parentID, Title: title, Content: content,
		Weight: storage.DefaultWeight, Type: storage.VirtualHead,
		Source: source, ScheduledType: storage.NORMAL,
		Order: int(count + 1),
	}

	err = head.WithContext(context.Background()).Create(&headline)
	if err != nil {
		return logger.Errorf("Create virtual headline by parent id failed: %v", err)
	}
	return err
}

func (h *OrgHeadlineDB) GetVirtualHeadByParentID(parentID string) ([]*storage.Headline, error) {
	head := h.query.Headline
	headlines, err := head.WithContext(context.Background()).Where(head.ParentID.Eq(parentID)).Where(head.Type.Eq(storage.VirtualHead)).Find()
	if err != nil {
		return nil, logger.Errorf("Get virtual child headline by parent id %s error: %v", parentID, err)
	}
	return headlines, nil
}

func (h *OrgHeadlineDB) IfVirtualHeadExpandable(id string) (int, error) {
	head := h.query.Headline
	count, err := head.WithContext(context.Background()).Where(head.ParentID.Eq(id)).Count()
	if err != nil {
		return 0, logger.Errorf("Get headline by id %s error: %v", id, err)
	}
	if count > 0 {
		return 1, nil
	} else {
		return 0, nil
	}
}
