package org

import (
	"errors"
	"memo/pkg/db"
	"strings"
	"sync"

	"memo/pkg/card"
	"memo/pkg/client"
	"memo/pkg/logger"
	"memo/pkg/org/parser"
	"memo/pkg/storage"

	"github.com/karrick/godirwalk"
	epc "github.com/kiwanami/go-elrpc"
	"gorm.io/gorm"
)

var muteFile = sync.Mutex{}

type OrgApi struct {
	db           *gorm.DB
	annotationDB *db.AnnotationDB
}

func NewOrgApi() (*OrgApi, error) {
	DB, err := storage.InitDBEngine()
	if err != nil {
		return nil, err
	}

	annotationDB, err := db.NewAnnotationDB()
	if err != nil {
		return nil, err
	}

	return &OrgApi{db: DB, annotationDB: annotationDB}, nil
}

func (o *OrgApi) RegistryEpcMethod(service *epc.ServerService) *epc.ServerService {
	service.RegisterMethod(epc.MakeMethod("UploadFile", o.UploadFile, "string", "Upload org file to database"))
	service.RegisterMethod(epc.MakeMethod("SyncOrgDir", o.UploadFilesUnderDir, "string", "Upload org files under dir to database"))
	service.RegisterMethod(epc.MakeMethod("UpdateOrgHeadContent", o.UpdateOrgHeadContent, "string", "Update org head content"))
	service.RegisterMethod(epc.MakeMethod("UpdateOrgHeadProperty", o.UpdateOrgHeadProperty, "string", "Update org head property"))
	service.RegisterMethod(epc.MakeMethod("GetOrgHeadProperty", o.GetOrgHeadProperty, "string", "Get org head property by id."))
	service.RegisterMethod(epc.MakeMethod("ExportOrgFileToDisk", o.ExportOrgFileToDisk, "string", "Export org file to disk"))
	service.RegisterMethod(epc.MakeMethod("CreateVirtHead", o.CreateVirtHead, "string", "Create virt head."))
	service.RegisterMethod(epc.MakeMethod("GetVirtFile", o.GetVirtfileContent, "string", "Get virt file content."))
	service.RegisterMethod(epc.MakeMethod("UploadVirtFile", o.UploadVirtFile, "string", "upload virt file content."))
	service.RegisterMethod(epc.MakeMethod("GetChildVirtPropertyMap", o.GetChildrenVirtPropertyMap, "string", "Get child virt head property."))
	// Annotation API methods
	service.RegisterMethod(epc.MakeMethod("CreateAnnotation", o.CreateAnnotation, "string", "Create a new annotation"))
	service.RegisterMethod(epc.MakeMethod("GetAnnotationByID", o.GetAnnotationByID, "string", "Get annotation by ID"))
	service.RegisterMethod(epc.MakeMethod("GetAnnotationsByHeadlineID", o.GetAnnotationsByHeadlineID, "string", "Get all annotations for a headline"))
	service.RegisterMethod(epc.MakeMethod("UpdateAnnotation", o.UpdateAnnotation, "string", "Update an existing annotation"))
	service.RegisterMethod(epc.MakeMethod("DeleteAnnotationByID", o.DeleteAnnotationByID, "string", "Delete annotation by ID"))
	return service
}

func (o *OrgApi) GetChildrenVirtPropertyMap(headid, key string) db.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	data := headdb.GetVirtPropertyMap(headid, key)
	return db.Result{Data: data, Err: nil}
}

func (o *OrgApi) CreateVirtHead(headid, title, content string) db.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: "", Err: err}
	}
	content = strings.ReplaceAll(content, "\\\\", "\\")
	content = strings.ReplaceAll(content, "\\\"", "\"")
	content += "\n"
	id, err := headdb.CreateVirtHead(headid, title, content)
	if err != nil {
		return db.Result{Data: "", Err: err}
	}
	card.ScanInitFsrs()
	return db.Result{Data: id, Err: nil}
}

func (o *OrgApi) GetVirtfileContent(headid string) db.Result {
	file, err := GetFileFromID(headid, storage.VirtualFile)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	content := file.String()
	return db.Result{Data: content, Err: nil}
}

func (o *OrgApi) UploadVirtFile(headid, content string, fo int) db.Result {
	var force bool
	if fo == 0 {
		force = false
	} else {
		force = true
	}
	f, err := NewVirtFileFromHeadID(headid)
	if err != nil {
		return db.Result{Data: false, Err: err}
	} else if f == nil {
		return db.Result{Data: false, Err: err}
	}

	content = strings.ReplaceAll(content, "\\\\", "\\")
	content = strings.ReplaceAll(content, "\\\"", "\"")
	err = f.LoadFromContent(content)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	err = f.SaveDB(force)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	return db.Result{Data: true, Err: nil}
}

// UploadFile first load hash and filePath from file in disk，
// Then get nodes and fileId, first try to get from kv cache, second to parse file.
// force is not 0 mean alway update file even hash is same.
// batchMode is 1 mean not scan head for fsrs init after upload.
func (o *OrgApi) UploadFile(filePath string, fo int, batchMode int) db.Result {
	var force bool
	if fo == 0 {
		force = false
	} else {
		force = true
	}

	f, err := NewFileFromPath(filePath)
	if err != nil {
		return db.Result{Data: false, Err: err}
	} else if f == nil {
		return db.Result{Data: false, Err: err}
	}
	err = f.LoadFromFile(force)
	if err != nil && !errors.Is(err, parser.MissFileID) {
		return db.Result{Data: false, Err: err}
	} else if err != nil && errors.Is(err, parser.MissFileID) {
		logger.Warnf("Miss file id in file %s.", filePath)
		return db.Result{Data: false, Err: nil}
	}
	err = f.SaveDB(force)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}

	if batchMode == 0 {
		card.ScanInitFsrs()
	}

	if err != nil {
		return db.Result{Data: false, Err: err}
	} else {
		return db.Result{Data: true, Err: nil}
	}
}

func (o *OrgApi) UploadFilesUnderDir(dirPath string, needForce int) db.Result {
	err := godirwalk.Walk(dirPath, &godirwalk.Options{
		Callback: func(osPathname string, de *godirwalk.Dirent) error {
			// Following string operation is not most performant way
			// of doing this, but common enough to warrant a simple
			// example here:
			if strings.Contains(osPathname, ".org") && !de.IsDir() {
				result := o.UploadFile(osPathname, needForce, 1)
				if errors.Is(result.Err, parser.MissFileID) {
					return nil
				} else if errors.Is(result.Err, parser.FoundDupID) {
					return nil
				} else if result.Err != nil {
					return logger.Errorf("Upload file %s failed: %v", osPathname, result.Err)
				}
				return godirwalk.SkipThis
			}
			return nil
		},
		Unsorted: true,
	})
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	card.ScanInitFsrs()
	return db.Result{Data: true, Err: nil}
}

func (o *OrgApi) ExportOrgFileToDisk(fileid string, filePath string) error {
	file, err := GetFileFromID(fileid, storage.NormalFile)
	if err != nil {
		return err
	}
	err = file.SaveToDiskFile(filePath)
	if err != nil {
		return err
	}
	if filePath == "" {
		client.EClient.EvalInEmacs("memo-reload-org", file.path)
	} else {
		client.EClient.EvalInEmacs("memo-reload-org", filePath)
	}
	return nil
}

func (o *OrgApi) exportOrgFileToDiskByOrgID(orgid string, filePath string) error {
	headdb, err := db.NewOrgHeadlineDB()
	fileID, err := headdb.GetFileIDByOrgID(orgid)
	if err != nil {
		return err
	}
	if fileID != nil {
		err = o.ExportOrgFileToDisk(*fileID, filePath)
		if err != nil {
			return err
		}
	}
	return nil
}

func (o *OrgApi) UpdateOrgHeadContent(orgid, bodyContent string) db.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	bodyContent = strings.ReplaceAll(bodyContent, "\\\\", "\\")
	bodyContent = strings.ReplaceAll(bodyContent, "\\\"", "\"")
	err = headdb.UpdateHeadlineContent(orgid, bodyContent)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	go func() {
		muteFile.Lock()
		defer muteFile.Unlock()
		o.exportOrgFileToDiskByOrgID(orgid, "")
	}()

	go func() {
		headdb.UpdateHeadlineHash(orgid)
	}()
	return db.Result{Data: true, Err: nil}
}

func (o *OrgApi) UpdateOrgHeadProperty(orgid, key, value string) db.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	err = headdb.UpdateProperty(orgid, key, value)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}
	go func() {
		muteFile.Lock()
		defer muteFile.Unlock()
		o.exportOrgFileToDiskByOrgID(orgid, "")
	}()

	go func() {
		headdb.UpdateHeadlineHash(orgid)
	}()
	return db.Result{Data: true, Err: nil}
}

func (o *OrgApi) GetOrgHeadProperty(orgid, key string) db.Result {
	headdb, err := db.NewOrgHeadlineDB()
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}
	value := headdb.GetPropertyValue(orgid, key)
	return db.Result{Data: value, Err: nil}
}

// CreateAnnotation creates a new annotation for a headline
func (o *OrgApi) CreateAnnotation(headlineID string, startPos, endPos uint, annoText, commentText, face string) db.Result {
	annotation := &storage.Annotation{
		HeadlineID:  headlineID,
		Start:       startPos,
		End:         endPos,
		AnnoText:    annoText,
		CommentText: commentText,
		Face:        face,
	}

	anno, err := o.annotationDB.CreateAnnotation(annotation)
	if err != nil {
		return db.Result{Data: nil, Err: logger.Errorf("Failed to create annotation: %v", err)}
	}

	return db.Result{Data: anno, Err: nil}
}

// GetAnnotationByID retrieves an annotation by its ID
func (o *OrgApi) GetAnnotationByID(annotationID uint) db.Result {
	annotation, err := o.annotationDB.GetAnnotationByID(annotationID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	return db.Result{Data: annotation, Err: nil}
}

// GetAnnotationsByHeadlineID retrieves all annotations for a headline
func (o *OrgApi) GetAnnotationsByHeadlineID(headlineID string) db.Result {
	annotations, err := o.annotationDB.GetAnnotationsByHeadlineID(headlineID)
	if err != nil {
		return db.Result{Data: nil, Err: err}
	}

	return db.Result{Data: annotations, Err: nil}
}

// UpdateAnnotation updates an existing annotation
func (o *OrgApi) UpdateAnnotation(annotationID uint, startPos, endPos uint, headid, face, annoText, commentText string) db.Result {
	// First get the existing annotation to ensure it exists
	existingAnnotation, err := o.annotationDB.GetAnnotationByID(annotationID)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}

	// Update the annotation fields
	existingAnnotation.Start = startPos
	existingAnnotation.End = endPos
	existingAnnotation.HeadlineID = headid
	existingAnnotation.Face = face
	existingAnnotation.AnnoText = annoText
	existingAnnotation.CommentText = commentText

	// Save the updated annotation
	err = o.annotationDB.UpdateAnnotation(existingAnnotation)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}

	return db.Result{Data: true, Err: nil}
}

// DeleteAnnotationByID deletes an annotation by its ID
func (o *OrgApi) DeleteAnnotationByID(annotationID uint) db.Result {
	err := o.annotationDB.DeleteAnnotationByID(annotationID)
	if err != nil {
		return db.Result{Data: false, Err: err}
	}

	return db.Result{Data: true, Err: nil}
}
