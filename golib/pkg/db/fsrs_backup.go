package db

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"memo/cmd/options"
	"memo/pkg/logger"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"gorm.io/gorm"
)

// FsrsBackupData FSRS备份数据结构
type FsrsBackupData struct {
	FsrsInfos  []*storage.FsrsInfo  `json:"fsrs_infos"`
	ReviewLogs []*storage.ReviewLog `json:"review_logs"`
	BackupTime time.Time            `json:"backup_time"`
}

// OrgBackupData 组织数据备份结构
type OrgBackupData struct {
	Files      []*storage.File     `json:"files"`
	Headlines  []*storage.Headline `json:"headlines"`
	Properties []*storage.Property `json:"properties"`
	Clocks     []*storage.Clock    `json:"clocks"`
	Tags       []*storage.Tag      `json:"tags"`
	BackupTime time.Time           `json:"backup_time"`
}

// createBackupDir 创建备份目录
func createBackupDir() (string, error) {
	// 获取数据库目录
	con := options.ConfigInit()
	dbPath := con.GetDBPath()

	// 创建备份目录
	backupDir := filepath.Join(dbPath, "backup")
	err := os.MkdirAll(backupDir, os.ModePerm)
	if err != nil {
		return "", logger.Errorf("Create backup directory failed: %v", err)
	}

	return backupDir, nil
}

// generateBackupFileName 生成备份文件名
func generateBackupFileName(prefix string, currentTime time.Time) string {
	return fmt.Sprintf("%s_%s.json", prefix, currentTime.Format("2006-01-02"))
}

// writeBackupFile 将备份数据写入文件
func writeBackupFile(backupFilePath string, data interface{}) error {
	// 序列化为JSON
	jsonData, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return logger.Errorf("Marshal backup data failed: %v", err)
	}

	// 写入文件
	err = ioutil.WriteFile(backupFilePath, jsonData, 0644)
	if err != nil {
		return logger.Errorf("Write backup file failed: %v", err)
	}

	return nil
}

// ExportFsrsData 导出FSRS数据到备份目录
// 导出的文件名格式为：fsrs_backup_YYYY-MM-DD.json
func (f *FsrsDB) ExportFsrsData() (string, error) {
	// 创建备份目录
	backupDir, err := createBackupDir()
	if err != nil {
		return "", err
	}

	// 获取当前日期作为文件名
	currentTime := time.Now()
	backupFileName := generateBackupFileName("fsrs_backup", currentTime)
	backupFilePath := filepath.Join(backupDir, backupFileName)

	// 获取所有FsrsInfo数据
	fsrs := dal.FsrsInfo
	fsrsInfos, err := fsrs.WithContext(context.Background()).Find()
	if err != nil {
		return "", logger.Errorf("Get FsrsInfo data failed: %v", err)
	}

	// 获取所有ReviewLog数据
	reviewLog := dal.ReviewLog
	reviewLogs, err := reviewLog.WithContext(context.Background()).Find()
	if err != nil {
		return "", logger.Errorf("Get ReviewLog data failed: %v", err)
	}

	// 创建备份数据结构
	backupData := FsrsBackupData{
		FsrsInfos:  fsrsInfos,
		ReviewLogs: reviewLogs,
		BackupTime: currentTime,
	}

	// 写入文件
	err = writeBackupFile(backupFilePath, backupData)
	if err != nil {
		return "", err
	}

	return backupFilePath, nil
}

// getBackupFilePath 获取备份文件路径
func getBackupFilePath(prefix string, date string) (string, error) {
	// 获取数据库目录
	con := options.ConfigInit()
	dbPath := con.GetDBPath()
	backupDir := filepath.Join(dbPath, "backup")

	// 检查备份目录是否存在
	if _, err := os.Stat(backupDir); os.IsNotExist(err) {
		return "", logger.Errorf("Backup directory does not exist")
	}

	// 获取备份文件路径
	var backupFilePath string
	if date != "" {
		// 使用指定日期的备份文件
		backupFileName := fmt.Sprintf("%s_%s.json", prefix, date)
		backupFilePath = filepath.Join(backupDir, backupFileName)

		// 检查文件是否存在
		if _, err := os.Stat(backupFilePath); os.IsNotExist(err) {
			return "", logger.Errorf("Backup file for date %s does not exist", date)
		}
	} else {
		// 获取最新的备份文件
		files, err := ioutil.ReadDir(backupDir)
		if err != nil {
			return "", logger.Errorf("Read backup directory failed: %v", err)
		}

		// 过滤出备份文件并排序
		var backupFiles []string
		for _, file := range files {
			if !file.IsDir() && strings.HasPrefix(file.Name(), prefix+"_") && strings.HasSuffix(file.Name(), ".json") {
				backupFiles = append(backupFiles, file.Name())
			}
		}

		if len(backupFiles) == 0 {
			return "", logger.Errorf("No backup files found for prefix %s", prefix)
		}

		// 按文件名排序（日期格式保证了按时间排序）
		sort.Strings(backupFiles)
		latestBackup := backupFiles[len(backupFiles)-1]
		backupFilePath = filepath.Join(backupDir, latestBackup)
	}

	return backupFilePath, nil
}

// readBackupFile 读取备份文件
func readBackupFile(backupFilePath string, data interface{}) error {
	// 读取备份文件
	jsonData, err := ioutil.ReadFile(backupFilePath)
	if err != nil {
		return logger.Errorf("Read backup file failed: %v", err)
	}

	// 解析JSON数据
	err = json.Unmarshal(jsonData, data)
	if err != nil {
		return logger.Errorf("Unmarshal backup data failed: %v", err)
	}

	return nil
}

// beginTransaction 开始数据库事务
func (f *FsrsDB) beginTransaction() (*gorm.DB, error) {
	tx := f.db.Begin()
	if tx.Error != nil {
		return nil, logger.Errorf("Begin transaction failed: %v", tx.Error)
	}
	return tx, nil
}

// commitTransaction 提交事务并清除缓存
func commitTransactionAndInvalidateCache(tx *gorm.DB) error {
	if err := tx.Commit().Error; err != nil {
		return logger.Errorf("Commit transaction failed: %v", err)
	}

	// 清除缓存
	if cacheManager := GetCacheManager(); cacheManager != nil {
		cacheManager.InvalidateAllCache()
	}

	return nil
}

// ImportFsrsData 从备份文件导入FSRS数据
// date参数格式为：YYYY-MM-DD，如果为空则导入最新的备份
func (f *FsrsDB) ImportFsrsData(date string) error {
	// 获取备份文件路径
	backupFilePath, err := getBackupFilePath("fsrs_backup", date)
	if err != nil {
		return err
	}

	// 读取备份文件
	var backupData FsrsBackupData
	err = readBackupFile(backupFilePath, &backupData)
	if err != nil {
		return err
	}

	// 开始事务
	tx, err := f.beginTransaction()
	if err != nil {
		return err
	}

	// 清空现有数据
	fsrs := dal.FsrsInfo
	reviewLog := dal.ReviewLog

	_, err = fsrs.WithContext(context.Background()).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing FsrsInfo data failed: %v", err)
	}

	_, err = reviewLog.WithContext(context.Background()).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing ReviewLog data failed: %v", err)
	}

	// 导入FsrsInfo数据
	for _, info := range backupData.FsrsInfos {
		err = fsrs.WithContext(context.Background()).Create(info)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import FsrsInfo data failed: %v", err)
		}
	}

	// 导入ReviewLog数据
	for _, log := range backupData.ReviewLogs {
		err = reviewLog.WithContext(context.Background()).Create(log)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import ReviewLog data failed: %v", err)
		}
	}

	// 提交事务并清除缓存
	return commitTransactionAndInvalidateCache(tx)
}

// ResetFsrsData 清空重置FSRS数据
func (f *FsrsDB) ResetFsrsData() error {
	// 开始事务
	tx, err := f.beginTransaction()
	if err != nil {
		return err
	}

	// 清空FsrsInfo和ReviewLog表
	fsrs := dal.FsrsInfo
	reviewLog := dal.ReviewLog

	_, err = fsrs.WithContext(context.Background()).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete FsrsInfo data failed: %v", err)
	}

	_, err = reviewLog.WithContext(context.Background()).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete ReviewLog data failed: %v", err)
	}

	// 提交事务并清除缓存
	return commitTransactionAndInvalidateCache(tx)
}

// ExportOrgData 导出组织数据到备份目录
// 导出的文件名格式为：org_backup_YYYY-MM-DD.json
func (f *FsrsDB) ExportOrgData() (string, error) {
	// 创建备份目录
	backupDir, err := createBackupDir()
	if err != nil {
		return "", err
	}

	// 获取当前日期作为文件名
	currentTime := time.Now()
	backupFileName := generateBackupFileName("org_backup", currentTime)
	backupFilePath := filepath.Join(backupDir, backupFileName)

	// 获取所有表数据
	ctx := context.Background()

	// 获取File表数据
	file := dal.File
	files, err := file.WithContext(ctx).Find()
	if err != nil {
		return "", logger.Errorf("Get File data failed: %v", err)
	}

	// 获取Headline表数据
	headline := dal.Headline
	headlines, err := headline.WithContext(ctx).Find()
	if err != nil {
		return "", logger.Errorf("Get Headline data failed: %v", err)
	}

	// 获取Property表数据
	property := dal.Property
	properties, err := property.WithContext(ctx).Find()
	if err != nil {
		return "", logger.Errorf("Get Property data failed: %v", err)
	}

	// 获取Clock表数据
	clock := dal.Clock
	clocks, err := clock.WithContext(ctx).Find()
	if err != nil {
		return "", logger.Errorf("Get Clock data failed: %v", err)
	}

	// 获取Tag表数据
	tag := dal.Tag
	tags, err := tag.WithContext(ctx).Find()
	if err != nil {
		return "", logger.Errorf("Get Tag data failed: %v", err)
	}

	// 创建备份数据结构
	backupData := OrgBackupData{
		Files:      files,
		Headlines:  headlines,
		Properties: properties,
		Clocks:     clocks,
		Tags:       tags,
		BackupTime: currentTime,
	}

	// 写入文件
	err = writeBackupFile(backupFilePath, backupData)
	if err != nil {
		return "", err
	}

	return backupFilePath, nil
}

// ImportOrgData 从备份文件导入组织数据
// date参数格式为：YYYY-MM-DD，如果为空则导入最新的备份
func (f *FsrsDB) ImportOrgData(date string) error {
	// 获取备份文件路径
	backupFilePath, err := getBackupFilePath("org_backup", date)
	if err != nil {
		return err
	}

	// 读取备份文件
	var backupData OrgBackupData
	err = readBackupFile(backupFilePath, &backupData)
	if err != nil {
		return err
	}

	// 开始事务
	tx, err := f.beginTransaction()
	if err != nil {
		return err
	}

	ctx := context.Background()

	// 清空现有数据
	file := dal.File
	headline := dal.Headline
	property := dal.Property
	clock := dal.Clock
	tag := dal.Tag

	// 删除数据时需要考虑外键约束，按照依赖关系顺序删除
	_, err = tag.WithContext(ctx).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing Tag data failed: %v", err)
	}

	_, err = property.WithContext(ctx).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing Property data failed: %v", err)
	}

	_, err = clock.WithContext(ctx).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing Clock data failed: %v", err)
	}

	_, err = headline.WithContext(ctx).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing Headline data failed: %v", err)
	}
	_, err = file.WithContext(ctx).Unscoped().Delete()
	if err != nil {
		tx.Rollback()
		return logger.Errorf("Delete existing File data failed: %v", err)
	}
	// 导入数据
	for _, fileData := range backupData.Files {
		err = file.WithContext(ctx).Create(fileData)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import File data failed: %v", err)
		}
	}
	for _, headlineData := range backupData.Headlines {
		err = headline.WithContext(ctx).Create(headlineData)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import Headline data failed: %v", err)
		}
	}
	for _, propertyData := range backupData.Properties {
		err = property.WithContext(ctx).Create(propertyData)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import Property data failed: %v", err)
		}
	}
	for _, clockData := range backupData.Clocks {
		err = clock.WithContext(ctx).Create(clockData)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import Clock data failed: %v", err)
		}
	}
	for _, tagData := range backupData.Tags {
		err = tag.WithContext(ctx).Create(tagData)
		if err != nil {
			tx.Rollback()
			return logger.Errorf("Import Tag data failed: %v", err)
		}
	}

	// 提交事务并清除缓存
	return commitTransactionAndInvalidateCache(tx)
}
