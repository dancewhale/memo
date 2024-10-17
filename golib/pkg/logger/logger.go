package logger

import (
	"fmt"
	"os"
	"sync"

	"memo/cmd/libmemo/options"

	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

var once sync.Once
var logger *zap.Logger

func Init() {
	once.Do(NewLogger)
}

func NewLogger() {
	var allCore []zapcore.Core

	stdCore := getStdCore()
	allCore = append(allCore, stdCore)

	core := zapcore.NewTee(allCore...)
	logger = zap.New(core,
		zap.AddCaller(),
		// 输出上一层的信息
		zap.AddCallerSkip(1),
		// Fatal级别输出堆栈信息
		zap.AddStacktrace(zapcore.FatalLevel),
	)
	defer logger.Sync()
}

func getStdCore() zapcore.Core {
	var config options.Config
	c := config.ConfigInit()
	stdEncoder := getEncoder()
	consoleWriter := zapcore.Lock(os.Stdout)
	logList := [7]int64{-1, 0, 1, 2, 3, 4, 5}
	for _, logLevel := range logList {
		if logLevel == int64(c.LogLevel) {
			LogLevel := zapcore.Level(c.LogLevel)
			return zapcore.NewCore(stdEncoder, consoleWriter, LogLevel)
		}
	}
	LogLevel := zapcore.InfoLevel
	return zapcore.NewCore(stdEncoder, consoleWriter, LogLevel)
}

func getEncoder() zapcore.Encoder {
	encoderConfig := zap.NewProductionEncoderConfig()
	encoderConfig.EncodeTime = zapcore.ISO8601TimeEncoder
	// Level大写， 比如info为INFO
	encoderConfig.EncodeLevel = zapcore.CapitalLevelEncoder
	// 文件路径显示绝对路径
	encoderConfig.EncodeCaller = zapcore.FullCallerEncoder
	return zapcore.NewConsoleEncoder(encoderConfig)
}

func Info(msg string) {
	logger.Info(msg)
}

func Infof(msg string, a ...any) {
	logger.Info(fmt.Sprintf(msg, a...))
}

func Error(msg string) {
	logger.Error(msg)
}

func Errorf(msg string, a ...any) {
	logger.Error(fmt.Sprintf(msg, a...))
}

func Fatal(msg string) {
	logger.Fatal(msg)
}

func Fatalf(msg string, a ...any) {
	logger.Fatal(fmt.Sprintf(msg, a...))
}

func Warn(msg string) {
	logger.Warn(msg)
}

func Warnf(msg string, a ...any) {
	logger.Warn(fmt.Sprintf(msg, a...))
}

func Debug(msg string) {
	logger.Debug(msg)
}

func Debugf(msg string, a ...any) {
	logger.Debug(fmt.Sprintf(msg, a...))
}
