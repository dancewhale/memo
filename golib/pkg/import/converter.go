package _import

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"memo/pkg/logger"
)

// SanitizeFilename cleans up unsafe characters in filenames.
func SanitizeFilename(filename string) string {
	// Replace Windows/Unix unsupported characters
	// Invalid characters regex: r'[<>:"/\\|?*\[\]]'
	// Go's regex doesn't need to escape backslashes as much in raw strings, but for general strings, they are needed.
	re := regexp.MustCompile(`[<>:"/\\|?*\[\]]`) // Adjusted for Go: removed unnecessary Go escapes for \, added for "
	filename = re.ReplaceAllString(filename, "_")
	// Handle consecutive underscores
	re = regexp.MustCompile(`_+`)
	filename = re.ReplaceAllString(filename, "_")
	return strings.Trim(filename, "_")
}

// ConvertFileToOrgMode converts the input file to Org mode format.
// It automatically detects the file type based on its extension.
func ConvertFileToOrgMode(fileid, inputPath string, outputFilePath string, mediaOutPutPath string) (string, error) {
	// Check for pandoc dependency
	if _, err := exec.LookPath("pandoc"); err != nil {
		return "", logger.Errorf("pandoc command not found, please ensure it's installed and in PATH: %v", err)
	}

	fileExt := strings.ToLower(filepath.Ext(inputPath))

	switch fileExt {
	case ".epub":
		return outputFilePath, convertWithPandoc(fileid, inputPath, outputFilePath, "epub", mediaOutPutPath)
	case ".md", ".markdown":
		return outputFilePath, convertWithPandoc(fileid, inputPath, outputFilePath, "markdown", mediaOutPutPath)
	case ".html", ".htm":
		return outputFilePath, convertWithPandoc(fileid, inputPath, outputFilePath, "html", mediaOutPutPath)
	case ".pdf":
		// Check for pdftotext dependency before calling convertPDFToOrg
		if _, err := exec.LookPath("pdftotext"); err != nil {
			return "", logger.Errorf("pdftotext command not found, please ensure it's installed and in PATH (usually part of poppler-utils), %v", err)
		}
		return outputFilePath, convertPDFToOrg(inputPath, outputFilePath)
	default:
		return "", fmt.Errorf("unsupported file type: %s", fileExt)
	}
}

func convertWithPandoc(fileid, inputFile, outputFile, fromFormat, mediaOutPutDir string) error {
	// Ensure mediaOutPutDir is absolute for consistent imageDir path construction
	absOutputDir, err := filepath.Abs(mediaOutPutDir)
	if err != nil {
		return logger.Errorf("failed to get absolute path for media output directory: %s,  %v", mediaOutPutDir, err)
	}

	imageDir := filepath.Join(absOutputDir, fileid) // This will be an absolute path

	err = os.MkdirAll(imageDir, 0755)
	if err != nil {
		return logger.Errorf("failed to create image directory: %s, %v", imageDir, err)
	}

	cmd := exec.Command("pandoc",
		"--wrap=none",
		"--standalone",
		"-t", "org",
		"--no-highlight",
		fmt.Sprintf("--extract-media=%s", imageDir), // Pandoc receives an absolute path here
		"-f", fromFormat,
		inputFile,
		"-o", outputFile)

	output, err := cmd.CombinedOutput()
	if err != nil {
		return logger.Errorf("pandoc conversion failed for %s: %s, %v", inputFile, string(output), err)
	}

	// Post-process to update image links to be relative to the org file, within the imageDirName subdirectory
	content, err := os.ReadFile(outputFile)
	if err != nil {
		return logger.Errorf("failed to read converted file: %s, %v", outputFile, err)
	}

	// Go equivalent: `\[\[file(.*?)\]\]`
	// Construct the regex pattern string based on mediaOutPutDir
	// regexp.QuoteMeta escapes any special regex characters in mediaOutPutDir
	patternString := fmt.Sprintf(`(\[\[%s)(.*?)(]])`, regexp.QuoteMeta(mediaOutPutDir))
	linkRegex := regexp.MustCompile(patternString)

	newContent := linkRegex.ReplaceAllStringFunc(string(content), func(match string) string {
		submatches := linkRegex.FindStringSubmatch(match)
		if len(submatches) < 4 { // 0: full, 1: prefix, 2: path, 3: suffix
			return match // Should not happen with this regex
		}
		// We want the link to be relative to the .org file, pointing into the _images directory
		// e.g., [[file:../index/image.png]]
		newImagePath := "[[file:../media" + submatches[2] + submatches[3]
		return newImagePath
	})

	err = os.WriteFile(outputFile, []byte(newContent), 0644)
	if err != nil {
		return logger.Errorf("failed to write updated content to file: %s, %v", outputFile, err)
	}

	// Check if the imageDir is empty and remove it if so.
	// Log errors from this operation but don't let them fail the overall conversion.
	if cleanupErr := removeDirIfEmpty(imageDir); cleanupErr != nil {
		return logger.Errorf("Failed to cleanup media directory %s: %v. Continuing as main conversion was successful.", imageDir, cleanupErr)
	}

	return nil
}

func convertPDFToOrg(inputFile, outputFile string) error {
	// PDF conversion using pdftotext.
	// First attempt: pdftotext inputFile -
	cmd := exec.Command("pdftotext", inputFile, "-") // "-" outputs to stdout
	pdfContentBytes, err := cmd.Output()             // Use Output to get stdout, CombinedOutput for stdout+stderr

	if err != nil {
		// If first attempt fails, try with -layout option
		// Log the first error, or include it in the final error message
		// originalErr := err // Keep original error for context if needed
		cmdLayout := exec.Command("pdftotext", "-layout", inputFile, "-")
		pdfContentBytesLayout, errLayout := cmdLayout.Output()
		if errLayout != nil {
			// Capture stderr for better error reporting
			var stderrLayout []byte
			if ee, ok := errLayout.(*exec.ExitError); ok {
				stderrLayout = ee.Stderr
			}
			var stderrOriginal []byte
			if ee, ok := err.(*exec.ExitError); ok {
				stderrOriginal = ee.Stderr
			}
			return logger.Errorf("pdftotext conversion failed for %s (with -layout option, stderr: %s). Original pdftotext error (stderr: %s)", inputFile, string(stderrLayout), string(stderrOriginal))
		}
		pdfContentBytes = pdfContentBytesLayout // Use layout output if successful
	}

	err = os.WriteFile(outputFile, pdfContentBytes, 0644)
	if err != nil {
		return logger.Errorf("failed to write pdftotext output to file: %s, %v", outputFile, err)
	}

	return nil
}
