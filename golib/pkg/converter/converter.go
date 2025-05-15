package converter

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
	re := regexp.MustCompile(`[<>:"/\|?*\[\]]`) // Adjusted for Go: removed unnecessary Go escapes for \, added for "
	filename = re.ReplaceAllString(filename, "_")
	// Handle consecutive underscores
	re = regexp.MustCompile(`_+`)
	filename = re.ReplaceAllString(filename, "_")
	return strings.Trim(filename, "_")
}

// ConvertFileToOrgMode converts the input file to Org mode format.
// It automatically detects the file type based on its extension.
func ConvertFileToOrgMode(inputPath string, outputDir string) (string, error) {
	// Check for pandoc dependency
	if _, err := exec.LookPath("pandoc"); err != nil {
		return "", logger.Errorf("pandoc command not found, please ensure it's installed and in PATH: %v", err)
	}

	fileExt := strings.ToLower(filepath.Ext(inputPath))
	baseName := strings.TrimSuffix(filepath.Base(inputPath), fileExt)
	safeBaseName := SanitizeFilename(baseName)
	outputFileName := fmt.Sprintf("%s.org", safeBaseName)
	outputFilePath := filepath.Join(outputDir, outputFileName)

	err := os.MkdirAll(outputDir, 0755)
	if err != nil {
		return "", logger.Errorf("failed to create output directory: %s, %v", outputDir, err)
	}

	switch fileExt {
	case ".epub":
		return outputFilePath, convertWithPandoc(inputPath, outputFilePath, "epub", outputDir)
	case ".md", ".markdown":
		return outputFilePath, convertWithPandoc(inputPath, outputFilePath, "markdown", outputDir)
	case ".html", ".htm":
		return outputFilePath, convertWithPandoc(inputPath, outputFilePath, "html", outputDir)
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

func convertWithPandoc(inputFile, outputFile, fromFormat, outputDir string) error {
	// Ensure outputDir is absolute for consistent imageDir path construction
	absOutputDir, err := filepath.Abs(outputDir)
	if err != nil {
		return logger.Errorf("failed to get absolute path for output directory: %s,  %v", outputDir, err)
	}
	absOutputFile, err := filepath.Abs(outputFile)
	if err != nil {
		return logger.Errorf("failed to get absolute path for output file: %s, %v", outputFile, err)
	}

	imageDirName := fmt.Sprintf("%s_images", strings.TrimSuffix(filepath.Base(absOutputFile), filepath.Ext(absOutputFile)))
	imageDir := filepath.Join(absOutputDir, imageDirName) // This will be an absolute path

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

	// Regex to find [[file:ANYTHING]] links
	// The Python script's regex: r'\[\[file:(.*?)\]\]'
	// Go equivalent: `\[\[file:(.*?)\]\]`
	linkRegex := regexp.MustCompile(`(\[\[file:)(.*?)(]])`)

	newContent := linkRegex.ReplaceAllStringFunc(string(content), func(match string) string {
		submatches := linkRegex.FindStringSubmatch(match)
		if len(submatches) < 4 { // 0: full, 1: prefix, 2: path, 3: suffix
			return match // Should not happen with this regex
		}
		originalImagePathInLink := submatches[2]
		// We want the link to be relative to the .org file, pointing into the _images directory
		// e.g., [[file:mybook_images/image.png]]
		newImagePath := filepath.Join(imageDirName, filepath.Base(originalImagePathInLink))
		// Ensure forward slashes for org mode links, even on Windows
		newImagePath = filepath.ToSlash(newImagePath)
		return fmt.Sprintf("%s%s%s", submatches[1], newImagePath, submatches[3])
	})

	err = os.WriteFile(outputFile, []byte(newContent), 0644)
	if err != nil {
		return logger.Errorf("failed to write updated content to file: %s, %v", outputFile, err)
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
