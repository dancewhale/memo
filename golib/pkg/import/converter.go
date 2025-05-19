package _import

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"

	"memo/pkg/logger"
)

// ConvertFileToOrgMode converts the input file to Org mode format.
// It automatically detects the file type based on its extension.
func (f *File) ConvertFileToTempOrgMode() error {
	// Check for pandoc dependency
	if _, err := exec.LookPath("pandoc"); err != nil {
		return logger.Errorf("pandoc command not found, please ensure it's installed and in PATH: %v", err)
	}

	switch f.fileExt {
	case ".epub":
		return f.convertWithPandoc("epub")
	case ".mobi":
		return f.convertWithPandoc("mobi")
	case ".md", ".markdown":
		return f.convertWithPandoc("markdown")
	case ".html", ".htm":
		return f.convertWithPandoc("html")
	case ".pdf":
		// Check for pdftotext dependency before calling convertPDFToOrg
		if _, err := exec.LookPath("pdftotext"); err != nil {
			return logger.Errorf("pdftotext command not found, please ensure it's installed and in PATH (usually part of poppler-utils), %v", err)
		}
		return f.convertPDFToOrg()
	default:
		return fmt.Errorf("unsupported file type: %s", f.fileExt)
	}
}

// ensureMediaDirectory creates the media directory for a given file and returns its absolute path.
func (f *File) ensureMediaTempDirectory() (string, error) {
	// f.getTempMediaRootDir() returns a path like /path/to/temp/media (it's based on f.temp which is a root dir)
	absMediaRoot, err := filepath.Abs(f.getTempMediaRootDir())
	if err != nil {
		return "", logger.Errorf("failed to get absolute path for media root directory: %s, %v", f.getTempMediaRootDir(), err)
	}

	return filepath.Join(absMediaRoot, "media"), nil // If ID is empty, return the media root path
}

// executePandocCommand runs the pandoc command to convert the source file.
func (f *File) executePandocCommand(fromFormat string, imageDir string) error {
	cmd := exec.Command("pandoc",
		"--wrap=none",
		"--standalone",
		"-t", "org",
		"--no-highlight",
		fmt.Sprintf("--extract-media=%s", imageDir), // imageDir is absolute path like /path/to/temp/media/generatedID
		"-f", fromFormat,
		f.srcFilePath,
		"-o", f.tempFilePath)

	output, err := cmd.CombinedOutput()
	if err != nil {
		return logger.Errorf("pandoc conversion failed for %s: %s, %v", f.srcFilePath, string(output), err)
	}
	return nil
}

// postProcessOrgFileLinks updates image links in the converted org file to be relative.
// Pandoc generates links like [[file:/path/to/media/ID/image.png]].
// These need to be converted to [[file:../media/ID/image.png]] relative to the org file.
func (f *File) postProcessOrgFileLinks() error {
	content, err := os.ReadFile(f.tempFilePath)
	if err != nil {
		return logger.Errorf("failed to read converted file: %s, %v", f.tempFilePath, err)
	}

	// absMediaRoot is the absolute path to the general media directory (e.g., /tmp/orgmemo/media)
	absMediaRoot, err := filepath.Abs(f.getTempMediaRootDir())
	if err != nil {
		return logger.Errorf("failed to get absolute path for media root directory in postProcess: %s, %v", f.getTempMediaRootDir(), err)
	}
	// pandocLinkedImageDir is the specific directory pandoc used for this file's media (e.g., /tmp/orgmemo/media/FILE_ID)
	pandocLinkedImageDir := filepath.Join(absMediaRoot, f.ID)

	// Pattern matches [[file:/path/to/media/FILE_ID/image.png]]
	// Group 1: [[file:
	// Group 2: /path/to/media/FILE_ID (path to specific image dir)
	// Group 3: /image.png (actual image path relative to image dir)
	// Group 4: ]]
	patternString := fmt.Sprintf(`(\[\[file:)(%s)(/.*?)(]])`, regexp.QuoteMeta(pandocLinkedImageDir))
	linkRegex := regexp.MustCompile(patternString)

	newContent := linkRegex.ReplaceAllStringFunc(string(content), func(match string) string {
		submatches := linkRegex.FindStringSubmatch(match)
		if len(submatches) < 5 { // 0:full, 1:prefix, 2:pandocLinkedDir, 3:imgPath, 4:suffix
			logger.Warnf("Unexpected link format, not modifying: %s", match) // Log if format is unexpected
			return match
		}
		// Desired: [[file:../media/FILE_ID/image.png]]
		// submatches[1] = "[[file:"
		// MediaDir = "media" (constant)
		// f.ID = "FILE_ID"
		// submatches[3] = "/image.png"
		// submatches[4] = "]]"
		newImagePath := fmt.Sprintf("%s../%s/%s%s%s", submatches[1], MediaDir, f.ID, submatches[3], submatches[4])
		return newImagePath
	})

	err = os.WriteFile(f.tempFilePath, []byte(newContent), 0644)
	if err != nil {
		return logger.Errorf("failed to write updated content to file: %s, %v", f.tempFilePath, err)
	}
	return nil
}

// cleanupMediaDirectory removes the specific media directory for the file if it's empty.
// imageDir is the absolute path to the directory like /tmp/orgmemo/media/generatedID
func (f *File) cleanupMediaDirectory(imageDir string) {
	if cleanupErr := removeDirIfEmpty(imageDir); cleanupErr != nil {
		logger.Warnf("Failed to cleanup media directory %s: %v. Conversion itself was successful.", imageDir, cleanupErr)
	}
}

func (f *File) convertWithPandoc(fromFormat string) error {
	imageDir, err := f.ensureMediaTempDirectory()
	if err != nil {
		return err // Error already logged by ensureMediaTempDirectory
	}

	err = f.executePandocCommand(fromFormat, imageDir)
	if err != nil {
		f.cleanupMediaDirectory(imageDir) // Attempt cleanup even if pandoc fails
		return err                        // Error already logged by executePandocCommand
	}

	err = f.postProcessOrgFileLinks()
	if err != nil {
		f.cleanupMediaDirectory(imageDir) // Attempt cleanup if post-processing fails
		return err                        // Error already logged by postProcessOrgFileLinks
	}

	f.cleanupMediaDirectory(imageDir) // Final cleanup attempt
	return nil
}

// executePdfToTextCommand runs pdftotext command and returns its output.
// It tries with -layout option if the initial attempt fails.
func executePdfToTextCommand(filePath string) ([]byte, error) {
	cmd := exec.Command("pdftotext", filePath, "-") // "-" outputs to stdout
	pdfContentBytes, err := cmd.Output()

	if err != nil {
		// If first attempt fails, try with -layout option
		cmdLayout := exec.Command("pdftotext", "-layout", filePath, "-")
		pdfContentBytesLayout, errLayout := cmdLayout.Output()
		if errLayout != nil {
			var stderrLayout []byte
			if ee, ok := errLayout.(*exec.ExitError); ok {
				stderrLayout = ee.Stderr
			}
			var stderrOriginal []byte
			if ee, ok := err.(*exec.ExitError); ok {
				stderrOriginal = ee.Stderr
			}
			return nil, fmt.Errorf("pdftotext conversion failed (with -layout option, stderr: %s). Original pdftotext error (stderr: %s)", string(stderrLayout), string(stderrOriginal))
		}
		return pdfContentBytesLayout, nil // Use layout output if successful
	}
	return pdfContentBytes, nil
}

func (f *File) convertPDFToOrg() error {
	pdfContentBytes, err := executePdfToTextCommand(f.srcFilePath)
	if err != nil {
		return logger.Errorf("Failed to extract text from PDF %s: %v", f.srcFilePath, err)
	}

	err = os.WriteFile(f.tempFilePath, pdfContentBytes, 0644)
	if err != nil {
		return logger.Errorf("failed to write pdftotext output to file: %s, %v", f.tempFilePath, err)
	}

	return nil
}
